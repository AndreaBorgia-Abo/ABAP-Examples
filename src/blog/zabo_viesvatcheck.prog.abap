*&---------------------------------------------------------------------*
*& Report ZABO_VIESVATCHECK
*&---------------------------------------------------------------------*
*& Author: Andrea Borgia, based on an example by a former forum member
*& (see: https://blogs.sap.com/2017/05/01/automatic-vat-validation-for-eu/)
*& Specs: https://ec.europa.eu/taxation_customs/vies/technicalInformation.html
*& Debug tips: https://blogs.sap.com/2013/10/17/tracing-serialization-deserialization-issues-in-sap-web-services/
*&---------------------------------------------------------------------*
*& Important: the specs page lists the "test" WSDL only, the link to the
*& real service is in the FAQs. Just create the two ports below, that's all.
*&---------------------------------------------------------------------*
REPORT zabo_viesvatcheck.


CONSTANTS:
  vies_proxy_real TYPE prx_logical_port_name VALUE 'ZVIESVATCHECK',
  vies_proxy_test TYPE prx_logical_port_name VALUE 'ZVIESVATTESTCHECK'.


TYPES: BEGIN OF ty_vat_response,
         country_code TYPE string,
         vat_number   TYPE string,
         request_date TYPE xsddate_iso,
         valid        TYPE xsdboolean,
         name         TYPE string,
         address      TYPE string,
       END OF ty_vat_response.


DATA:
  vies_proxy      TYPE prx_logical_port_name,
  vies_proxy_err  TYPE string,
  vies_proxy_awol TYPE string,
  vat_checks      TYPE STANDARD TABLE OF zviescheck_vat_request WITH EMPTY KEY,
  vat_replies     TYPE STANDARD TABLE OF ty_vat_response WITH EMPTY KEY,
  gr_table        TYPE REF TO cl_salv_table.



SELECTION-SCREEN BEGIN OF BLOCK sele.
PARAMETERS: p_test AS CHECKBOX DEFAULT abap_true,
            p_land LIKE t002-laiso DEFAULT `IT` OBLIGATORY,
            p_vat  TYPE string DEFAULT `02415190400` OBLIGATORY.
SELECTION-SCREEN END OF BLOCK sele.


START-OF-SELECTION.
  REFRESH: vat_checks, vat_replies.
  CLEAR: vies_proxy_err, vies_proxy_awol.

  zcl_aab=>break_point( 'ZABO_DEMO' ).

  IF p_test = abap_true.
    vies_proxy = vies_proxy_test.

    vat_checks = VALUE #(
      ( country_code = p_land  vat_number = `100` ) "Valid request with Valid VAT Number
      ( country_code = p_land  vat_number = `200` ) "Valid request with an Invalid VAT Number
      ( country_code = p_land  vat_number = `201` ) "Error : INVALID_INPUT
      ( country_code = p_land  vat_number = `202` ) "Error : INVALID_REQUESTER_INFO
      ( country_code = p_land  vat_number = `300` ) "Error : SERVICE_UNAVAILABLE
      ( country_code = p_land  vat_number = `301` ) "Error : MS_UNAVAILABLE
      ( country_code = p_land  vat_number = `302` ) "Error : TIMEOUT
      ( country_code = p_land  vat_number = `400` ) "Error : VAT_BLOCKED
      ( country_code = p_land  vat_number = `401` ) "Error : IP_BLOCKED
      ( country_code = p_land  vat_number = `500` ) "Error : GLOBAL_MAX_CONCURRENT_REQ
      ( country_code = p_land  vat_number = `501` ) "Error : GLOBAL_MAX_CONCURRENT_REQ_TIME
      ( country_code = p_land  vat_number = `600` ) "Error : MS_MAX_CONCURRENT_REQ
      ( country_code = p_land  vat_number = `601` ) "Error : MS_MAX_CONCURRENT_REQ_TIME
    ).

  ELSE.
    vies_proxy = vies_proxy_real.
    vat_checks = VALUE #( ( country_code = p_land  vat_number = p_vat ) ).
  ENDIF.

  TRY.
      DATA(lo_vat_chk) = NEW zviesco_check_vat_port_type( logical_port_name = vies_proxy ).

      LOOP AT vat_checks ASSIGNING FIELD-SYMBOL(<fs_vat>).
        TRY.
            lo_vat_chk->check_vat(
              EXPORTING
                check_vat_request = VALUE zviescheck_vat_request(
                  country_code = <fs_vat>-country_code
                  vat_number = <fs_vat>-vat_number
                )
              IMPORTING
                check_vat_response = DATA(ls_response)
            ).

          CATCH cx_ai_system_fault INTO DATA(lo_exc_syst_fault).
            " Internal Error
            vies_proxy_err = lo_exc_syst_fault->get_text( ).
            CLEAR ls_response.
            MOVE-CORRESPONDING <fs_vat> TO ls_response.
            ls_response-name = vies_proxy_err.
        ENDTRY.

        APPEND VALUE #(
          country_code = ls_response-country_code
          vat_number = ls_response-vat_number
          request_date = ls_response-request_date
          valid = ls_response-valid
          name = ls_response-name
          address = ls_response-address
        ) TO vat_replies.

      ENDLOOP.

    CATCH cx_ai_system_fault INTO lo_exc_syst_fault.
      MESSAGE e000(ckmlwip) INTO DATA(lv_error_msg) ##NEEDED.
      vies_proxy_awol = lo_exc_syst_fault->get_text( ).
  ENDTRY.



END-OF-SELECTION.
  zcl_aab=>break_point( 'ZABO_DEMO' ).
  IF vies_proxy_awol IS INITIAL.
    cl_salv_table=>factory(
        EXPORTING list_display = abap_true
        IMPORTING r_salv_table = gr_table
        CHANGING t_table = vat_replies ).

    gr_table->get_columns( )->set_optimize( abap_true ).

    gr_table->display( ).
  ELSE.
    WRITE: / vies_proxy_awol.
  ENDIF.
