*&---------------------------------------------------------------------*
*& Author: Jagdish Patil
*& Source: https://blogs.sap.com/2021/04/22/abap-code-to-internal-table-as-excel-file-on-sap-application-server/
*&---------------------------------------------------------------------*
CLASS zabo_itab_to_excel DEFINITION PUBLIC FINAL.
  PUBLIC SECTION.
    METHODS:
      itab_to_xstring
        IMPORTING ir_data_ref       TYPE REF TO data
        RETURNING VALUE(rv_xstring) TYPE xstring.
ENDCLASS.

CLASS zabo_itab_to_excel IMPLEMENTATION.
  METHOD itab_to_xstring.

    FIELD-SYMBOLS: <fs_data> TYPE ANY TABLE.
* Older ABAP:
*    DATA: lo_table TYPE REF TO cl_salv_table,
*          lt_fcat TYPE lvc_t_fcat,
*          lo_result TYPE REF TO cl_salv_ex_result_data_table.

    CLEAR rv_xstring.
    ASSIGN ir_data_ref->* TO <fs_data>.

    TRY.
* Older ABAP:
*        cl_salv_table=>factory(
*          IMPORTING r_salv_table = lo_table
*          CHANGING  t_table      = <fs_data> ).
*
*        lt_fcat =
*          cl_salv_controller_metadata=>get_lvc_fieldcatalog(
*            r_columns      = lo_table->get_columns( )
*            r_aggregations = lo_table->get_aggregations( ) ).
*
*        lo_result =
*          cl_salv_ex_util=>factory_result_data_table(
*            r_data         = ir_data_ref
*            t_fieldcatalog = lt_fcat ).

* Newer ABAP:
        cl_salv_table=>factory(
          IMPORTING r_salv_table = DATA(lo_table)
          CHANGING  t_table      = <fs_data> ).

        DATA(lt_fcat) =
          cl_salv_controller_metadata=>get_lvc_fieldcatalog(
            r_columns      = lo_table->get_columns( )
            r_aggregations = lo_table->get_aggregations( ) ).

        DATA(lo_result) =
          cl_salv_ex_util=>factory_result_data_table(
            r_data         = ir_data_ref
            t_fieldcatalog = lt_fcat ).

** First version (produces INVALID file on 7.31 release)
**********************************************************************
** Resulting header line)
** Client
** Airline Flight Number
** Date
** Airfare
** Airline
** Currency
** Plane Type
** Max. capacity econ.
** Occupied econ.
** Total
** Max. capacity bus.
** Occupied bus.
** Max. capacity 1st
** Occupied 1st
*        cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform(
*          EXPORTING
*            xml_type      = if_salv_bs_xml=>c_type_xlsx
*            xml_version   = cl_salv_bs_a_xml_base=>get_version( )
*            r_result_data = lo_result
*            xml_flavour   = if_salv_bs_c_tt=>c_tt_xml_flavour_export
*            gui_type      = if_salv_bs_xml=>c_gui_type_gui
*          IMPORTING
*            xml           = rv_xstring ).

* Alternative version (produces valid file on 7.31 release)
* Source: https://blogs.sap.com/2021/04/22/abap-code-to-internal-table-as-excel-file-on-sap-application-server/#comment-567945
**********************************************************************
* Resulting header line)
* Client
* Airline
* Connection Number
* Flight Date
* Airfare
* Airline local currency
* Plane Type
* Maximum capacity economy class
* Occupied seats economy class
* Booking total
* Maximum capacity business class
* Occupied seats business class
* Maximum capacity in first class
* Occupied seats in first class
        rv_xstring = lo_table->to_xml( xml_type = if_salv_bs_xml=>c_type_xlsx ).

      CATCH cx_root.
        CLEAR rv_xstring.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
