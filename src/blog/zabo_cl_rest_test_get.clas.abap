CLASS zabo_cl_rest_test_get DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zabo_if_rest .

    METHODS constructor
      IMPORTING
        !io_request  TYPE REF TO if_http_request
        !io_response TYPE REF TO if_http_response .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS get_result
      IMPORTING
        VALUE(io_request) TYPE REF TO if_http_request
      RETURNING
        VALUE(eo_result)  TYPE char1
      EXCEPTIONS
        invalidinput .
ENDCLASS.



CLASS zabo_cl_rest_test_get IMPLEMENTATION.


  METHOD constructor.

    me->zabo_if_rest~response = io_response.
    me->zabo_if_rest~request = io_request.

  ENDMETHOD.


  METHOD get_result.

    DATA: lv_input TYPE char1.

    lv_input = me->zabo_if_rest~request->get_form_field('input').

    CALL FUNCTION 'ZABO_TESTFUNCTION'
      EXPORTING
        i_input  = lv_input
      IMPORTING
        o_output = eo_result
*      EXCEPTIONS
*       zexception = 1
*       OTHERS   = 2
      .
    IF sy-subrc <> 0.
* Implement exception handling
    ENDIF.

  ENDMETHOD.


  METHOD zabo_if_rest~handle_request.
* Author: Matthijs Mennens
* Source: https://blogs.sap.com/2018/06/28/writing-a-sicf-service/

***************************************************************************
    " VARIABLES
***************************************************************************
    DATA: lv_string_writer  TYPE REF TO cl_sxml_string_writer.
    DATA: lv_xstring        TYPE xstring.
    DATA: lv_input TYPE char1.

***************************************************************************
    " EXECUTE BACKEND METHOD
***************************************************************************
    TRY.

        lv_input = get_result( me->zabo_if_rest~request ).

***************************************************************************
        " CONVERT DATA TO JSON
***************************************************************************
        lv_string_writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
        CALL TRANSFORMATION id SOURCE array =  lv_input RESULT XML lv_string_writer.
        lv_xstring = lv_string_writer->get_output( ).

***************************************************************************
        " ADD THE JSON DATA TO THE RESPONSE
***************************************************************************
        me->zabo_if_rest~response->set_data( data = lv_xstring ).

      CATCH cx_root.
    ENDTRY.


  ENDMETHOD.


  METHOD zabo_if_rest~set_response.

    CALL METHOD me->zabo_if_rest~response->set_data
      EXPORTING
        data = is_data.

  ENDMETHOD.
ENDCLASS.
