CLASS zabo_cl_rest_test_post DEFINITION
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
ENDCLASS.



CLASS zabo_cl_rest_test_post IMPLEMENTATION.


  METHOD constructor.

    me->zabo_if_rest~response = io_response.
    me->zabo_if_rest~request = io_request.

  ENDMETHOD.


  METHOD zabo_if_rest~handle_request.
* Author: Matthijs Mennens
* Source: https://blogs.sap.com/2018/06/28/writing-a-sicf-service/

    DATA: lv_input  TYPE char1,
          lv_output TYPE char1.

    DATA: lv_json_body        TYPE string.
    DATA: lr_json_deserializer     TYPE REF TO cl_trex_json_deserializer.
    DATA: lv_string_writer    TYPE REF TO cl_sxml_string_writer.
    DATA: lv_xstring          TYPE xstring.

    CREATE OBJECT lr_json_deserializer.

***************************************************************************
    " JSON TO ABAP DATA
***************************************************************************
    lv_json_body = me->zabo_if_rest~request->get_cdata( ).

* JSON notes: when sending a single value, say X,
* "X" is valid JSON
* { "X" } is NOT!!!
* see: https://jsonformatter.curiousconcept.com/

    lr_json_deserializer->deserialize(
      EXPORTING
        json = lv_json_body
      IMPORTING
        abap = lv_input ).

***************************************************************************
    " CREATE OBJECT / DO WHATEVER YOU NEED TO DO HERE  WITH THE DATA !!!
***************************************************************************
    CALL FUNCTION 'ZABO_TESTFUNCTION'
      EXPORTING
        i_input  = lv_input
      IMPORTING
        o_output = lv_output.

***************************************************************************
    " CONVERT OUTPUT TO JSON STRING
***************************************************************************
    lv_string_writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
    CALL TRANSFORMATION id SOURCE array = lv_output RESULT XML lv_string_writer.
    lv_xstring = lv_string_writer->get_output( ).

***************************************************************************
    " RETURN CREATED OBJECT AS RESPONSE (CONVENTION)
***************************************************************************
    me->zabo_if_rest~response->set_data( data = lv_xstring ).

  ENDMETHOD.


  METHOD zabo_if_rest~set_response.

    CALL METHOD me->zabo_if_rest~response->set_data
      EXPORTING
        data = is_data.

  ENDMETHOD.
ENDCLASS.
