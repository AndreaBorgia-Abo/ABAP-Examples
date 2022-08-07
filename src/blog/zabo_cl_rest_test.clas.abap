CLASS zabo_cl_rest_test DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS get_rest
      IMPORTING
        !io_server     TYPE REF TO if_http_server
      RETURNING
        VALUE(eo_rest) TYPE REF TO zabo_if_rest .
ENDCLASS.



CLASS zabo_cl_rest_test IMPLEMENTATION.


  METHOD get_rest.


***************************************************************************
    " VARIABLES
***************************************************************************
    DATA: lv_class_name           TYPE seoclsname.
    DATA: lv_request_method       TYPE string.

***************************************************************************
    " APPEND REQUEST METHOD TO BASE CLASS
***************************************************************************
    lv_request_method = io_server->request->get_header_field( '~request_method' ).

    CONCATENATE 'ZABO_CL_REST_TEST_' lv_request_method INTO lv_class_name.

***************************************************************************
    " RETURN CLASS OBJECT
***************************************************************************
    TRY.
        CREATE OBJECT eo_rest
        TYPE (lv_class_name)
        EXPORTING
        io_request   = io_server->request
        io_response  = io_server->response.

***************************************************************************
        " ERRORS
***************************************************************************
      CATCH cx_sy_create_object_error.
    ENDTRY.

  ENDMETHOD.


  METHOD if_http_extension~handle_request.
* Author: Matthijs Mennens
* Source: https://blogs.sap.com/2018/06/28/writing-a-sicf-service/

***************************************************************************
    " VARIABLES
***************************************************************************
    DATA: lo_rest_class     TYPE REF TO zabo_if_rest.
    DATA: lo_error          TYPE REF TO cx_root.
    DATA: lv_reason         TYPE string.

***************************************************************************
    " GET THE CLASS OBJECT
***************************************************************************
    TRY.

        lo_rest_class ?= get_rest( io_server = server ).

***************************************************************************
        " EXECUTE THE RETRIEVED CLASS
***************************************************************************
        lo_rest_class->handle_request( ).

***************************************************************************
        " ERROR
***************************************************************************
      CATCH cx_root INTO lo_error.

        lv_reason = lo_error->get_text( ).
        server->response->set_status( code = 500 reason = lv_reason ).

    ENDTRY.

  ENDMETHOD.
ENDCLASS.
