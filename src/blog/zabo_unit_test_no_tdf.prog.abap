*&---------------------------------------------------------------------*
*& Report zabo_unit_test_no_tdf
*&---------------------------------------------------------------------*
*& Author: Andrea Borgia
*& Source: variant of https://blogs.sap.com/2020/05/03/example-with-open-sql-test-double-framework/
*&         for systems without TDF support (< 7.51 ?)
*& Useful info:
*& https://answers.sap.com/questions/12945373/how-can-authority-checks-be-mocked-in-unit-tests.html
*& https://blogs.sap.com/2013/04/29/abap-doc/
*&---------------------------------------------------------------------*
REPORT zabo_unit_test_no_tdf.


"! This interface enables switching between real and mock data
"! for the purporse of unit tests.
INTERFACE lif_client_getter.
  TYPES client  TYPE t000.
  TYPES clients TYPE TABLE OF t000 WITH KEY mandt.

  METHODS get_clients
    IMPORTING
      client        TYPE mandt OPTIONAL
    RETURNING
      VALUE(result) TYPE clients.
ENDINTERFACE.


"! This class implements the production version
"! which reads clients from table T000
CLASS lcl_system_clients DEFINITION
                         FINAL
                         CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_client_getter.

ENDCLASS.

CLASS lcl_system_clients IMPLEMENTATION.
  METHOD lif_client_getter~get_clients.
    IF client IS INITIAL.
      SELECT * FROM t000 AS clients
        INTO TABLE result.
    ELSE.
      SELECT SINGLE * FROM t000 AS clients
        INTO @DATA(single_result)
        WHERE mandt = @client.
      IF single_result IS NOT INITIAL.
        APPEND single_result TO result.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


"! This class extracts all system clients
CLASS lcl_extract_data DEFINITION.
  PUBLIC SECTION.

    "! Constructs real or mock data extraction class
    METHODS constructor
      IMPORTING io_client_getter TYPE REF TO lif_client_getter OPTIONAL.

    "! Get list of all system clients
    METHODS get_client_list
      IMPORTING VALUE(i_client)       TYPE lif_client_getter=>client OPTIONAL
      RETURNING VALUE(rv_client_list) TYPE lif_client_getter=>clients.

  PRIVATE SECTION.
    "! Pointer to backend extraction class, real or mock
    DATA o_client_getter TYPE REF TO lif_client_getter.
ENDCLASS.

CLASS lcl_extract_data IMPLEMENTATION.
  METHOD constructor.
    o_client_getter = COND #( WHEN io_client_getter IS BOUND
                              THEN io_client_getter
                              ELSE NEW lcl_system_clients( )
      ).
  ENDMETHOD.

  METHOD get_client_list.
    rv_client_list = o_client_getter->get_clients(  ).
  ENDMETHOD.
ENDCLASS.


"! Sample exception class
CLASS lcx_exception DEFINITION
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF mandt_not_found,
        msgid TYPE symsgid VALUE 'CL_UNIT_TEST_NO_TDF',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END   OF mandt_not_found.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS lcx_exception IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


**********************************************************************
START-OF-SELECTION.
**********************************************************************
  DATA(g_extract_data) = NEW lcl_extract_data(  ).
  DATA(g_clients) = g_extract_data->get_client_list(  ).

  cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(g_salv)
          CHANGING
            t_table      = g_clients ).

  g_salv->display( ).



**********************************************************************
*   A B A P   U n i t   T e s t   c o m p o n e n t
**********************************************************************
  "! Mock data provider (workaround for systems w/o cl_abap_testdouble)
CLASS lcl_mock_data_provider DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES lif_client_getter.

    "! When using mock data, this loads the fake contents of T000
    METHODS set_mock_data
      IMPORTING
        clientlist TYPE lif_client_getter=>clients.

  PRIVATE SECTION.
    "! Fale contents of T000
    DATA clients_test_data TYPE lif_client_getter~clients.
ENDCLASS.

CLASS lcl_mock_data_provider IMPLEMENTATION.
  METHOD set_mock_data.
    clients_test_data = clientlist.
  ENDMETHOD.

  METHOD lif_client_getter~get_clients.

    IF client IS INITIAL.
      result = clients_test_data.
    ELSE.
      READ TABLE clients_test_data
        WITH KEY mandt = client
        INTO DATA(single_result).
      IF sy-subrc = 0.
        APPEND single_result TO result.
      ELSE.
        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING
            textid = lcx_exception=>mandt_not_found.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS ltc_test DEFINITION FINAL
               FOR TESTING
               DURATION SHORT
               RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS get_client_000 FOR TESTING.
    METHODS get_client_001 FOR TESTING.
    METHODS get_client_123 FOR TESTING.
    METHODS get_client_999 FOR TESTING.

  PRIVATE SECTION.
    CLASS-DATA o_cut TYPE REF TO lcl_extract_data.
    CLASS-DATA o_client_mock TYPE REF TO lcl_mock_data_provider.

    CLASS-METHODS class_setup.
ENDCLASS.

CLASS ltc_test IMPLEMENTATION.

  METHOD class_setup.
    o_client_mock = NEW #(  ).
    cl_abap_unit_assert=>assert_bound( o_client_mock ).

    o_cut = NEW lcl_extract_data( o_client_mock ).
    cl_abap_unit_assert=>assert_bound( o_cut ).

    o_client_mock->set_mock_data(
     VALUE lcl_mock_data_provider=>lif_client_getter~clients(
      ( mandt = '000' mtext = 'Test double 000' )
      ( mandt = '001' mtext = 'Test double 001' )
      ( mandt = '999' mtext = 'Test double 999' ) ) ).
  ENDMETHOD.

  METHOD get_client_000.
    DATA(result) = o_cut->get_client_list(  ).
    cl_abap_unit_assert=>assert_table_contains(
      EXPORTING
        line             = VALUE o_client_mock->lif_client_getter~client( mandt = '000' mtext = 'Test double 000' )
        table            = result
    ).
  ENDMETHOD.

  METHOD get_client_001.
    DATA(result) = o_cut->get_client_list(  ).
    cl_abap_unit_assert=>assert_table_contains(
      EXPORTING
        line             = VALUE o_client_mock->lif_client_getter~client( mandt = '001' mtext = 'Test double 001' )
        table            = result
    ).
  ENDMETHOD.

  METHOD get_client_123.
    DATA(result) = o_cut->get_client_list(  ).
    cl_abap_unit_assert=>assert_table_not_contains(
      EXPORTING
        line             = VALUE o_client_mock->lif_client_getter~client( mandt = '123' mtext = 'Test double 123' )
        table            = result
    ).
  ENDMETHOD.

  METHOD get_client_999.
    DATA(result) = o_cut->get_client_list(  ).
    cl_abap_unit_assert=>assert_table_contains(
      EXPORTING
        line             = VALUE o_client_mock->lif_client_getter~client( mandt = '999' mtext = 'Test double 999' )
        table            = result
    ).
  ENDMETHOD.

ENDCLASS.
