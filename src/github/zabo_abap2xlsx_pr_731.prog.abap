*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_PR_731
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/abap2xlsx/abap2xlsx/pull/731#issuecomment-917445212
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_pr_731.

CLASS ltc_main DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS setup.
    METHODS get_count
      IMPORTING
        iterator      TYPE REF TO zcl_excel_collection_iterator
      RETURNING
        VALUE(result) TYPE i.
    METHODS set_cell_columns_iterator FOR TESTING.
    METHODS set_cell_rows_iterator FOR TESTING.
    METHODS bind_table_columns_iterator FOR TESTING.
    METHODS bind_table_rows_iterator FOR TESTING.
    METHODS set_cell_columns FOR TESTING.
    METHODS set_cell_rows FOR TESTING.
    METHODS bind_table_columns FOR TESTING.
    METHODS bind_table_rows FOR TESTING.
    DATA: lo_excel         TYPE REF TO zcl_excel,
          lo_worksheet     TYPE REF TO zcl_excel_worksheet,
          scarr_lines      TYPE STANDARD TABLE OF scarr,
          columns_iterator TYPE REF TO zcl_excel_collection_iterator,
          rows_iterator    TYPE REF TO zcl_excel_collection_iterator,
          columns          TYPE REF TO zcl_excel_columns,
          rows             TYPE REF TO zcl_excel_rows.
ENDCLASS.
CLASS ltc_main IMPLEMENTATION.
  METHOD setup.
    SELECT * FROM scarr INTO TABLE @scarr_lines.
    lo_excel = NEW zcl_excel( ).
    lo_worksheet = lo_excel->get_active_worksheet( ).
  ENDMETHOD.
  METHOD set_cell_columns_iterator.
    lo_worksheet->set_cell( ip_column = 2 ip_row = 3 ip_value = 'a' ).
    columns_iterator = lo_worksheet->get_columns_iterator( ).
    cl_abap_unit_assert=>assert_true( columns_iterator->has_next( ) ).
    cl_abap_unit_assert=>assert_equals( act = get_count( columns_iterator ) exp = 1 ).
  ENDMETHOD.
  METHOD set_cell_rows_iterator.
    lo_worksheet->set_cell( ip_column = 2 ip_row = 3 ip_value = 'a' ).
    rows_iterator = lo_worksheet->get_rows_iterator( ).
    cl_abap_unit_assert=>assert_true( rows_iterator->has_next( ) ).
    cl_abap_unit_assert=>assert_equals( act = get_count( rows_iterator ) exp = 1 ).
  ENDMETHOD.
  METHOD bind_table_columns_iterator.
    lo_worksheet->bind_table(
        ip_table          = scarr_lines
        is_table_settings = VALUE zexcel_s_table_settings( top_left_column = 'B' top_left_row = 3 ) ).
    columns_iterator = lo_worksheet->get_columns_iterator( ).
    cl_abap_unit_assert=>assert_true( columns_iterator->has_next( ) ).
    cl_abap_unit_assert=>assert_equals( act = get_count( columns_iterator ) exp = 4 ).
  ENDMETHOD.
  METHOD bind_table_rows_iterator.
    lo_worksheet->bind_table(
        ip_table          = scarr_lines
        is_table_settings = VALUE zexcel_s_table_settings( top_left_column = 'B' top_left_row = 3 ) ).
    rows_iterator = lo_worksheet->get_rows_iterator( ).
    cl_abap_unit_assert=>assert_true( rows_iterator->has_next( ) ).
    cl_abap_unit_assert=>assert_equals( act = get_count( rows_iterator ) exp = lines( scarr_lines ) + 1 ).
  ENDMETHOD.
  METHOD set_cell_columns.
    lo_worksheet->set_cell( ip_column = 2 ip_row = 3 ip_value = 'a' ).
    columns = lo_worksheet->get_columns( ).
    cl_abap_unit_assert=>assert_equals( act = columns->size( ) exp = 1 ).
  ENDMETHOD.
  METHOD set_cell_rows.
    lo_worksheet->set_cell( ip_column = 2 ip_row = 3 ip_value = 'a' ).
    rows = lo_worksheet->get_rows( ).
    cl_abap_unit_assert=>assert_equals( act = rows->size( ) exp = 1 ).
  ENDMETHOD.
  METHOD bind_table_columns.
    lo_worksheet->bind_table(
        ip_table          = scarr_lines
        is_table_settings = VALUE zexcel_s_table_settings( top_left_column = 'B' top_left_row = 3 ) ).
    columns = lo_worksheet->get_columns( ).
    cl_abap_unit_assert=>assert_equals( act = columns->size( ) exp = 4 ).
  ENDMETHOD.
  METHOD bind_table_rows.
    lo_worksheet->bind_table(
        ip_table          = scarr_lines
        is_table_settings = VALUE zexcel_s_table_settings( top_left_column = 'B' top_left_row = 3 ) ).
    rows = lo_worksheet->get_rows( ).
    cl_abap_unit_assert=>assert_equals( act = rows->size( ) exp = lines( scarr_lines ) + 1 ).
  ENDMETHOD.
  METHOD get_count.
    result = 0.
    WHILE iterator->has_next( ) = abap_true.
      iterator->get_next( ).
      result = result + 1.
    ENDWHILE.
  ENDMETHOD.
ENDCLASS.

DATA: gc_save_file_name TYPE string VALUE 'PR_731.xlsx'.
INCLUDE zdemo_excel_outputopt_incl.
PARAMETERS iterator AS CHECKBOX.

START-OF-SELECTION.
  SELECT * FROM scarr INTO TABLE @DATA(scarr_lines).
  DATA(lo_excel) = NEW zcl_excel( ).
  DATA(lo_worksheet) = lo_excel->get_active_worksheet( ).
  lo_worksheet->set_cell( ip_column = 1 ip_row = 1 ip_value = 'a' ).
  lo_worksheet->bind_table(
      ip_table          = scarr_lines
      is_table_settings = VALUE zexcel_s_table_settings( top_left_column = 'A' top_left_row = 3 ) ).
  IF iterator = abap_true.
    DATA(lo_rows_iterator) = lo_worksheet->get_rows_iterator( ).
    WHILE abap_true = lo_rows_iterator->has_next( ).
      DATA(lo_row) = CAST zcl_excel_row( lo_rows_iterator->get_next( ) ).
      DATA(lo_columns_iterator) = lo_worksheet->get_columns_iterator( ).
      WHILE abap_true = lo_columns_iterator->has_next( ).
        DATA(lo_column) = CAST zcl_excel_column( lo_columns_iterator->get_next( ) ).
        lo_worksheet->get_cell(
          EXPORTING
            ip_column  = lo_column->get_column_index( )
            ip_row     = lo_row->get_row_index( )
          IMPORTING
            ep_value   = DATA(lv_value) ).
        WRITE: / 'row', lo_row->get_row_index( ), ', column:', lo_column->get_column_index( ), ', cell:', lv_value.
      ENDWHILE.
    ENDWHILE.
  ELSE.
    lcl_output=>output( lo_excel ).
  ENDIF.
