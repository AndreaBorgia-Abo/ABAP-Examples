*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_BUG_767
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/abap2xlsx/abap2xlsx/issues/767#issue-933039758
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_bug_767.

DATA: gc_save_file_name TYPE string VALUE 'test.xlsx'.
INCLUDE zdemo_excel_outputopt_incl.

START-OF-SELECTION.
  DATA: BEGIN OF line,
          c1 TYPE string,
          c2 TYPE string,
        END OF line,
        table LIKE TABLE OF line.
  table = VALUE #( ( c1 = `Cell 1` c2 = `Cell 2` ) ).
  DATA(lo_excel) = NEW zcl_excel( ).
  DATA(lo_worksheet) = lo_excel->get_active_worksheet( ).
  lo_worksheet->bind_table(
      ip_table          = table
      is_table_settings = VALUE #(
                            top_left_column = 'A'
                            top_left_row    = 1 ) ).
  lo_worksheet->bind_table(
      ip_table          = table
      is_table_settings = VALUE #(
                            top_left_column = 'A'
                            top_left_row    = 2 ) ). " <== table 1 overlapping not detected/ 1 would be detected
  lcl_output=>output( cl_excel = lo_excel ).
