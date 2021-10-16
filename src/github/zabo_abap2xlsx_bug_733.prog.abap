*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_BUG_733
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/sapmentors/abap2xlsx/issues/733#issue-804863530
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_bug_733.

DATA: gc_save_file_name TYPE string VALUE 'test.&'.
INCLUDE zdemo_excel_outputopt_incl.

START-OF-SELECTION.
  SELECT * FROM scarr INTO TABLE @DATA(scarr).
  DATA(lo_excel) = NEW zcl_excel( ).
  DATA(lo_worksheet) = lo_excel->get_active_worksheet( ).
  lo_worksheet->bind_table(
    EXPORTING
      ip_table            = scarr
      is_table_settings   = VALUE zexcel_s_table_settings(
                              table_name       = 'XFD1'  " <====== unfortunately this name is also a cell reference
                              top_left_column  = 'A'
                              top_left_row     = 1 ) ).
  lcl_output=>output( cl_excel = lo_excel ).
