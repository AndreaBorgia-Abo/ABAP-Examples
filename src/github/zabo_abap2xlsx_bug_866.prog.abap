*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_BUG_866
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/sapmentors/abap2xlsx/issues/866#issue-1041613104
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_bug_866.

CONSTANTS: gc_save_file_name TYPE string VALUE 'test.xlsx'.
INCLUDE zdemo_excel_outputopt_incl.

START-OF-SELECTION.
  DATA(lo_excel) = NEW zcl_excel( ).
  DATA(lo_worksheet) = lo_excel->get_active_worksheet( ).
  lo_worksheet->set_cell_formula( ip_column = 1 ip_row = 1 ip_formula = '"hello"&" "&"world"' ).
  lcl_output=>output( lo_excel ).
