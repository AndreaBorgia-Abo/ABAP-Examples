*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_BUG_932
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/abap2xlsx/abap2xlsx/issues/932#issue-1088778180
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_bug_932.

DATA: gc_save_file_name TYPE string VALUE 'test.xlsx'.
INCLUDE zdemo_excel_outputopt_incl.

START-OF-SELECTION.
  DATA(lo_excel) = NEW zcl_excel( ).
  DATA(lo_worksheet) = lo_excel->get_active_worksheet( ).
  lo_worksheet->set_cell_formula( ip_column = 'C' ip_row = 4 ip_formula = '"te"&"xt"' ).
  lcl_output=>output( cl_excel = lo_excel ).
