*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_BUG_1261
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/abap2xlsx/abap2xlsx/issues/1261#issue-2471612827
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_bug_1261.

CONSTANTS gc_save_file_name TYPE string VALUE 'Issue_1261.xlsx'.
INCLUDE zdemo_excel_outputopt_incl.

START-OF-SELECTION.
  DATA(lo_excel) = NEW zcl_excel( ).
  DATA(lo_worksheet) = lo_excel->get_active_worksheet( ).
  lo_worksheet->set_cell( ip_columnrow = 'A1'
                          ip_formula   = 'IF(1=1,2,3)' ).
  lcl_output=>output( lo_excel ).
