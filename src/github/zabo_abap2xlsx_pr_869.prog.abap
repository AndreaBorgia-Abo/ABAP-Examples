*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_PR_869
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/sapmentors/abap2xlsx/pull/869#issuecomment-962623853
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_pr_869.

DATA: gc_save_file_name TYPE string VALUE 'PR_869.xlsx'.
INCLUDE zdemo_excel_outputopt_incl.

START-OF-SELECTION.
  DATA(lo_excel) = NEW zcl_excel( ).
  DATA(lo_worksheet) = lo_excel->get_active_worksheet( ).
  lo_worksheet->set_area(
      ip_column_start = 1
      ip_column_end   = 2
      ip_row          = 1
      ip_row_to       = 2
      ip_value        = 'Hello world'
      ip_merge        = abap_false ).
  lcl_output=>output( lo_excel ).
