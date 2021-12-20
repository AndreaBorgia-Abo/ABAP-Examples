*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_BUG_921
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/abap2xlsx/abap2xlsx/issues/921#issue-1083870170
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_bug_921.

DATA: lo_excel     TYPE REF TO zcl_excel,
      lo_worksheet TYPE REF TO zcl_excel_worksheet.
CONSTANTS: gc_save_file_name TYPE string VALUE 'contiguous_spaces_in_formula.xlsx'.
INCLUDE zdemo_excel_outputopt_incl.

START-OF-SELECTION.
  CREATE OBJECT lo_excel.
  lo_worksheet = lo_excel->get_active_worksheet( ).
  lo_worksheet->set_cell(
        ip_column  = 1
        ip_row     = 1
        ip_formula = '"    A    B    "' ).
  lcl_output=>output( lo_excel ).
