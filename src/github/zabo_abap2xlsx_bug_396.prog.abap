*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_BUG_396
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/abap2xlsx/abap2xlsx/issues/396#issuecomment-1030686724
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_bug_396.

DATA: lo_excel     TYPE REF TO zcl_excel,
      lo_worksheet TYPE REF TO zcl_excel_worksheet.
CONSTANTS: gc_save_file_name TYPE string VALUE 'issue_396.xlsx'.
INCLUDE zdemo_excel_outputopt_incl.

START-OF-SELECTION.
  CREATE OBJECT lo_excel.
  lo_worksheet = lo_excel->get_active_worksheet( ).
  lo_worksheet->set_cell( ip_columnrow = 'A1' ip_value = 'Paper height = 194.5mm and width = 141.8mm' ).
  lo_worksheet->sheet_setup->paper_height = '194.5mm'.
  lo_worksheet->sheet_setup->paper_width = '141.8mm'.
  lcl_output=>output( lo_excel ).
