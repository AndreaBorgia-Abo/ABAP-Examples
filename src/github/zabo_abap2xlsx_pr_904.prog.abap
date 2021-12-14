*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_PR_904
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/abap2xlsx/abap2xlsx/pull/904#issuecomment-991775015
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_pr_904.

DATA: lo_excel     TYPE REF TO zcl_excel,
      lo_worksheet TYPE REF TO zcl_excel_worksheet.
CONSTANTS: gc_save_file_name TYPE string VALUE 'PR_904.xlsx'.
INCLUDE zdemo_excel_outputopt_incl.

START-OF-SELECTION.
  CREATE OBJECT lo_excel.
  lo_worksheet = lo_excel->get_active_worksheet( ).
  lo_worksheet->set_cell( ip_column = 1 ip_row = 1 ip_value = 'A1' ).
  lo_worksheet->set_cell( ip_column = 2 ip_row = 1 ip_value = 'B1' ).
  lo_worksheet->set_cell( ip_column = 3 ip_row = 1 ip_value = 'C1' ).
  DATA(lv_guid) = lo_worksheet->change_cell_style(
      ip_column           = 1
      ip_row              = 1
      ip_fill_filltype    = zcl_excel_style_fill=>c_fill_solid
      ip_fill_fgcolor_rgb = zcl_excel_style_color=>c_yellow ).
  lo_worksheet->set_cell_style( ip_column = 2 ip_row = 1 ip_style = lv_guid ).
  DATA(lv_guid2) = lo_worksheet->change_cell_style(
      ip_column         = 1
      ip_row            = 1
      ip_font_color_rgb = zcl_excel_style_color=>c_red ).
  lo_worksheet->set_cell_style( ip_column = 3 ip_row = 1 ip_style = lv_guid2 ).
  lcl_output=>output( lo_excel ).
