*&---------------------------------------------------------------------*
*& Report zabo_abap2xlsx_bug_1020
*&---------------------------------------------------------------------*
*& Author: nes77
*& Source: https://github.com/abap2xlsx/abap2xlsx/issues/1020#issue-1210791256
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_bug_1020.
*Based on zdemo_excel36

DATA:
  lo_excel              TYPE REF TO zcl_excel,
  lo_worksheet          TYPE REF TO zcl_excel_worksheet,
  lo_style_arial20      TYPE REF TO zcl_excel_style,
  lv_style_arial20_guid TYPE zexcel_cell_style,
  lv_date               TYPE sy-datum.

CONSTANTS: gc_save_file_name TYPE string VALUE '36_DefaultStyles.xlsx'.
INCLUDE zdemo_excel_outputopt_incl.

START-OF-SELECTION.

  CREATE OBJECT lo_excel.

  lo_style_arial20                  = lo_excel->add_new_style( ).
  lo_style_arial20->font->name      = zcl_excel_style_font=>c_name_arial.
  lo_style_arial20->font->scheme    = zcl_excel_style_font=>c_scheme_none.
  lo_style_arial20->font->size      = 20.
  lv_style_arial20_guid             = lo_style_arial20->get_guid( ).

  lo_excel->set_default_style( lv_style_arial20_guid ).  " Default for all new worksheets

*1st sheet - do not change anything --> defaultstyle from lo_excel should apply
  lo_worksheet = lo_excel->get_active_worksheet( ).
  lo_worksheet->set_title( 'Style for complete document' ).

  lo_worksheet->set_cell( ip_column = 2 ip_row = 4 ip_value = 'All cells in this sheet are set to font Arial, fontsize 20' ).
  lo_worksheet->set_cell( ip_column = 2 ip_row = 5 ip_value = 'because no separate style was passed for this sheet' ).
  lo_worksheet->set_cell( ip_column = 2 ip_row = 6 ip_value = 'but a default style was set for the complete instance of zcl_excel' ).

  lo_worksheet->set_cell( ip_column = 1 ip_row = 10 ip_value = 'test' ).

  lv_date = sy-datum.
  lo_worksheet->set_cell( ip_column = 1 ip_row = 11 ip_value = lv_date ).

  lcl_output=>output( lo_excel ).
