*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_BUG_867
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/sapmentors/abap2xlsx/issues/867#issue-1041627058
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_bug_867.
CONSTANTS: gc_save_file_name TYPE string VALUE 'test.xlsx'.
INCLUDE zdemo_excel_outputopt_incl.

START-OF-SELECTION.
  DATA(lo_excel) = NEW zcl_excel( ).
  DATA(lo_worksheet) = lo_excel->get_active_worksheet( ).
  lo_worksheet->set_area(
          ip_column_start = 1
          ip_column_end   = 10
          ip_row          = 1
          ip_row_to       = 2
          ip_value        = 'Google' ).
  lo_worksheet->set_area_hyperlink(
          ip_column_start = 1
          ip_column_end   = 10
          ip_row          = 1
          ip_row_to       = 2
          ip_url          = 'https://www.google.com'
          ip_is_internal  = abap_false ).
* lo_worksheet->set_cell( ip_column = 1 ip_row = 3 ip_value = '' ). " workaround bug 866
* See https://github.com/sapmentors/abap2xlsx/issues/866#issuecomment-967222653
  lo_worksheet->set_cell( ip_column = 2 ip_row = 3 ip_value = '' ).
  lo_worksheet->set_area_formula(
          ip_column_start = 'A'
          ip_column_end   = 'J'
          ip_row          = 3
          ip_row_to       = 4
          ip_formula      = '"hello"&" "&"world"' ).
  DATA(style) = lo_excel->add_new_style( ).
  style->fill->fgcolor-rgb = zcl_excel_style_color=>c_yellow.
  style->fill->filltype = zcl_excel_style_fill=>c_fill_solid.
  lo_worksheet->set_area_style(
          ip_column_start = 1
          ip_column_end   = 10
          ip_row          = 5
          ip_row_to       = 6
          ip_style        = style->get_guid( ) ).
  lcl_output=>output( lo_excel ).
