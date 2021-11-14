*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_PR_869
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/sapmentors/abap2xlsx/pull/869#issuecomment-968069559
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_pr_869.

CONSTANTS: gc_save_file_name TYPE string VALUE 'PR_869.xlsx'.
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
  "-- NEW FEATURE --
  lo_worksheet->set_area(
          ip_column_start = 1
          ip_column_end   = 10
          ip_row          = 7
          ip_row_to       = 8
          ip_value        = 'Google'
          ip_area         = lo_worksheet->c_area-whole ).
  lo_worksheet->set_area_hyperlink(
          ip_column_start = 1
          ip_column_end   = 10
          ip_row          = 7
          ip_row_to       = 8
          ip_url          = 'https://www.google.com'
          ip_is_internal  = abap_false ).
  lo_worksheet->set_area_formula(
          ip_column_start = 'A'
          ip_column_end   = 'J'
          ip_row          = 9
          ip_row_to       = 10
          ip_formula      = '"hello"&" "&"world"'
          ip_area         = lo_worksheet->c_area-whole ).
  lcl_output=>output( lo_excel ).
