*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_PR_1025
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/abap2xlsx/abap2xlsx/pull/1025#issuecomment-1184156058
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_pr_1025.

CONSTANTS: gc_save_file_name TYPE string VALUE '36_DefaultStyles.xlsx'.
INCLUDE zdemo_excel_outputopt_incl.

START-OF-SELECTION.
  DATA(lo_excel) = NEW zcl_excel( ).
  DATA(lo_worksheet) = lo_excel->get_active_worksheet( ).
  lo_worksheet->get_column( 1 )->set_width( 50 ).
  lo_worksheet->set_cell( ip_column = 1 ip_row = 1 ip_value = 'set_cell before set_default_style' ).
  lo_worksheet->set_cell( ip_column = 2 ip_row = 1 ip_value = sy-datum ).
  lo_excel->set_default_style( zcl_excel_style_changer=>create( lo_excel
                    )->set_font_name( zcl_excel_style_font=>c_name_arial
                    )->set_font_scheme( zcl_excel_style_font=>c_scheme_none
                    )->set_font_size( 20
                    )->get_guid( ) ).
  lo_worksheet->set_cell( ip_column = 1 ip_row = 2 ip_value = 'set_cell AFTER set_default_style' ).
  lo_worksheet->set_cell( ip_column = 2 ip_row = 2 ip_value = sy-datum ).
  lcl_output=>output( lo_excel ).
