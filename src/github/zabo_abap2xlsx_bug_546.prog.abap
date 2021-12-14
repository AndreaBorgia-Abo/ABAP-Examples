*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_BUG_546
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/abap2xlsx/abap2xlsx/issues/546#issuecomment-932781725
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_bug_546.

CONSTANTS: gc_save_file_name TYPE string VALUE 'issue_546.xlsx'.
INCLUDE zdemo_excel_outputopt_incl.

START-OF-SELECTION.
  DATA(lo_excel) = NEW zcl_excel( ).
  DATA(lo_worksheet) = lo_excel->get_active_worksheet( ).
  lo_worksheet->set_cell( ip_column = 2 ip_row = 2 ip_value = 'HeadA' ).
  lo_worksheet->set_cell( ip_column = 3 ip_row = 2 ip_value = 'HeadB' ).
  lo_worksheet->set_cell( ip_column = 4 ip_row = 2 ip_value = 'HeadC' ).
  DATA(lo_autofilter) = lo_excel->add_new_autofilter( io_sheet = lo_worksheet ) .
  lo_autofilter->set_filter_area( VALUE #( row_start = 2 col_start = 1
                                           row_end   = 2 col_end   = 3 ) ).
*  lo_autofilter->set_value( i_column = 3 i_value = 'B1' ).
  lcl_output=>output( lo_excel ).
