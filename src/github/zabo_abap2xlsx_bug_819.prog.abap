*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_BUG_819
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/abap2xlsx/abap2xlsx/issues/819#issue-1014141719
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_bug_819.

CONSTANTS: gc_save_file_name TYPE string VALUE 'issue.xlsx'.
INCLUDE zdemo_excel_outputopt_incl.

START-OF-SELECTION.
  DATA(lo_excel) = NEW zcl_excel( ).
  DATA(lo_worksheet) = lo_excel->get_active_worksheet( ).
  lo_worksheet->set_cell( ip_column = 1 ip_row = 1 ip_value = 'HeadA' ).
  lo_worksheet->set_cell( ip_column = 2 ip_row = 1 ip_value = 'HeadB' ).
  lo_worksheet->set_cell( ip_column = 3 ip_row = 1 ip_value = 'HeadC' ).
  lo_worksheet->set_cell( ip_column = 2 ip_row = 2 ip_value = 'B2' ).
  lo_worksheet->set_cell( ip_column = 2 ip_row = 3 ip_value = 'B3' ).
  DATA(lo_autofilter) = lo_excel->add_new_autofilter( io_sheet = lo_worksheet ) .
  lo_autofilter->set_filter_area( VALUE #( row_start = 1 col_start = 1
                                           row_end   = 3 col_end   = 3 ) ).
  lo_autofilter->set_value( i_column = 2 i_value = 'B2' ).
  DATA(lo_writer) = NEW zcl_excel_writer_2007( ).
  DATA(lo_reader2) = NEW zcl_excel_reader_2007( ).
  DATA(lo_excel2) = lo_reader2->zif_excel_reader~load( lo_writer->zif_excel_writer~write_file( lo_excel ) ).
  lcl_output=>output( lo_excel2 ).
