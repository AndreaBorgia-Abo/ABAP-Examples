*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_BUG_1081
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/abap2xlsx/abap2xlsx/issues/1081#issue-1560911924
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_bug_1081.

DATA: gc_save_file_name TYPE string VALUE 'test.xlsx'.
INCLUDE zdemo_excel_outputopt_incl.

START-OF-SELECTION.
  DATA(lo_excel) = NEW zcl_excel( ).
  DATA(lo_worksheet) = lo_excel->get_active_worksheet( ).
  DATA(style_large_bold) = lo_excel->add_new_style( ).
  lo_excel->set_default_style( style_large_bold->get_guid( ) ).
  lo_worksheet->set_cell( ip_columnrow = 'A2' ip_value = 'test' ).
  DATA(lo_writer) = CAST zif_excel_writer( NEW zcl_excel_writer_2007( ) ).
  DATA(xstring) = lo_writer->write_file( lo_excel ).
  DATA(lo_reader) = NEW zcl_excel_reader_2007( ).
  DATA(lo_excel_2) = lo_reader->zif_excel_reader~load( i_excel2007 = xstring ). " <=== Exception
  lo_excel = lo_excel_2.
  lcl_output=>output( cl_excel = lo_excel ).
