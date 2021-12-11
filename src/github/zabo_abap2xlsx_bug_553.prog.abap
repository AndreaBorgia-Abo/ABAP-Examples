*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_BUG_553
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/abap2xlsx/abap2xlsx/issues/553#issuecomment-966594892
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_bug_553.

DATA: gc_save_file_name TYPE string VALUE 'issue_553.xlsx'.
INCLUDE zdemo_excel_outputopt_incl.

START-OF-SELECTION.
  DATA(lo_excel) = NEW zcl_excel( ).
  DATA(lo_worksheet) = lo_excel->get_active_worksheet( ).
  lo_worksheet->set_cell( ip_column = 'A' ip_row = 1 ip_value = 'row 1' ).
  lo_worksheet->set_cell( ip_column = 'A' ip_row = 2 ip_value = 'row 2' ).
  lo_worksheet->set_cell( ip_column = 'A' ip_row = 3 ip_value = 'row 3' ).
  lo_worksheet->set_row_height( ip_row = 1 ip_height_fix = 30 ).
  DATA(lo_row) = lo_worksheet->get_row( 2 ).
  lo_row->set_row_height( ip_row_height = 40 ip_custom_height = abap_true ).
  lo_row = lo_worksheet->get_row( 3 ).
  lo_row->set_row_height( ip_row_height = 50 ip_custom_height = abap_false ).
  lo_worksheet->sheet_setup->fit_to_page       = 'X'.  " you should turn this on to activate fit_to_height and fit_to_width
  lo_worksheet->sheet_setup->fit_to_height     = 0.    " used only if ip_fit_to_page = 'X'
  lo_worksheet->sheet_setup->fit_to_width      = 2.    " used only if ip_fit_to_page = 'X'
  TRY.
      lcl_output=>output( cl_excel = lo_excel iv_writerclass_name = 'ZCL_EXCEL_WRITER_HUGE_FILE' ).
    CATCH cx_root INTO DATA(lx).
      MESSAGE lx TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
