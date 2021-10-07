*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_BUG_727
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/sapmentors/abap2xlsx/issues/727#issuecomment-936171906
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_bug_727.

DATA: gc_save_file_name TYPE string VALUE 'test.xlsx'.
INCLUDE zdemo_excel_outputopt_incl.
PARAMETERS bug RADIOBUTTON GROUP rb2 DEFAULT 'X'.
PARAMETERS no_bug RADIOBUTTON GROUP rb2.

START-OF-SELECTION.
  DATA(lo_excel) = NEW zcl_excel( ).
  DATA(lo_sheet1) = lo_excel->get_active_worksheet( ).
  DATA(lo_sheet2) = lo_excel->add_new_worksheet( ip_title = COND #( WHEN bug = 'X' THEN 'Sheet2' ELSE 'Sheet' ) ).
  SELECT * FROM sbook UP TO 10 ROWS INTO TABLE @DATA(sbook_lines).
  lo_sheet2->bind_table( ip_table = sbook_lines ).
  DATA(num_columns) = lines( CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( NEW sbook( ) ) )->components ).
  DATA(num_rows) = lines( sbook_lines ) + 1.
  DO num_rows TIMES.
    DATA(row) = sy-index.
    DO num_columns TIMES.
      DATA(column) = sy-index.
      lo_sheet1->set_cell( ip_column  = column
                           ip_row     = row
                           ip_formula = zcl_excel_common=>shift_formula(
                                      iv_reference_formula = |{ lo_sheet2->get_title( ) }!A1|
                                      iv_shift_cols        = column - 1
                                      iv_shift_rows        = row - 1 ) ).
    ENDDO.
  ENDDO.
  lcl_output=>output( lo_excel ).
