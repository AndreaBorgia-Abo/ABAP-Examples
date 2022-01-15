*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_PR_965
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/abap2xlsx/abap2xlsx/pull/965#issue-1097233983
*& Note: evolution of ZABO_ABAP2XLSX_BUG_751
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_pr_965.

DATA: gc_save_file_name TYPE string.
INCLUDE zdemo_excel_outputopt_incl.
PARAMETERS calltwic AS CHECKBOX.

START-OF-SELECTION.
  TRY.
      DATA(excel) = NEW zcl_excel( ).
      DATA(style_changer) = zcl_excel_style_changer=>create( excel ).
      style_changer->set_fill_filltype( zcl_excel_style_fill=>c_fill_solid )->set_fill_fgcolor_rgb( zcl_excel_style_color=>c_yellow ).
      DATA(sheet) = excel->get_active_worksheet( ).
      sheet->set_cell( ip_columnrow = 'A1' ip_value = 'Hello world' ).
      style_changer->apply( ip_worksheet = sheet ip_column = 'A' ip_row = 1 ).
      DATA(writer) = NEW zcl_excel_writer_2007( ).
      DATA(lv_xl_xdata) = writer->zif_excel_writer~write_file( excel ).
      DATA(reader) = NEW zcl_excel_reader_2007( ).
      excel = reader->zif_excel_reader~load( lv_xl_xdata ).
      IF calltwic = abap_true.
        excel = reader->zif_excel_reader~load( lv_xl_xdata ). " <=== used to produce issue 751
      ENDIF.
      IF calltwic = abap_false.
        gc_save_file_name = 'issue-751-once.xlsx'.
      ELSE.
        gc_save_file_name = 'issue-751-twice.xlsx'.
      ENDIF.
      lcl_output=>output( excel ).
    CATCH cx_root INTO DATA(lx_root).
      MESSAGE lx_root TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
