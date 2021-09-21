*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_BUG_751
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/sapmentors/abap2xlsx/issues/751#issuecomment-922033164
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_bug_751.

DATA: gc_save_file_name TYPE string VALUE 'test.xlsx'.
PARAMETERS p_upfile TYPE string LOWER CASE DEFAULT 'C:\Temp\Template.xlsx'.
INCLUDE zdemo_excel_outputopt_incl.

START-OF-SELECTION.
  TRY.
      DATA(reader) = NEW zcl_excel_reader_2007( ).
      DATA(excel) = reader->zif_excel_reader~load_file( p_upfile ).
      DATA(writer) = NEW zcl_excel_writer_2007( ).
      DATA(lv_xl_xdata) = writer->zif_excel_writer~write_file( excel ).
*      excel = reader->zif_excel_reader~load( lv_xl_xdata ).                     " <=== produces issue 751
      excel = NEW zcl_excel_reader_2007( )->zif_excel_reader~load( lv_xl_xdata ). " <=== solves issue 751
      lcl_output=>output( excel ).
    CATCH cx_root INTO DATA(lx_root).
      MESSAGE lx_root TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
