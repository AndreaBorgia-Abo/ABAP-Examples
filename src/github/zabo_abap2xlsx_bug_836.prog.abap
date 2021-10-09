*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_BUG_836
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/sapmentors/abap2xlsx/issues/836#issue-1020216975
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_bug_836.

CONSTANTS gc_save_file_name TYPE string VALUE 'issue-836.xlsx'.
INCLUDE zdemo_excel_outputopt_incl.
START-OF-SELECTION.
  SELECT carrid, connid, fldate, bookid FROM sbook UP TO 3 ROWS
  INTO TABLE @DATA(gt_sbook).
  TRY.
      DATA(excel) = NEW zcl_excel( ).
      DATA(worksheet) = excel->get_active_worksheet( ).
      worksheet->bind_table( ip_table = gt_sbook ).
      lcl_output=>output( excel ).
    CATCH cx_root INTO DATA(lx).
      MESSAGE lx TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
