*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_BUG_836
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/abap2xlsx/abap2xlsx/issues/836#issuecomment-938111516
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
      worksheet->set_ignored_errors( VALUE #( ( cell_coords = 'B2:B4' number_stored_as_text = abap_true )
                                              ( cell_coords = 'D2:D4' number_stored_as_text = abap_true ) ) ).
      lcl_output=>output( excel ).
    CATCH cx_root INTO DATA(lx).
      MESSAGE lx TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
