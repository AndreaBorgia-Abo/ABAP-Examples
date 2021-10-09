*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_BUG_745
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/sapmentors/abap2xlsx/issues/745#issuecomment-933805297
*&---------------------------------------------------------------------*
*& Run it with ALV_ONLY selected, hide some columns and total some other, save the layout as default
*& Run it with BINDONLY selected => no hidden columns and no total.
*& Run it with BIND_ALV selected => columns are hidden and total is shown.
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_bug_745.


CONSTANTS gc_save_file_name TYPE string VALUE 'issue-745.xlsx'.
INCLUDE zdemo_excel_outputopt_incl.
PARAMETERS alv_only RADIOBUTTON GROUP rb2 DEFAULT 'X'.
PARAMETERS bindonly RADIOBUTTON GROUP rb2.
PARAMETERS alv_bind RADIOBUTTON GROUP rb2.

START-OF-SELECTION.
  SELECT * FROM sbook UP TO 15 ROWS
  INTO TABLE @DATA(gt_sbook).
  TRY.
      cl_salv_table=>factory(
        EXPORTING
          list_display = abap_false
        IMPORTING
          r_salv_table = DATA(lo_salv)
        CHANGING
          t_table      = gt_sbook[] ).
      cl_salv_bs_runtime_info=>clear_all( ).
      DATA(layout) = lo_salv->get_layout( ).
      layout->set_key( VALUE salv_s_layout_key( report = sy-repid ) ).
      layout->set_default( abap_true ).
      layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
      IF alv_only = abap_true OR alv_bind = abap_true.
        lo_salv->get_functions( )->set_all( ).
        lo_salv->display( ).
      ENDIF.
      IF bindonly = abap_true OR alv_bind = abap_true.
        cl_salv_bs_runtime_info=>clear_all( ).
        cl_salv_bs_runtime_info=>set(
          EXPORTING
            display  = abap_false
            metadata = abap_true
            data     = abap_true ).
        lo_salv->display( ).
        DATA(excel)     = NEW zcl_excel( ).
        DATA(worksheet) = excel->get_active_worksheet( ).
        worksheet->bind_alv(
            io_alv   = lo_salv
            it_table = gt_sbook  ).
        lcl_output=>output( excel ).
      ENDIF.
    CATCH cx_root INTO DATA(lx).
      MESSAGE lx TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
