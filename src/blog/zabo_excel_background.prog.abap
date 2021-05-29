*&---------------------------------------------------------------------*
*& Report zabo_excel_background
*&---------------------------------------------------------------------*
*& Author: Andrea Borgia (uses class by Łukasz Pęgiel)
*& Source: https://abapblog.com/articles/tricks/120-create-xlsx-file-from-internal-table-in-background-v2
*&---------------------------------------------------------------------*
REPORT zabo_excel_background.

DATA: lv_xls_file TYPE string VALUE `/tmp/sflight_excel_bg.xlsx`,
      t_rawdata   TYPE solix_tab.


SELECT * FROM sflight
INTO TABLE @DATA(t_sflight).

DATA(lv_xstring) = NEW zabo_xlsx_from_itab(  )->create_xlsx_from_itab(
*                                               it_fieldcat   =
*                                               it_sort       =
*                                               it_filt       =
*                                               is_layout     =
*                                               it_hyperlinks =
                                               it_data       = t_sflight
                                             ).
t_rawdata = cl_bcs_convert=>xstring_to_solix( iv_xstring  = lv_xstring ).
DATA(bytecount) = xstrlen( lv_xstring ).

cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize = bytecount
                                                  filename     = lv_xls_file
                                                  filetype     = 'BIN'
                                         CHANGING data_tab     = t_rawdata ).
