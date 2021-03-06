*&---------------------------------------------------------------------*
*& Report zabo_excel_without_abap2xlsx
*&---------------------------------------------------------------------*
*& Author: Andrea Borgia (uses class by Jagdish Patil)
*& Source: https://blogs.sap.com/2021/04/22/abap-code-to-internal-table-as-excel-file-on-sap-application-server/
*&---------------------------------------------------------------------*
REPORT zabo_excel_without_abap2xlsx.


DATA: lv_xls_file TYPE string VALUE `/tmp/sflight_no_abap2xlsx.xlsx`,
      t_rawdata   TYPE solix_tab.
* Older ABAP:
*DATA: lo_data_ref TYPE REF TO data,
*      lv_xstring  TYPE xstring,
*      bytecount   TYPE i,
*      lo_mail     TYPE REF TO zabo_itab_to_excel.

SELECT * FROM sflight
INTO TABLE @DATA(t_sflight).

* Older ABAP:
*GET REFERENCE OF t_sflight INTO lo_data_ref.
*CREATE OBJECT lo_mail.
*lv_xstring = lo_mail->itab_to_xstring( lo_data_ref ).
*t_rawdata = cl_bcs_convert=>xstring_to_solix( iv_xstring  = lv_xstring ).
*bytecount = xstrlen( lv_xstring ).

* Newer ABAP:
GET REFERENCE OF t_sflight INTO DATA(lo_data_ref).
DATA(lv_xstring) = NEW zabo_itab_to_excel( )->itab_to_xstring( lo_data_ref ).
t_rawdata = cl_bcs_convert=>xstring_to_solix( iv_xstring  = lv_xstring ).
DATA(bytecount) = xstrlen( lv_xstring ).

cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize = bytecount
                                                  filename     = lv_xls_file
                                                  filetype     = 'BIN'
                                         CHANGING data_tab     = t_rawdata ).
