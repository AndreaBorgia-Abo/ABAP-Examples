*&---------------------------------------------------------------------*
*& Report ZABO_SCREENSHOT
*&---------------------------------------------------------------------*
*& Author: Harshal Kulkarni
*& Source: https://answers.sap.com/answers/678755/view.html
*&---------------------------------------------------------------------*
REPORT zabo_screenshot.

PARAMETERS : lv_fpath     LOWER CASE TYPE string default `/tmp/screenshot.png`.

DATA: lv_mime_type TYPE string.
DATA: lv_image_bytes TYPE xstring.

TRY.
*       Screenshot function call
    cl_gui_frontend_services=>get_screenshot( IMPORTING
                                                mime_type_str = lv_mime_type
                                                image         = lv_image_bytes ).

*
*      * XSTRING -> SOLIX (RAW)
    DATA(it_raw_data) = cl_bcs_convert=>xstring_to_solix( EXPORTING iv_xstring = lv_image_bytes ).


*       Daten lokal speichern
    cl_gui_frontend_services=>gui_download( EXPORTING
                                              filename = lv_fpath
                                              filetype = 'BIN'
                                            CHANGING
                                              data_tab = it_raw_data ).


*          ENDIF.
  CATCH cx_root INTO DATA(e_txt).
ENDTRY.
