*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_PR_844
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/sapmentors/abap2xlsx/pull/860#issuecomment-955801796
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_pr_844.

DATA: gc_save_file_name TYPE string VALUE 'test.xlsx'.
INCLUDE zdemo_excel_outputopt_incl.

START-OF-SELECTION.
  TRY.
      DATA(lo_excel) = NEW zcl_excel( ).
      DATA(lo_worksheet) = lo_excel->get_active_worksheet( ).
      lo_worksheet->set_cell( ip_column = 'B' ip_row = 2 ip_value = CONV decfloat16( 1 ) ).
      " 1) cell should have no border
      lo_worksheet->change_cell_style(
            ip_column             = 'B'
            ip_row                = 2
            ip_borders_allborders = VALUE #( border_style = zcl_excel_style_border=>c_border_thick
                                             border_color = VALUE #( rgb = zcl_excel_style_color=>c_darkred ) )
            ip_xborders_allborders = VALUE #( border_style = abap_false
                                              border_color = VALUE #( rgb = abap_false ) )
            ip_xborders = VALUE #( allborders = VALUE #( border_style = abap_true
                                                         border_color = VALUE #( rgb = abap_true ) ) ) ).
      " 2) cell should have borders
      lo_worksheet->change_cell_style(
            ip_column             = 'D'
            ip_row                = 2
            ip_borders_allborders = VALUE #( border_style = zcl_excel_style_border=>c_border_thick
                                             border_color = VALUE #( rgb = zcl_excel_style_color=>c_darkred ) )
            ip_xborders_allborders = VALUE #( border_style = abap_true
                                              border_color = VALUE #( rgb = abap_true ) ) ).
      " 3) cell should have borders
      lo_worksheet->change_cell_style(
            ip_column             = 'F'
            ip_row                = 2
            ip_borders_allborders = VALUE #( border_style = zcl_excel_style_border=>c_border_thick
                                             border_color = VALUE #( rgb = zcl_excel_style_color=>c_darkred ) ) ).
      lcl_output=>output( cl_excel = lo_excel ).
    CATCH cx_root INTO DATA(lx_root).
      MESSAGE lx_root TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
