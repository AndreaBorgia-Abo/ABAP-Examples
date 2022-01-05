*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_PR_930
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/abap2xlsx/abap2xlsx/issues/524#issuecomment-1001204292
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_pr_930.

TABLES sscrfields.
DATA: gc_save_file_name TYPE string VALUE 'test.xlsx'.
INCLUDE zdemo_excel_outputopt_incl.
DATA: lo_excel TYPE REF TO zcl_excel.

AT SELECTION-SCREEN.
  CHECK sscrfields-ucomm = 'ONLI'.
  DATA: lx_root TYPE REF TO cx_root.
  TRY.
      lo_excel = NEW zcl_excel( ).
      DATA(lo_worksheet) = lo_excel->get_active_worksheet( ).

      "==================
      " SET_AREA
      "==================
      lo_worksheet->set_area( ip_range = 'A1:B2' ip_formula = '"new R"&ROW()&"C"&COLUMN()' ip_area = lo_worksheet->c_area-whole ).
      lo_worksheet->set_area( ip_column_start = 'D' ip_column_end = 'E' ip_row = 1 ip_row_to = 2 ip_formula = '"old R"&ROW()&"C"&COLUMN()' ip_area = lo_worksheet->c_area-whole ).

      "==================
      " CHANGE_AREA_STYLE
      "==================
      lo_worksheet->change_area_style( ip_range = 'A1:B1'
                                       ip_style_changer = zcl_excel_style_changer=>create( lo_excel
                                                          )->set_fill_filltype( zcl_excel_style_fill=>c_fill_solid
                                                          )->set_fill_fgcolor_rgb( zcl_excel_style_color=>c_yellow ) ).
      lo_worksheet->change_area_style( ip_column_start = 'A' ip_row = 1 ip_row_to = 2
                                       ip_style_changer = zcl_excel_style_changer=>create( lo_excel
                                                          )->set_font_color( VALUE #( rgb = zcl_excel_style_color=>c_red ) ) ).

      "==================
      " CHANGE_CELL_STYLE
      "==================
      lo_worksheet->change_cell_style( ip_columnrow = 'D1'
                                       ip_fill_filltype = zcl_excel_style_fill=>c_fill_solid
                                       ip_fill_fgcolor_rgb = zcl_excel_style_color=>c_yellow ).
      lo_worksheet->change_cell_style( ip_column = 'D' ip_row = 1
                                       ip_font_color_rgb = zcl_excel_style_color=>c_red ).

      "==================
      " GET_CELL
      "==================
      lo_worksheet->get_cell( EXPORTING ip_columnrow = 'B3'
                              IMPORTING ep_value = DATA(value) ).
      lo_worksheet->get_cell( EXPORTING ip_column = 'B' ip_row = 3
                              IMPORTING ep_value = DATA(value2) ).
      lo_worksheet->get_cell( EXPORTING ip_column = 2 ip_row = 3
                              IMPORTING ep_value = DATA(value3) ).
      TRY.
          lo_worksheet->get_cell( EXPORTING ip_columnrow = 'B3' ip_column = 2 ip_row = 3
                                  IMPORTING ep_value = DATA(value4) ).
          MESSAGE 'get_cell too many row/column parameters' TYPE 'E'.
        CATCH cx_root INTO lx_root.
      ENDTRY.
      TRY.
          lo_worksheet->get_cell( IMPORTING ep_value = DATA(value5) ).
          MESSAGE 'get_cell missing row/column parameter' TYPE 'E'.
        CATCH cx_root INTO lx_root.
      ENDTRY.

      "==================
      " SET_AREA_HYPERLINK
      "==================
      lo_worksheet->set_area_hyperlink( ip_range = 'D1:E1' ip_url = 'https://www.google.com' ip_is_internal = abap_false ).
      lo_worksheet->set_area_hyperlink( ip_column_start = 'D' ip_column_end = 'E' ip_row = 2 ip_url = 'https://www.sap.com' ip_is_internal = abap_false ).

      "==================
      " SET_AREA_STYLE
      "==================
      DATA(style_font_blue) = lo_excel->add_new_style( ).
      style_font_blue->font->color-rgb = zcl_excel_style_color=>c_blue.
      lo_worksheet->set_area_style( ip_range = 'D1:E1' ip_style = style_font_blue->get_guid( ) ).
      lo_worksheet->set_area_style( ip_column_start = 'D' ip_column_end = 'E' ip_row = 2 ip_style = style_font_blue->get_guid( ) ).

      "==================
      " SET_CELL
      "==================
      lo_worksheet->set_cell( ip_columnrow = 'B3' ip_value = CONV decfloat16( '0.3-' ) ).
      lo_worksheet->set_cell( ip_column = 'C' ip_row = 4 ip_value = 'text' ).
      lo_worksheet->set_cell( ip_column = 4 ip_row = 5 ip_value = 'text' ).
      TRY.
          lo_worksheet->set_cell( ip_value = 'text' ).
          MESSAGE 'set_cell missing row/column parameter' TYPE 'E'.
        CATCH cx_root INTO lx_root.
      ENDTRY.
      TRY.
          lo_worksheet->set_cell( ip_columnrow = 'B3' ip_column = 'B' ip_row = 3 ip_value = 'text' ).
          MESSAGE 'set_cell too many row/column parameters' TYPE 'E'.
        CATCH cx_root INTO lx_root.
      ENDTRY.

      "==================
      " SET_CELL_FORMULA
      "==================
      lo_worksheet->set_cell_formula( ip_columnrow = 'C3' ip_formula = '2*-0.15' ).
      lo_worksheet->set_cell_formula( ip_column = 'A' ip_row = 4 ip_formula = '"te"&"xt"' ).

      "==================
      " SET_CELL_STYLE
      "==================
      lo_worksheet->set_cell_style( ip_columnrow = 'B3' ip_style = style_font_blue->get_guid( ) ).
      lo_worksheet->set_cell_style( ip_column = 'C' ip_row = 3 ip_style = style_font_blue->get_guid( ) ).

      "==================
      " SET_MERGE
      "==================
      DATA(style_center) = lo_excel->add_new_style( ).
      style_center->alignment->horizontal = zcl_excel_style_alignment=>c_horizontal_center.
      style_center->alignment->vertical = zcl_excel_style_alignment=>c_vertical_center.
      lo_worksheet->set_merge( ip_range = 'C3:D3' ip_style = style_center->get_guid( ) ).
      lo_worksheet->set_merge( ip_column_start = 'B' ip_row = 3 ip_row_to = 4 ip_style = style_center->get_guid( ) ).

      "==================
      " SET_MERGE_STYLE
      "==================
      DATA(style_fill_yellow) = lo_excel->add_new_style( ).
      style_fill_yellow->fill->filltype = zcl_excel_style_fill=>c_fill_solid.
      style_fill_yellow->fill->fgcolor-rgb = zcl_excel_style_color=>c_yellow.
      lo_worksheet->set_merge_style( ip_range = 'A4:A5' ip_style = style_fill_yellow->get_guid( ) ).
      lo_worksheet->set_merge_style( ip_column_start = 'D' ip_column_end = 'E' ip_row = 5 ip_row_to = 6 ip_style = style_fill_yellow->get_guid( ) ).

    CATCH cx_root INTO lx_root.
      MESSAGE lx_root TYPE 'E'.
  ENDTRY.
  ASSERT 1 = 1. " debug helper

START-OF-SELECTION.
  lcl_output=>output( cl_excel = lo_excel ).
  ASSERT 1 = 1. " debug helper
