*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_BUG_1043
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/abap2xlsx/abap2xlsx/issues/1043#issuecomment-1180772083
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_bug_1043.

DATA(gc_save_file_name) = 'bug_1043.xlsx'.
INCLUDE zdemo_excel_outputopt_incl.

START-OF-SELECTION.
  DATA(workbook) = NEW zcl_excel( ).
  TYPES: BEGIN OF ty_excel_table_line,
           field_1 TYPE i,
           field_2 TYPE i,
           field_3 TYPE i,
         END OF ty_excel_table_line,
         ty_excel_table TYPE STANDARD TABLE OF ty_excel_table_line WITH EMPTY KEY.
  DATA(excel_table) = VALUE ty_excel_table( ).
  DATA(field_catalog) = zcl_excel_common=>get_fieldcatalog( ip_table = excel_table ).
  LOOP AT field_catalog ASSIGNING FIELD-SYMBOL(<field_catalog_line>).
    <field_catalog_line>-scrtext_l = 'Text Text Text Text Text Text Text Tex'.
  ENDLOOP.
  DATA(worksheet) = workbook->get_active_worksheet( ).
  worksheet->bind_table( ip_table          = excel_table
                         it_field_catalog  = field_catalog
                         is_table_settings = VALUE #( top_left_column = 'A' top_left_row = 1 )
                         iv_default_descr  = 'L' ).
