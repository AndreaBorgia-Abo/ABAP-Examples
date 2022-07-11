*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_BUG_1043
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/abap2xlsx/abap2xlsx/issues/1043#issuecomment-1180772083
*& Changes: added way more columns and output call
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
           field_4 TYPE i,
           field_5 TYPE i,
           field_6 TYPE i,
           field_7 TYPE i,
           field_8 TYPE i,
           field_9 TYPE i,
           field_10 TYPE i,
           field_11 TYPE i,
           field_12 TYPE i,
           field_13 TYPE i,
           field_14 TYPE i,
           field_15 TYPE i,
           field_16 TYPE i,
           field_17 TYPE i,
           field_18 TYPE i,
           field_19 TYPE i,
           field_20 TYPE i,
           field_21 TYPE i,
           field_22 TYPE i,
           field_23 TYPE i,
           field_24 TYPE i,
           field_25 TYPE i,
           field_26 TYPE i,
           field_27 TYPE i,
           field_28 TYPE i,
           field_29 TYPE i,
           field_30 TYPE i,
           field_31 TYPE i,
           field_32 TYPE i,
           field_33 TYPE i,
           field_34 TYPE i,
           field_35 TYPE i,
           field_36 TYPE i,
           field_37 TYPE i,
           field_38 TYPE i,
           field_39 TYPE i,
           field_40 TYPE i,
           field_41 TYPE i,
           field_42 TYPE i,
           field_43 TYPE i,
           field_44 TYPE i,
           field_45 TYPE i,
           field_46 TYPE i,
           field_47 TYPE i,
           field_48 TYPE i,
           field_49 TYPE i,
           field_50 TYPE i,
           field_51 TYPE i,
           field_52 TYPE i,
           field_53 TYPE i,
           field_54 TYPE i,
           field_55 TYPE i,
           field_56 TYPE i,
           field_57 TYPE i,
           field_58 TYPE i,
           field_59 TYPE i,
           field_60 TYPE i,
           field_61 TYPE i,
           field_62 TYPE i,
           field_63 TYPE i,
           field_64 TYPE i,
           field_65 TYPE i,
           field_66 TYPE i,
           field_67 TYPE i,
           field_68 TYPE i,
           field_69 TYPE i,
           field_70 TYPE i,
           field_71 TYPE i,
           field_72 TYPE i,
           field_73 TYPE i,
           field_74 TYPE i,
           field_75 TYPE i,
           field_76 TYPE i,
           field_77 TYPE i,
           field_78 TYPE i,
           field_79 TYPE i,
           field_80 TYPE i,
           field_81 TYPE i,
           field_82 TYPE i,
           field_83 TYPE i,
           field_84 TYPE i,
           field_85 TYPE i,
           field_86 TYPE i,
           field_87 TYPE i,
           field_88 TYPE i,
           field_89 TYPE i,
           field_90 TYPE i,
           field_91 TYPE i,
           field_92 TYPE i,
           field_93 TYPE i,
           field_94 TYPE i,
           field_95 TYPE i,
           field_96 TYPE i,
           field_97 TYPE i,
           field_98 TYPE i,
           field_99 TYPE i,
           field_100 TYPE i,
           field_101 TYPE i,
           field_102 TYPE i,
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
  lcl_output=>output( workbook ).
