*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_BUG_1098
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/abap2xlsx/abap2xlsx/issues/1098#issuecomment-1510354691
*&---------------------------------------------------------------------*
REPORT ZABO_ABAP2XLSX_BUG_1098.

DATA: gc_save_file_name TYPE string VALUE 'test.xlsx'.
INCLUDE zdemo_excel_outputopt_incl.
DATA g_dummy_x2 TYPE x LENGTH 2.

SELECT-OPTIONS x2_codes FOR g_dummy_x2.

PARAMETERS p_wr2007 RADIOBUTTON GROUP rb3 DEFAULT 'X'.
PARAMETERS p_wrhuge RADIOBUTTON GROUP rb3.

selection-screen begin of block b01 WITH FRAME title text_B01.
PARAMETERS p_rd_no  RADIOBUTTON GROUP rb4 DEFAULT 'X'.
PARAMETERS p_rd2007 RADIOBUTTON GROUP rb4.
PARAMETERS p_rdhuge RADIOBUTTON GROUP rb4.
selection-screen end of block b01.

INITIALIZATION.
* NB: in Excel, U+0000, U+0002, U+001C to U+001F, invalid surrogate characters (e.g. U+D800 + U+DC00) appear like invisible characters, others appear like boxes or ? characters
  x2_codes[] = VALUE #( sign = 'I' option = 'EQ'
  ( low = '0000' ) ( low = '0001' ) ( low = '0002' ) ( low = '0003' ) ( low = '0004' )  ( low = '0005' ) ( low = '0006' ) ( low = '0007' ) ( low = '0008' )
  ( low = '000B' ) ( low = '000C' )
  ( low = '000E' ) ( low = '000F' )  ( low = '0010' ) ( low = '0011' ) ( low = '0012' ) ( low = '0013' ) ( low = '0014' ) ( low = '0015' ) ( low = '0016' )
  ( low = '0017' ) ( low = '0018' ) ( low = '0019' ) ( low = '001A' ) ( low = '001B' ) ( low = '001C' ) ( low = '001D' ) ( low = '001E' ) ( low = '001F' )
  ( low = 'D83D' ) ( low = 'DCA3' ) " Bomb icon U+1F4A3 = UTF-16 D83D DCA3
  ( low = 'FFFE' ) ( low = 'FFFF' )
  ).
text_b01 = ''.

START-OF-SELECTION.
  TRY.
      DATA(lo_excel) = NEW zcl_excel( ).
      DATA(lo_worksheet) = lo_excel->get_active_worksheet( ).
      TYPES: BEGIN OF ty_itab_line,
               text TYPE c LENGTH 100,
             END OF ty_itab_line,
             ty_itab TYPE STANDARD TABLE OF ty_itab_line WITH EMPTY KEY.
      DATA(control_characters) = REDUCE string( INIT t = `` FOR <x2_code> IN x2_codes NEXT t = t && cl_abap_conv_in_ce=>uccp( <x2_code>-low ) ).
      DATA(table) = VALUE ty_itab( ( text = control_characters ) ).
      lo_worksheet->bind_table(
          ip_table            = table
          is_table_settings   = VALUE #( top_left_column = 'A' top_left_row = 1 )
          iv_default_descr    = 'L' ).
      DATA(style) = lo_excel->add_new_style( ).
      DATA(red) = lo_excel->add_new_style( io_clone_of = style ).
      red->font->color-rgb = 'FFFF0000'.
      DATA(len_control_characters) = strlen( control_characters ).
      lo_worksheet->set_cell(
        ip_columnrow = 'A4'
        ip_value = |normal red { control_characters } { control_characters } xxx|
        ip_style = style
        it_rtf = VALUE #(
            ( offset = 7 length = 3 font = red->font->get_structure( ) )
            ( offset = 12 + len_control_characters length = len_control_characters font = red->font->get_structure( ) ) ) ).
      IF p_rd_no = abap_false.
        TYPES ty_ref_zif_excel_reader TYPE REF TO zif_excel_reader.
        DATA(lo_reader) = COND ty_ref_zif_excel_reader(
          WHEN p_rd2007 = 'X' THEN NEW zcl_excel_reader_2007( )
          WHEN p_rdhuge = 'X' THEN NEW zcl_excel_reader_huge_file( ) ).
        lo_excel = lo_reader->load( i_excel2007 = NEW zcl_excel_writer_2007( )->zif_excel_writer~write_file( lo_excel ) ).
      ENDIF.

      " write or display the generated Excel file
      DATA(writerclass_name) = COND string(
                WHEN p_wr2007 = 'X' THEN 'ZCL_EXCEL_WRITER_2007'
                WHEN p_wrhuge = 'X' THEN 'ZCL_EXCEL_WRITER_HUGE_FILE' ).
      lcl_output=>output( cl_excel = lo_excel iv_writerclass_name = writerclass_name ).

    CATCH cx_root INTO DATA(lx_root).
      MESSAGE lx_root TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
