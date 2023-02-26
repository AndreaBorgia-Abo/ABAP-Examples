*&---------------------------------------------------------------------*
*& Report ZABO_FORMAT_PACKAGE
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://gist.github.com/sandraros/9282da1c7c39c944a87d843e49508651
*&---------------------------------------------------------------------*
REPORT ZABO_FORMAT_PACKAGE.

SELECT-OPTIONS s_prog FOR sy-repid.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (64) label_1.
PARAMETERS prog_bef TYPE syrepid DEFAULT 'ZTEMPORARY'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (64) label_2.
PARAMETERS prog_aft TYPE syrepid DEFAULT 'ZTEMPORARY2'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (64) label_3.
PARAMETERS ign_ind AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (64) label_4.
PARAMETERS ign_com AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF LINE.

INITIALIZATION.
  label_1 = 'Program to contain code before pretty printer to show differences'.
  label_2 = 'Program to contain code after pretty printer to show differences'.
  label_3 = 'Ignore indentations when determining differences'.
  label_4 = 'Ignore comments when determining differences'.

  s_prog[] = VALUE #(
      sign = 'I' option = 'CP' ( low = 'ZCL_EXCEL*' )
                               ( low = 'ZCX_EXCEL*' )
                               ( low = 'ZIF_EXCEL*' )
                               ( low = 'ZABAP2XLSX*' )
                               ( low = 'ZDEMO_TECHED*' )
                               ( low = 'ZDEMO_EXCEL*' )
      sign = 'I' option = 'EQ' ( low = 'ZANGRY_BIRDS' )
                               ( low = 'ZDEMO_CALENDAR' )
                               ( low = 'ZDEMO_CALENDAR_CLASSES' )
                               ( low = 'ZTEST_EXCEL_IMAGE_HEADER' )
                               ( low = 'ZEXCEL_BIND_SUB_ALV' )
                               ( low = 'ZEXCEL_TEMPLATE_GET_TYPES' )
      sign = 'E' option = 'CP' ( low = '*CP' )    " class pools
                               ( low = '*IP' ) ). " interface pools

START-OF-SELECTION.
  TYPES: table_programs  TYPE STANDARD TABLE OF syrepid WITH DEFAULT KEY,
         table_altbl1024 TYPE STANDARD TABLE OF altbl1024 WITH DEFAULT KEY.

  DATA(abap_lines_dummy) = VALUE string_table( ).
  READ REPORT prog_bef INTO abap_lines_dummy.
  IF sy-subrc <> 0 OR abap_lines_dummy IS NOT INITIAL.
    WRITE |include program { prog_bef } must exist and be empty|.
    STOP.
  ENDIF.
  READ REPORT prog_aft INTO abap_lines_dummy.
  IF sy-subrc <> 0 OR abap_lines_dummy IS NOT INITIAL.
    WRITE |include program { prog_aft } must exist and be empty|.
    STOP.
  ENDIF.

  SELECT name FROM trdir
      WHERE name IN @s_prog
    INTO TABLE @DATA(programs).

  DATA(ls_vers_udefs) = VALUE vers_udefs( uname = sy-uname ).
  SELECT SINGLE * FROM vers_udefs INTO ls_vers_udefs WHERE uname = sy-uname.
  ls_vers_udefs-comp_mode = SWITCH #( boolc( ign_ind = 'X' ) && boolc( ign_com = 'X' )
                            WHEN '  ' THEN 1
                            WHEN 'X ' THEN 2
                            WHEN ' X' THEN 3
                            WHEN 'XX' THEN 4 ).
  MODIFY vers_udefs FROM ls_vers_udefs.

  DATA(valid_programs) = VALUE table_programs( ).
  DATA(invalid_programs) = VALUE table_programs( ).

  LOOP AT programs INTO DATA(program).
    DATA(abap_lines) = VALUE abaptxt255_tab( ).
    DATA(abap_lines_after_pretty_print) = VALUE abaptxt255_tab( ).
    READ REPORT program INTO abap_lines.
    CALL FUNCTION 'PRETTY_PRINTER'
      EXPORTING
        inctoo             = abap_false
      TABLES
        ntext              = abap_lines_after_pretty_print
        otext              = abap_lines
      EXCEPTIONS
        enqueue_table_full = 1
        include_enqueued   = 2
        include_readerror  = 3
        include_writeerror = 4
        OTHERS             = 5.
    DATA(diff) = abap_false.
    IF abap_lines_after_pretty_print <> abap_lines.
      INSERT REPORT prog_bef FROM abap_lines.
      INSERT REPORT prog_aft FROM abap_lines_after_pretty_print.
      COMMIT WORK AND WAIT.
      SUBMIT rsvrsrs3
            WITH objnam2 = prog_aft
            WITH objname = prog_bef
            WITH objtyp1 = 'REPS'
            WITH objtyp2 = 'REPS'
            WITH versno1 = 0
            WITH versno2 = 0
            EXPORTING LIST TO MEMORY
            AND RETURN.
      DATA(lt_listobject) = VALUE table_abaplist( ).
      DATA(lt_listasci) = VALUE table_altbl1024( ).
      CALL FUNCTION 'LIST_FROM_MEMORY'
        TABLES
          listobject = lt_listobject
        EXCEPTIONS
          not_found  = 1
          OTHERS     = 2.
      IF sy-subrc = 0.
        CLEAR lt_listasci.
        CALL FUNCTION 'LIST_TO_ASCI'
          TABLES
            listasci   = lt_listasci
            listobject = lt_listobject
          EXCEPTIONS
            OTHERS     = 3.
        " EITHER (when indentations or comments are to be ignored)
        "     Apart from some indentations and comments that may differ,...
        "     the sources of the two versions are the same.
        " OR (when doing an exact comparison)
        "     There are no differences in the sources of these versions.
        IF NOT line_exists( lt_listasci[ line = 'the sources of the two versions are the same.' ] )
        AND NOT line_exists( lt_listasci[ line = 'There are no differences in the sources of these versions.' ] ).
          diff = abap_true.
          WRITE : / program COLOR COL_NEGATIVE.
          CALL FUNCTION 'WRITE_LIST'
            TABLES
              listobject = lt_listobject
            EXCEPTIONS
              empty_list = 1
              OTHERS     = 2.
        ENDIF.
      ENDIF.
    ENDIF.
    IF diff = abap_false.
      APPEND program TO valid_programs.
    ELSE.
      APPEND program TO invalid_programs.
    ENDIF.
  ENDLOOP.
  LOOP AT valid_programs INTO program.
    WRITE : / program COLOR COL_POSITIVE.
  ENDLOOP.
  LOOP AT invalid_programs INTO program.
    WRITE : / program COLOR COL_NEGATIVE.
  ENDLOOP.
  " empty the temporary programs, so that next run can start without error
  abap_lines_dummy = VALUE #( ).
  INSERT REPORT prog_bef FROM abap_lines_dummy.
  INSERT REPORT prog_aft FROM abap_lines_dummy.
  COMMIT WORK.
