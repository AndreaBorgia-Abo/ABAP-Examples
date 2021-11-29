*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_PR_897
*&---------------------------------------------------------------------*
*& Author: Andrea Borgia (adapted from ZDEMO_EXCEL37 / ZDEMO_EXCEL15)
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_pr_897.

DATA: excel  TYPE REF TO zcl_excel,
      reader TYPE REF TO zif_excel_reader.

DATA: ex  TYPE REF TO zcx_excel,
      msg TYPE string.

DATA: worksheet      TYPE REF TO zcl_excel_worksheet,
      highest_column TYPE zexcel_cell_column,
      highest_row    TYPE int4,
      column         TYPE zexcel_cell_column VALUE 1,
      col_str        TYPE zexcel_cell_column_alpha,
      row            TYPE int4               VALUE 1,
      value          TYPE zexcel_cell_value.



PARAMETERS: p_upfile TYPE string LOWER CASE.



INITIALIZATION.
  IF p_upfile IS INITIAL.
    p_upfile = 'c:\temp\whatever.xlsx'.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_upfile.
  PERFORM f4_p_upfile CHANGING p_upfile.



START-OF-SELECTION.
  TRY.
      CREATE OBJECT reader TYPE zcl_excel_reader_huge_file.
      excel = reader->load_file( p_upfile ).

      worksheet = excel->get_active_worksheet( ).
      highest_column = worksheet->get_highest_column( ).
      highest_row    = worksheet->get_highest_row( ).

      WRITE: / 'Filename ', p_upfile.
      WRITE: / 'Highest column: ', highest_column, 'Highest row: ', highest_row.
      WRITE: /.

      WHILE row <= highest_row.
        WHILE column <= highest_column.
          col_str = zcl_excel_common=>convert_column2alpha( column ).
          worksheet->get_cell(
            EXPORTING
              ip_column = col_str
              ip_row    = row
            IMPORTING
              ep_value = value
          ).
          WRITE: value.
          column = column + 1.
        ENDWHILE.
        WRITE: /.
        column = 1.
        row = row + 1.
      ENDWHILE.
    CATCH zcx_excel INTO ex.    " Exceptions for ABAP2XLSX
      msg = ex->get_text( ).
      WRITE: / msg.
  ENDTRY.


*&---------------------------------------------------------------------*
*&      Form  F4_P_UPFILE
*&---------------------------------------------------------------------*
FORM f4_p_upfile  CHANGING p_upfile TYPE string.

  DATA: lv_repid       TYPE syrepid,
        lt_fields      TYPE dynpread_tabtype,
        ls_field       LIKE LINE OF lt_fields,
        lt_files       TYPE filetable,
        lv_rc          TYPE i,
        lv_file_filter TYPE string.

  lv_repid = sy-repid.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = lv_repid
      dynumb               = '1000'
      request              = 'A'
    TABLES
      dynpfields           = lt_fields
    EXCEPTIONS
      invalid_abapworkarea = 01
      invalid_dynprofield  = 02
      invalid_dynproname   = 03
      invalid_dynpronummer = 04
      invalid_request      = 05
      no_fielddescription  = 06
      undefind_error       = 07.
  READ TABLE lt_fields INTO ls_field WITH KEY fieldname = 'P_UPFILE'.
  p_upfile = ls_field-fieldvalue.

  lv_file_filter = 'Excel Files (*.XLSX;*.XLSM)|*.XLSX;*.XLSM'.
  cl_gui_frontend_services=>file_open_dialog( EXPORTING
                                                default_filename        = p_upfile
                                                file_filter             = lv_file_filter
                                              CHANGING
                                                file_table              = lt_files
                                                rc                      = lv_rc
                                              EXCEPTIONS
                                                OTHERS                  = 1 ).
  READ TABLE lt_files INDEX 1 INTO p_upfile.

ENDFORM.                    " F4_P_UPFILE
