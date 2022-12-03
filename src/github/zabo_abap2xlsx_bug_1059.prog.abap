*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_BUG_1059
*&---------------------------------------------------------------------*
*& Author: Andrea Borgia (adapted from ZABO_ABAP2XLSX_PR_897)
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_bug_1059.

DATA: excel            TYPE REF TO zcl_excel,
      reader           TYPE REF TO zif_excel_reader,
      writer           TYPE REF TO zif_excel_writer,
      lv_file          TYPE xstring,
      lv_bytecount     TYPE i,
      lt_file_tab      TYPE solix_tab,
      ex               TYPE REF TO zcx_excel,
      msg              TYPE string,
      worksheet        TYPE REF TO zcl_excel_worksheet,
      existing_col     TYPE REF TO zcl_excel_column,
      nonexisting_col  TYPE REF TO zcl_excel_column,
      output_file_path TYPE string.


PARAMETERS: p_upfile TYPE string LOWER CASE.



INITIALIZATION.
  IF p_upfile IS INITIAL.
    p_upfile = 'c:\temp\abap2xlsx_bug_1059-test_input.xlsx'.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_upfile.
  PERFORM f4_p_upfile CHANGING p_upfile.



START-OF-SELECTION.
  TRY.
      CREATE OBJECT reader TYPE zcl_excel_reader_2007.
      excel = reader->load_file( p_upfile ).

      worksheet = excel->get_active_worksheet( ).

* Demonstrates overwriting existing column with new "COL3"
      worksheet->add_new_column( ip_column = 1 ). " to insert new column COL3 in excel file
      worksheet->set_cell( ip_column = 1 ip_row = 1 ip_value = 'COL3' ). "Setting Column Header for new column
      worksheet->set_cell( ip_column = 1 ip_row = 2 ip_value = 'C32' ). "Setting Row 1 Column Value for new column COL3
      worksheet->set_cell( ip_column = 1 ip_row = 3 ip_value = 'C33' ). "Setting Row 1 Column Value for new column COL3

* Demonstrates checking existence before overwriting (this output will NOT appear)
      existing_col = worksheet->get_column( ip_column = 2 ).
      IF existing_col IS NOT BOUND.
        worksheet->add_new_column( ip_column = 2 ).
        worksheet->set_cell( ip_column = 2 ip_row = 1 ip_value = 'COL4' ).
        worksheet->set_cell( ip_column = 2 ip_row = 2 ip_value = 'C42' ).
        worksheet->set_cell( ip_column = 2 ip_row = 3 ip_value = 'C43' ).
      ENDIF.

* Intended to demonstrate adding a new column at the end,
* unfortunately get_column will call add_new_column internally
* so this path is never taken!
      nonexisting_col = worksheet->get_column( ip_column = 3 ).
      IF nonexisting_col IS NOT BOUND.
        worksheet->add_new_column( ip_column = 3 ).
        worksheet->set_cell( ip_column = 3 ip_row = 1 ip_value = 'COL5' ).
        worksheet->set_cell( ip_column = 3 ip_row = 2 ip_value = 'C52' ).
        worksheet->set_cell( ip_column = 3 ip_row = 3 ip_value = 'C53' ).
      ENDIF.

      CREATE OBJECT writer TYPE zcl_excel_WRITER_2007.
      lv_file = writer->write_file( excel ).

      " Convert to binary
      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = lv_file
        IMPORTING
          output_length = lv_bytecount
        TABLES
          binary_tab    = lt_file_tab.

      output_file_path = p_upfile.
      REPLACE '.xlsx' IN output_file_path WITH '-edited.xlsx'.

      " Save the file
      cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize = lv_bytecount
                                                        filename     = output_file_path
                                                        filetype     = 'BIN'
                                               CHANGING data_tab     = lt_file_tab ).

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
