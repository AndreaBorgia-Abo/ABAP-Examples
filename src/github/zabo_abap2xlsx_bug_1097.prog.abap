*&---------------------------------------------------------------------*
*& Report zabo_abap2xlsx_bug_1097
*&---------------------------------------------------------------------*
*& Author: Paul Hardy
*& Source: https://github.com/abap2xlsx/abap2xlsx/issues/1097#issuecomment-1510516759
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_bug_1097.

START-OF-SELECTION.

  SELECT *
       FROM sflight
       INTO TABLE @DATA(lt_output_data)
       ORDER BY PRIMARY KEY.

  TRY.
      cl_salv_table=>factory(
        IMPORTING r_salv_table = DATA(lo_alv)
        CHANGING  t_table      = lt_output_data ).
    CATCH cx_salv_msg INTO DATA(lx_salv_msg).
      MESSAGE lx_salv_msg->get_text( ) TYPE 'I'.
  ENDTRY.

  DATA(lo_excel)     = NEW zcl_excel( ).
  DATA(lo_converter) = NEW zcl_excel_converter( ).

  lo_converter->convert(
            EXPORTING
              io_alv        = lo_alv
              it_table      = lt_output_data[]
              i_table       = abap_true "Create as Table
              i_style_table = zcl_excel_table=>builtinstyle_medium2
            CHANGING
              co_excel      = lo_excel ).
