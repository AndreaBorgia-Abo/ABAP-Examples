*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_PR_842
*&---------------------------------------------------------------------*
*& Author: Andrea Borgia
*& Refers to: https://github.com/sapmentors/abap2xlsx/pull/842
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_pr_842.

DATA: media_source TYPE c.


CLASS lc_test DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS testchar
      IMPORTING input TYPE c.
ENDCLASS.

CLASS lc_test IMPLEMENTATION.
  METHOD testchar.
    cl_demo_output=>write( |Input: "{ input }" of length { strlen( input ) }| ).
  ENDMETHOD.
ENDCLASS.


START-OF-SELECTION.
  lc_test=>testchar( cl_abap_conv_in_ce=>uccp( 'FFFD' ) ).
  lc_test=>testchar( `Longer text` ).

  media_source = `Longer text`.
  lc_test=>testchar( media_source ).

  cl_demo_output=>display( ).
