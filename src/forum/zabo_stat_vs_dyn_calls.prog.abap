*&---------------------------------------------------------------------*
*& Report ZABO_STAT_VS_DYN_CALLS
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://answers.sap.com/answers/13693152/view.html
*&---------------------------------------------------------------------*
REPORT zabo_stat_vs_dyn_calls.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS method
      IMPORTING
        object TYPE REF TO if_ixml.
ENDCLASS.
CLASS lcl_app IMPLEMENTATION.
  METHOD method.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA object TYPE REF TO object.
  DATA(ixml) = cl_ixml=>create( ).
  object = ixml.

  " static calling FAILS
  TRY.
*      lcl_app=>method( object = object ). "<==== doesn't compile
    CATCH cx_root INTO DATA(error).
      MESSAGE error TYPE 'I'.
  ENDTRY.

  " static calling SUCCEEDS
  lcl_app=>method( object = ixml ).


  " dynamic calling FAILS
  DATA(parameters) = VALUE abap_parmbind_tab(
      ( name  = 'OBJECT'
        kind  = 'E'
        value = REF #( object ) ) ).
  TRY.
      CALL METHOD ('LCL_APP')=>method PARAMETER-TABLE parameters. "<==== fails
    CATCH cx_root INTO error.
      MESSAGE error TYPE 'I'.
  ENDTRY.

  " dynamic calling SUCCEEDS
  parameters = VALUE abap_parmbind_tab(
      ( name  = 'OBJECT'
        kind  = 'E'
        value = REF #( ixml ) ) ).
  CALL METHOD ('LCL_APP')=>method PARAMETER-TABLE parameters.
