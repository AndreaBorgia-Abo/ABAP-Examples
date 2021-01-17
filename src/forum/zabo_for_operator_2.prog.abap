*&---------------------------------------------------------------------*
*& Report ZABO_FOR_OPERATOR_2
*&---------------------------------------------------------------------*
*& Author: Mateusz Adamus
*& Source: https://answers.sap.com/answers/13222379/view.html
*&---------------------------------------------------------------------*
REPORT ZABO_FOR_OPERATOR_2.

CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      test
        IMPORTING iv_string        TYPE string
        RETURNING VALUE(rv_string) TYPE string.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD test.
    rv_string = |NEW: { iv_string }|.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  TYPES:
    BEGIN OF yls_string,
      value TYPE string,
    END OF yls_string,
    ylt_strings TYPE SORTED TABLE OF yls_string WITH NON-UNIQUE KEY value.

  DATA:
    ls_string TYPE yls_string,
    lt_strings TYPE ylt_strings.

  ls_string-value = |String 1|.
  APPEND ls_string TO lt_strings.
  ls_string-value = |String 2|.
  APPEND ls_string TO lt_strings.

  WRITE / 'Old'.
  LOOP AT lt_strings INTO ls_string.
    WRITE / ls_string-value.
  ENDLOOP.

  DATA(lt_results) = VALUE ylt_strings(
    FOR ls_string_tmp IN lt_strings
    (
      value = lcl_class=>test( ls_string_tmp-value )
    )
  ).

  WRITE / 'New'.
  LOOP AT lt_results INTO ls_string.
    WRITE / ls_string-value.
  ENDLOOP.
