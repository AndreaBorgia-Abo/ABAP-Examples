*&---------------------------------------------------------------------*
*& Report zabo_find_unittest
*&---------------------------------------------------------------------*
*& Author: Enno Wulff
*& Source: https://blogs.sap.com/2021/04/29/do-unit-tests-exist/
*& See also:
*&  https://github.com/larshp/abapOpenChecks/blob/main/src/utils/zaoc_count_classes_with_tests.prog.abap
*&  https://help.sap.com/viewer/ba879a6e2ea04d9bb94c7ccd7cdac446/7.5.9/en-US/4b068514217447e298868d3a6fd1041f.html
*&---------------------------------------------------------------------*
REPORT zabo_find_unittest.

TYPES: BEGIN OF _test_class,
         classname TYPE seoclsname,
         testclass TYPE seoclsname,
         unit_test TYPE seocpdname,
       END OF _test_class,
       _test_classes TYPE STANDARD TABLE OF _test_class WITH EMPTY KEY.
DATA test_classes TYPE _test_classes.
DATA test_class TYPE _test_class.

SELECT-OPTIONS clsnam FOR test_class-classname DEFAULT 'CL_AUNIT*' OPTION CP OBLIGATORY.

START-OF-SELECTION.

  "read classes to check
  SELECT * FROM seoclass
    INTO TABLE @DATA(clt)
    WHERE clsname IN @clsnam.

  LOOP AT clt INTO DATA(cls).
    test_class-classname = cls-clsname.

    "get test classes and their unit tests
    DATA(hlp) = NEW cl_aunit_factory( ).
    DATA(res) = hlp->get_test_class_handles(
      EXPORTING
        obj_type = 'CLAS'
        obj_name = CONV #( cls-clsname ) ).

    LOOP AT res INTO DATA(tcl).
      test_class-testclass = tcl->get_class_name( ).
      LOOP AT tcl->get_test_methods( ) INTO test_class-unit_test.
        APPEND test_class TO test_classes.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.


  TRY.
      "display found classes
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table   = DATA(grid)              " Basis Class Simple ALV Tables
        CHANGING
          t_table        = test_classes ).
      grid->get_functions( )->set_all( ).
      grid->display( ).
    CATCH cx_salv_msg INTO DATA(msg).
      MESSAGE msg TYPE 'I'.
  ENDTRY.
