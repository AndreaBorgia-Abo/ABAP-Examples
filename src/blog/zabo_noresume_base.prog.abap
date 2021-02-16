*&---------------------------------------------------------------------*
*& Report zabo_noresume_base
*&---------------------------------------------------------------------*
*& Author: Sougata Chatterjee
*& Source: https://blogs.sap.com/2021/02/02/write-smart-abap-not-boring-abap-part-1/
*&---------------------------------------------------------------------*
REPORT zabo_noresume_base.

INCLUDE zabo_noresume_class.

DATA extract_t TYPE lcl_employee=>empl_data_t.
DATA error_t   TYPE string_table.

START-OF-SELECTION.

  DATA(all_employees_t) = VALUE int4_table(  ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ).

  LOOP AT all_employees_t REFERENCE INTO DATA(dref).

    DATA(ref) = NEW lcl_employee( dref->* ).

    TRY.
        DATA(extract) = VALUE lcl_employee=>empl_data(
                          empid = ref->get_emp_id( )
                          emptyp = ref->get_emptyp( )
                          phone  = ref->get_phone( )
                        ).

      CATCH cx_no_data_found INTO DATA(lx_no_data).
        error_t = VALUE #( BASE error_t ( lx_no_data->get_text( ) ) ).
        CONTINUE.
    ENDTRY.

    "Note that the call to the method GET_SALARY( ) is separated
    TRY.
        extract = VALUE #( BASE extract
                     salary = ref->get_salary( )
                   ).
      CATCH cx_no_data_found INTO lx_no_data.
    ENDTRY.

    INSERT extract INTO TABLE extract_t.
  ENDLOOP.

  cl_demo_output=>new( )->write( extract_t )->write( error_t )->display( ).
