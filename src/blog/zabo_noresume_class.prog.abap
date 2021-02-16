*&---------------------------------------------------------------------*
*& Include zabo_noresume_class
*&---------------------------------------------------------------------*
*& Author: Sougata Chatterjee
*& Source: https://blogs.sap.com/2021/02/02/write-smart-abap-not-boring-abap-part-1/
*&---------------------------------------------------------------------*

CLASS lcl_employee DEFINITION FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF empl_data,
             empid  TYPE int4,          "Employee ID
             emptyp TYPE string,        "Org Assignment data
             salary TYPE decfloat16,    "Pay data
             phone  TYPE numc10,        "Communication data
           END OF empl_data,
           empl_data_t TYPE SORTED TABLE OF empl_data WITH UNIQUE KEY empid.

    METHODS constructor IMPORTING VALUE(i_empid)   TYPE int4.
    METHODS get_emptyp  RETURNING VALUE(r_result) TYPE string
                        RAISING   cx_no_data_found.
    METHODS get_salary  RETURNING VALUE(r_result) TYPE decfloat16
                        RAISING   cx_no_data_found.
    METHODS get_phone   RETURNING VALUE(r_result)  TYPE numc10.
    METHODS get_emp_id  RETURNING VALUE(r_result)  TYPE int4.
* NOTE: methods are all public, we need to call them separately!
  PRIVATE SECTION.
    DATA emp_id TYPE int4.

ENDCLASS.

CLASS lcl_employee IMPLEMENTATION.

  METHOD constructor.
    me->emp_id = i_empid.
  ENDMETHOD.

  METHOD get_emptyp.
    r_result = SWITCH #( me->get_emp_id( )
                WHEN 1 THEN |Full-Time|
                WHEN 2 THEN |Part-Time|
                WHEN 3 THEN |Contractor|
                WHEN 4 THEN |Casual|
                ELSE THROW cx_no_data_found(
                            rel_proc_id = CONV #( me->get_emp_id( ) ) )
              ).
  ENDMETHOD.

  METHOD get_phone.
    r_result = SWITCH #( me->get_emptyp( )
                WHEN `Full-Time` THEN |1234567890|
                WHEN `Part-Time` THEN |5678901234|
                WHEN `Casual`    THEN |7890123456|
                ELSE |0399999999|
              ).
  ENDMETHOD.

  METHOD get_salary.
    r_result = SWITCH #( me->get_emptyp( )
                 WHEN `Full-Time` THEN 50000
                 WHEN `Part-Time` THEN 25000
                 WHEN `Casual`    THEN 5000
                 ELSE THROW cx_no_data_found(
                            rel_proc_id = CONV #( me->get_emp_id( ) ) )
               ).
  ENDMETHOD.

  METHOD get_emp_id.
    r_result = me->emp_id.
  ENDMETHOD.

ENDCLASS.
