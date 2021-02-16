*&---------------------------------------------------------------------*
*& Report zabo_resume_base
*&---------------------------------------------------------------------*
*& Author: Sougata Chatterjee
*& Source: https://blogs.sap.com/2021/02/02/write-smart-abap-not-boring-abap-part-1/
*&---------------------------------------------------------------------*
REPORT zabo_resume_base.

INCLUDE zabo_resume_class.

DATA extract_t TYPE lcl_employee=>empl_data_t.
DATA error_t   TYPE string_table.

START-OF-SELECTION.

  DATA(all_employees_t) = VALUE int4_table(  ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ).

  LOOP AT all_employees_t REFERENCE INTO DATA(dref).
    TRY.
        INSERT NEW lcl_employee( dref->* )->get_data( ) INTO TABLE extract_t.

      CATCH BEFORE UNWIND cx_no_data_found INTO DATA(lx_no_data).
        IF lx_no_data->is_resumable = abap_true.
          "Resumable Exception was raised
          RESUME.
        ELSE.
          "Non-Resumable Exception was raised
          error_t = VALUE #( BASE error_t ( lx_no_data->get_text( ) ) ).
        ENDIF.
    ENDTRY.
  ENDLOOP.

  cl_demo_output=>new( )->write( extract_t )->write( error_t )->display( ).
