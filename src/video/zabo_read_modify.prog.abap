*&---------------------------------------------------------------------*
*& Report ZABO_READ_MODIFY
*&---------------------------------------------------------------------*
*& Author: Sooraj
*& Source: https://www.youtube.com/watch?v=9Em1Scdh77A
*&---------------------------------------------------------------------*
REPORT zabo_read_modify.


TYPES: BEGIN OF ty_data,
         name TYPE char20,
         role TYPE char20,
       END OF ty_data.


DATA: it_data TYPE STANDARD TABLE OF ty_data.


it_data = VALUE #( ( name = `sooraj` role = `abap` )
                   ( name = `sam` role = `java` )
                   ( name = `sachin` role = `python` ) ).
cl_demo_output=>display( it_data ) .


DATA(lv_index) = line_index( it_data[ name = `sam` ] ).
WRITE:/ |Index:  { lv_index } |.

* First option: handle the exception
TRY.
    it_data[ name = `John` ]-role = `python`.
  CATCH  cx_sy_itab_line_not_found.
    WRITE:/ `Not found`.
ENDTRY.

* Second option: test presence
IF line_exists( it_data[ name = `sam` ] ).
  WRITE:/ `Value exists`.
  it_data[ name = `sam` ]-role = `python`.
ELSE.
  WRITE:/ `Value does NOT exist`.
ENDIF.

cl_demo_output=>display( it_data ) .
