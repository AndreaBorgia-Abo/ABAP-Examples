*&---------------------------------------------------------------------*
*& Report ZABO_FOR_OPERATOR
*&---------------------------------------------------------------------*
*& Author: Sooraj
*& Source: https://youtu.be/WPNcz7nXQQg
*&---------------------------------------------------------------------*
REPORT zabo_for_operator.


TYPES: BEGIN OF ty_data,
         c1 TYPE i,
         c2 TYPE i,
         c3 TYPE i,
         c4 TYPE i,
       END OF ty_data,
       BEGIN OF ty_data4,
         c1 TYPE i,
         c2 TYPE i,
       END OF ty_data4,
       tt_i     TYPE TABLE OF i WITH EMPTY KEY,
       tt_data  TYPE TABLE OF ty_data WITH EMPTY KEY,
       tt_data4 TYPE TABLE OF ty_data4 WITH EMPTY KEY.
* Memo: "empty key" to keep "VALUE" happy


DATA(it_data) = VALUE tt_data( FOR i = 10 THEN i + 10 UNTIL i > 50
   ( c1 = i c2 = i + 1 c3 = i + 2 c4 = i + 3 )
).
cl_demo_output=>write( it_data ).

DATA(it_data2) = VALUE tt_data(
FOR wa IN it_data WHERE ( c1 > 30 ) ( wa )
).
cl_demo_output=>write( it_data2 ).

DATA: it_data22 TYPE TABLE OF i.
it_data22 = VALUE #(
  FOR wa IN it_data WHERE ( c1 > 30 )
  ( wa-c1 )
).
cl_demo_output=>write( it_data22 ).

DATA(it_data3) = VALUE tt_data(
FOR wa IN it_data INDEX INTO lv_index WHERE ( c1 = 20 )
( LINES OF it_data FROM lv_index )
).
cl_demo_output=>write( it_data3 ).

DATA(it_data4) = VALUE tt_data4(
FOR wa IN it_data FROM 2 TO 5
( c1 = wa-c1 c2 = wa-c2 )
).
cl_demo_output=>write( it_data4 ).

DATA(it_data5) = VALUE tt_i(
  FOR wa IN it_data ( wa-c1 ) ( wa-c2 )
).
cl_demo_output=>display( it_data5 ).
