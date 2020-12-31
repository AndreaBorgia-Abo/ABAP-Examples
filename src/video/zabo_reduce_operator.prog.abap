*&---------------------------------------------------------------------*
*& Report ZABO_REDUCE_OPERATOR
*&---------------------------------------------------------------------*
*& Author: Sooraj
*& Source: https://www.youtube.com/watch?v=1TZnZ21mpSo
*&---------------------------------------------------------------------*
REPORT zabo_reduce_operator.

DATA(v1) = REDUCE i( INIT sum = 0 FOR i = 1 THEN i + 1 UNTIL i > 10 NEXT sum = sum + i ).
cl_demo_output=>write( v1 ).

* Memo: THEN may be omitted: defaults to increment by 1.
DATA(v2) = REDUCE string( INIT text = `Count up: ` FOR i = 1 UNTIL i > 10 NEXT text = text && | { i } | ).
cl_demo_output=>write( v2 ).

DATA(v3) = REDUCE string( INIT text = `Count down: ` FOR i = 10 THEN i - 1 UNTIL i = 0 NEXT text = text && | { i } | ).
cl_demo_output=>write( v3 ).


TYPES: BEGIN OF ty_student,
         name    TYPE char20,
         subject TYPE char20,
         marks   TYPE i,
       END OF ty_student.

DATA: it_student TYPE TABLE OF ty_student.

it_student = VALUE #( FOR i = 1 UNTIL i > 10 (
  name = 'Student' && i
  subject = 'Physics'
  marks = i
) ).

APPEND VALUE #(
  name = 'Student10'
  subject = 'Maths'
  marks = 20
) TO it_student.

DATA(marks_sum) = REDUCE i(
  INIT total = 0
  FOR wa IN it_student WHERE ( name = 'Student10' )
  NEXT total = total + wa-marks
).

cl_demo_output=>write( it_student ).
cl_demo_output=>display( marks_sum ).
