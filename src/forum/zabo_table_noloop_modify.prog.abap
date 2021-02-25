*&---------------------------------------------------------------------*
*& Report zabo_table_noloop_modify
*&---------------------------------------------------------------------*
*& Author: Jörgen Lindqvist
*& Source: https://answers.sap.com/answers/13291404/view.html
*&---------------------------------------------------------------------*
REPORT zabo_table_noloop_modify.

TYPES:
  BEGIN OF ty_one_column,
    column TYPE i,
  END OF ty_one_column.

TYPES:
  BEGIN OF ty_three_columns,
    column   TYPE i,
    column_2 TYPE string,
    column_3 TYPE string,
  END OF ty_three_columns.

DATA one_column_table TYPE STANDARD TABLE OF ty_one_column WITH EMPTY KEY.
DATA three_column_table TYPE STANDARD TABLE OF ty_three_columns WITH EMPTY KEY.

DO 5 TIMES.
  INSERT VALUE #( column = sy-index ) INTO TABLE one_column_table.
ENDDO.

BREAK-POINT.

three_column_table = CORRESPONDING #( one_column_table ).
MODIFY three_column_table
  FROM VALUE #( column_2 = 'x'
                column_3 = 'y' )
  TRANSPORTING column_2
               column_3
  WHERE column IS NOT INITIAL.

BREAK-POINT.
