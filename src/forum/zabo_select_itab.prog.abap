*&---------------------------------------------------------------------*
*& Report zabo_select_itab
*&---------------------------------------------------------------------*
*& Author: Jörgen Lindqvist
*& Source: https://answers.sap.com/answers/13355510/view.html
*& Docs: https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapselect_itab.htm
*&---------------------------------------------------------------------*
REPORT zabo_select_itab.


TYPES:
  BEGIN OF ty_struct,
    id     TYPE i,
    field1 TYPE string,
    field2 TYPE string,
  END OF ty_struct.
TYPES ty_table TYPE STANDARD TABLE OF ty_struct WITH EMPTY KEY.

DATA(itab) = VALUE ty_table( ( id = 1 field1 = '111' field2 = 'aaa' )
                             ( id = 2 field1 = '222' field2 = 'bbb' )
                             ( id = 3 field1 = '333' field2 = 'ccc' )
                             ( id = 4 field1 = '444' field2 = 'ddd' ) ).

SELECT SINGLE @abap_true FROM @itab AS itab WHERE id > 2 INTO @DATA(result).
BREAK-POINT.
