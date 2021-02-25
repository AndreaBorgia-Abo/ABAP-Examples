*&---------------------------------------------------------------------*
*& Report zabo_table_noloop_class
*&---------------------------------------------------------------------*
*& Author: Yunus Tuzun
*& Source: https://answers.sap.com/answers/13291485/view.html
*&---------------------------------------------------------------------*
REPORT zabo_table_noloop_class.

TYPES:
  BEGIN OF ty_one_column,
    column1 TYPE i,
  END OF ty_one_column.

TYPES:
  BEGIN OF ty_three_columns,
    column1 TYPE i,
    column2 TYPE string,
    column3 TYPE string,
  END OF ty_three_columns.

DATA one_column_table TYPE STANDARD TABLE OF ty_one_column WITH EMPTY KEY.
DATA three_column_table TYPE STANDARD TABLE OF ty_three_columns WITH EMPTY KEY.

DO 5 TIMES.
  INSERT VALUE #( column1 = sy-index ) INTO TABLE one_column_table.
ENDDO.

BREAK-POINT.

three_column_table = CORRESPONDING #( one_column_table ).
cl_abap_corresponding=>create_with_value(
  source            = one_column_table
  destination       = three_column_table
  mapping           = VALUE  cl_abap_corresponding=>mapping_table_value(
      ( level = 0 kind = cl_abap_corresponding=>mapping_component   srcname = 'column1' dstname = 'column1'  )
      ( level = 0 kind = cl_abap_corresponding=>mapping_value dstname = 'column2' value = REF #( `X` ) )
      ( level = 0 kind = cl_abap_corresponding=>mapping_value dstname = 'column3' value = REF #( `Y` ) ) )
   )->execute( EXPORTING source      = one_column_table
                CHANGING  destination = three_column_table ).

BREAK-POINT.
