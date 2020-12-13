*&---------------------------------------------------------------------*
*& Report ZABO_FILTER_DEMO
*&---------------------------------------------------------------------*
*& Author: Oleg Bashkatov
*& Source: https://answers.sap.com/answers/13203500/view.html
*&---------------------------------------------------------------------*
REPORT zabo_filter_demo.

TYPES: BEGIN OF ts_line1
             , src_sys TYPE sysysid
             , field1 TYPE char10
             , field2 TYPE char10
         , END OF ts_line1
         , tt_line1 TYPE STANDARD TABLE OF ts_line1
           WITH NON-UNIQUE SORTED KEY pr COMPONENTS src_sys
         .

TYPES: BEGIN OF ts_line2
             , src_sys TYPE sysysid
         , END OF ts_line2
         .

DATA lt_itab1 TYPE tt_line1.
DATA lt_itab2 TYPE HASHED TABLE OF ts_line2
      WITH UNIQUE KEY src_sys.

BREAK-POINT.

lt_itab1 = VALUE #(  ( src_sys = 'ONE' field1 = 'F11' field2 = 'F21' )
                            ( src_sys = 'TWO' field1 = 'F12' field2 = 'F22' )
                            ( src_sys = 'THREE' field1 = 'F13' field2 = 'F23' )
                            ( src_sys = 'FOUR' field1 = 'F14' field2 = 'F24' ) ).

lt_itab2 = VALUE #( ( src_sys = 'TWO' ) ( src_sys = 'FOUR' ) ).


""" to delete records that are not in lt_itab2
DATA(lt_itab10) = FILTER #( lt_itab1 USING KEY pr IN lt_itab2  WHERE src_sys = src_sys ).


""" to save records that are not in lt_itab2
DATA(lt_itab11) = FILTER #(
   lt_itab1 USING KEY pr EXCEPT IN lt_itab2
   WHERE src_sys = src_sys
).
" lt_itab11  will store ONE / THREE but not TWO and FOUR


* Another option:
* Source: https://answers.sap.com/answers/13203497/view.html
DATA(lt_itab12) = FILTER #(
    lt_itab1 IN lt_itab2
    WHERE src_sys = src_sys ).


BREAK-POINT.
