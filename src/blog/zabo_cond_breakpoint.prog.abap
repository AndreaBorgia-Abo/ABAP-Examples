*&---------------------------------------------------------------------*
*& Report ZABO_COND_BREAKPOINT
*&---------------------------------------------------------------------*
*& Author: Andrea Borgia
*& Class Author: Uladzislau Pralat
*& Source: https://blogs.sap.com/2015/05/25/use-checkpoint-group-to-debug-abap-code-called-in-background-or-remotelly/
*&         https://blogs.sap.com/2014/08/28/break-point-id-assert-id-and-log-point-id-easy-to-use-and-very-handy/
*&---------------------------------------------------------------------*
REPORT ZABO_COND_BREAKPOINT.

TYPES log_text TYPE c LENGTH 40.

* Unconditional
write: / 'Before BREAK'.
DATA(log_text) = CONV log_text( |in program { sy-repid }| ).
BREAK-POINT log_text.
write: / 'After BREAK'.

* Conditional
DATA : lv_iteration TYPE i.
DO 20 TIMES.
  zcl_aab=>break_point( 'ZABO_DEMO' ).
  lv_iteration = lv_iteration + 1.
  ASSERT ID ZABO_DEMO CONDITION lv_iteration < 20.
ENDDO.
