*&---------------------------------------------------------------------*
*& Report zabo_test_no_timeout
*&---------------------------------------------------------------------*
*& Author: Matthew Billingham
*& Source: https://blogs.sap.com/2019/08/02/how-to-run-z-program-in-foreground-for-long-hours-without-time-out-dump/#comment-468805
*& Additional info: the progress indicator implicitly resets the timemout
*&---------------------------------------------------------------------*
REPORT zabo_test_no_timeout.

CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    METHODS:
      go.

  PRIVATE SECTION.
    types: ty_data type t100.
    CONSTANTS c_var_limit TYPE i VALUE '500'.   " Interval for timer reset
    DATA:
      var_total TYPE i,
      var_count TYPE i.
    METHODS check_and_reset_timer.
    METHODS indicate_progess
      IMPORTING
        i_var_perc TYPE i
        i_var_msg  TYPE csequence.

ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD go.
    DATA : tab_data TYPE STANDARD TABLE OF ty_data.

**Get the Total number of records
     var_total = lines( tab_data ).

    LOOP AT tab_data REFERENCE INTO DATA(data).
**Increment the counter
      var_count = var_count + 1.
*{
*Do somthing
*}

**Reset the timer at the interval
      check_and_reset_timer( ).
    ENDLOOP.
  ENDMETHOD.

  METHOD check_and_reset_timer.
    IF var_count MOD c_var_limit = 0.
      DATA(var_perc) = ( var_count * 100 ) / var_total.

** &1 of &2 Records Processed....
      MESSAGE i002(s1) WITH var_count var_total INTO DATA(var_msg).
      CONDENSE var_msg.

      indicate_progess( i_var_perc = var_perc i_var_msg = var_msg ).
    ENDIF.
  ENDMETHOD.

  METHOD indicate_progess.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = i_var_perc
        text       = i_var_msg
      EXCEPTIONS
        OTHERS     = 1.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW lcl_main( )->go( ).
