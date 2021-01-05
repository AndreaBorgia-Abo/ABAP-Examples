*&---------------------------------------------------------------------*
*& Report zabo_segment_function
*&---------------------------------------------------------------------*
*& Author: Sooraj
*& Source: https://www.youtube.com/watch?v=yda87_t0KxY
*&---------------------------------------------------------------------*
REPORT zabo_segment_function.

DATA: lv_data   TYPE string VALUE `Sooraj,Sam,John,Jim `,
      lv_result TYPE string.

TRY.
    lv_result = segment( val = lv_data index = 5 sep = `,` ).
  CATCH cx_sy_strg_par_val.
    WRITE:/ `Not found.`.
ENDTRY.


DO.
  TRY.
      lv_result = segment( val = lv_data index = sy-index sep = `,` ).
    CATCH cx_sy_strg_par_val.
      EXIT.
  ENDTRY.
ENDDO.
