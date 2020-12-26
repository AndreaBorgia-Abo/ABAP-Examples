*&---------------------------------------------------------------------*
*& Report ZABO_OOP_SUBMIT_CALLED
*&---------------------------------------------------------------------*
*& Author: Andrea Borgia
*& Note: started from ZABO_OOP_SUBMIT_CALLING
*&---------------------------------------------------------------------*
REPORT zabo_oop_submit_called.


DATA: gr_table TYPE REF TO cl_salv_table,
      t_square TYPE STANDARD TABLE OF zabo_submit_demo.


SELECTION-SCREEN BEGIN OF BLOCK bl1.
PARAMETERS: p_iter TYPE int4.
SELECTION-SCREEN END OF BLOCK bl1.



START-OF-SELECTION.

  DO p_iter TIMES.
    APPEND VALUE #( index = sy-index square = ipow( base = sy-index exp = 2 ) ) TO t_square.
  ENDDO.

  IF t_square[] IS NOT INITIAL.
    cl_salv_table=>factory(
      IMPORTING r_salv_table = gr_table
      CHANGING  t_table   = t_square ).
    gr_table->display( ).
  ENDIF.
