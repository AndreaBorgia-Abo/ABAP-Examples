*&---------------------------------------------------------------------*
*& Report ZABO_ALV_COND_HIDE_CELLS
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://answers.sap.com/answers/13967488/view.html
*&---------------------------------------------------------------------*
REPORT zabo_alv_cond_hide_cells.

INCLUDE <cl_alv_control>.
PARAMETERS dummy.
DATA alv_grid TYPE REF TO cl_gui_alv_grid.
TYPES: BEGIN OF ty_sflight.
         INCLUDE TYPE sflight.
TYPES: t_style TYPE lvc_t_styl,
       END OF ty_sflight.
TYPES tt_sflight TYPE STANDARD TABLE OF ty_sflight WITH EMPTY KEY.
DATA flights TYPE tt_sflight.

AT SELECTION-SCREEN OUTPUT.
  IF alv_grid IS NOT BOUND.
    SELECT * FROM sflight INTO CORRESPONDING FIELDS OF TABLE @flights ORDER BY fldate.
    LOOP AT flights REFERENCE INTO DATA(flight)
        WHERE price < 650.
      flight->t_style = VALUE #( FOR <fieldname> IN VALUE string_table( ( `SEATSMAX` ) ( `SEATSOCC` )
                    ( `PAYMENTSUM` ) ( `SEATSMAX_B` ) ( `SEATSOCC_B` ) ( `SEATSMAX_F` ) ( `SEATSOCC_F` ) )
            ( fieldname = <fieldname>
              style     = alv_style_checkbox_no ) ).
    ENDLOOP.
    alv_grid = NEW cl_gui_alv_grid( i_parent = cl_gui_container=>screen0 ).
    alv_grid->set_table_for_first_display(
            EXPORTING  i_structure_name = 'SFLIGHT'
                       is_layout        = VALUE #( cwidth_opt = 'X' stylefname = 'T_STYLE' )
            CHANGING   it_outtab        = flights
            EXCEPTIONS OTHERS = 1 ).
  ENDIF.
