*&---------------------------------------------------------------------*
*& Report zabo_salv_refresh
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://answers.sap.com/comments/13419633/view.html
*&---------------------------------------------------------------------*
REPORT zabo_salv_refresh.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    METHODS main.
    METHODS on_double_click
                FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row.
    DATA salv TYPE REF TO cl_salv_table.
    DATA flights TYPE TABLE OF sflight.
ENDCLASS.
CLASS lcl_app IMPLEMENTATION.
  METHOD main.
    SELECT * FROM sflight INTO TABLE @flights.
    cl_salv_table=>factory(
      EXPORTING
        r_container  = cl_gui_container=>screen0
      IMPORTING
        r_salv_table = salv
      CHANGING
        t_table      = flights ).
    salv->get_functions( )->set_all( ).
    DATA lo_events TYPE REF TO cl_salv_events_table.
    lo_events = salv->get_event( ).
    SET HANDLER on_double_click FOR lo_events.
    salv->get_sorts( )->add_sort( columnname = 'CARRID' subtotal = 'X' ).
    salv->get_aggregations( )->add_aggregation( columnname = 'PRICE' ).
    salv->display( ).
  ENDMETHOD.
  METHOD on_double_click.
    DELETE flights INDEX row.
    salv->refresh( refresh_mode = if_salv_c_refresh=>full ).
  ENDMETHOD.
ENDCLASS.
PARAMETERS dummy.
DATA app TYPE REF TO lcl_app.

AT SELECTION-SCREEN OUTPUT.
  IF app IS NOT BOUND.
    app = NEW lcl_app( ).
    app->main( ).
  ENDIF.
