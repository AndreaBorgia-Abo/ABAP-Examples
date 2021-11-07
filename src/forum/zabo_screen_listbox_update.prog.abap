*&---------------------------------------------------------------------*
*& Report ZABO_SCREEN_LISTBOX_UPDATE
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://answers.sap.com/comments/13520346/view.html
*&---------------------------------------------------------------------*
REPORT zabo_screen_listbox_update.

PARAMETERS country(3) TYPE c AS LISTBOX VISIBLE LENGTH 20.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    METHODS main.
    METHODS on_double_click
      FOR EVENT double_click
                OF cl_salv_events_table
      IMPORTING row.
    DATA salv TYPE REF TO cl_salv_table.
    DATA sflight_s TYPE TABLE OF sflight.
    DATA go_docking TYPE REF TO cl_gui_docking_container.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD main.

    SELECT * FROM sflight INTO TABLE @sflight_s.

    CREATE OBJECT go_docking
      EXPORTING
        repid     = sy-repid
        dynnr     = sy-dynnr
        side      = cl_gui_docking_container=>dock_at_right
        extension = 400.

    cl_salv_table=>factory(
      EXPORTING
        r_container  = go_docking
      IMPORTING
        r_salv_table = salv
      CHANGING
        t_table      = sflight_s ).

    salv->get_functions( )->set_all( ).

    DATA lo_events TYPE REF TO cl_salv_events_table.
    lo_events = salv->get_event( ).
    SET HANDLER on_double_click FOR lo_events.

    salv->display( ).
  ENDMETHOD.
  METHOD on_double_click.
    cl_gui_cfw=>set_new_ok_code( '=00' ).
  ENDMETHOD.
ENDCLASS.

INITIALIZATION.
  DATA app TYPE REF TO lcl_app.

AT SELECTION-SCREEN OUTPUT.
  IF app IS NOT BOUND.
    app = NEW lcl_app( ).
    app->main( ).
    DATA: lt_value TYPE vrm_values,
          ls_value TYPE vrm_value.
    ls_value-key = 'FRA'.
    ls_value-text = 'France'.
    APPEND ls_value TO lt_value.
    ls_value-key = 'GER'.
    ls_value-text = 'Allemagne'.
    APPEND ls_value TO lt_value.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = 'COUNTRY'
        values          = lt_value
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.
    country = 'FRA'.
  ELSE.
    country = 'GER'.
  ENDIF.
