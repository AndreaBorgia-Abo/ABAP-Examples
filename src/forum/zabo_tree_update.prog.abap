*&---------------------------------------------------------------------*
*& Report zabo_tree_update
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://answers.sap.com/comments/13286259/view.html
*& See also: BCALV_TREE_04 (CL_GUI_ALV_TREE insteaf of CL_SALV_TREE)
*&---------------------------------------------------------------------*
REPORT zabo_tree_update.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    METHODS pbo
      RAISING
        cx_salv_error.
  PRIVATE SECTION.
    DATA: salv   TYPE REF TO cl_salv_tree,
          scarrs TYPE STANDARD TABLE OF scarr.
    METHODS on_added_function
      FOR EVENT added_function
                OF cl_salv_events_tree
      IMPORTING e_salv_function.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD pbo.
    IF salv IS NOT BOUND.
      cl_salv_tree=>factory( EXPORTING r_container = cl_gui_container=>screen0
                             IMPORTING r_salv_tree = salv
                             CHANGING  t_table     = scarrs ).
      DATA(lo_settings) = salv->get_tree_settings( ).
      lo_settings->set_hierarchy_size( 30 ).
      DATA(event) = salv->get_event( ).
      salv->get_functions( )->add_function( name = 'HIDE_FIRST_ROW' text = 'HIDE_FIRST_ROW' tooltip = '' position = 1 ).
      SET HANDLER on_added_function FOR event.
      SELECT * FROM scarr INTO TABLE @DATA(local_scarrs).
      LOOP AT local_scarrs REFERENCE INTO DATA(scarr).
        DATA(node) = salv->get_nodes( )->add_node(
          related_node = space " (root node)
          relationship = cl_gui_column_tree=>relat_last_child
          text         = |{ scarr->carrid } - { scarr->carrname }|
          data_row     = scarr->*
          folder       = abap_true ).
        salv->get_nodes( )->add_node(
          related_node = node->get_key( )
          relationship = cl_gui_column_tree=>relat_last_child
          text         = |test|
          folder       = abap_false ).
      ENDLOOP.
      salv->display( ).
    ENDIF.
    LOOP AT SCREEN.
      screen-active = '0'.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.
  METHOD on_added_function.
    IF e_salv_function = 'HIDE_FIRST_ROW'.
      salv->get_nodes( )->get_top_node( )->set_visible( value = abap_false )."expand( complete_subtree = abap_true ).
      salv->close_screen( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

PARAMETERS dummy.

LOAD-OF-PROGRAM.
  DATA(app) = NEW lcl_app( ).

AT SELECTION-SCREEN OUTPUT.
  TRY.
      app->pbo( ).
    CATCH cx_root INTO DATA(lx).
      MESSAGE lx TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
