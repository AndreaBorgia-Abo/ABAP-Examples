*&---------------------------------------------------------------------*
*& Report zabo_salv_tree_expcoll
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://answers.sap.com/answers/13453526/view.html
*& See also: https://blogs.sap.com/2021/08/06/refresh-of-cl_salv_tree/
*&---------------------------------------------------------------------*
REPORT zabo_salv_tree_expcoll.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    METHODS pbo
      RAISING
        cx_salv_error.
  PRIVATE SECTION.
    DATA: salv   TYPE REF TO cl_salv_tree,
          scarrs TYPE STANDARD TABLE OF scarr.
    METHODS on_double_click
        FOR EVENT if_salv_events_tree~double_click
        OF cl_salv_events_tree.
    METHODS get_salv_tree_gui_control
      IMPORTING
        container     TYPE REF TO cl_gui_container OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO cl_gui_column_tree.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD pbo.
    IF salv IS NOT BOUND.
      cl_salv_tree=>factory( EXPORTING r_container = cl_gui_container=>screen0
                             IMPORTING r_salv_tree = salv
                             CHANGING  t_table     = scarrs ).
      DATA(lo_settings) = salv->get_tree_settings( ). "MEMO: see https://answers.sap.com/comments/13452624/view.html
      lo_settings->set_hierarchy_size( 30 ).
      DATA(event) = salv->get_event( ).
      SET HANDLER on_double_click FOR event.
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
  METHOD on_double_click.
    DATA(gui_control) = get_salv_tree_gui_control(
    cl_gui_container=>screen0
    ).
    DATA(node_key_table) = VALUE treev_nks( ).
    gui_control->get_expanded_nodes( CHANGING node_key_table = node_key_table EXCEPTIONS OTHERS = 4 ).
    IF lines( node_key_table ) = 0.
      MESSAGE 'Nodes are all collapsed' TYPE 'I'.
    ELSEIF lines( node_key_table ) = 1.
      MESSAGE |Only this node is expanded: { CAST scarr-carrname( salv->get_nodes( )->get_node( node_key_table[ 1 ]
          )->get_item( 'CARRNAME' )->get_value( ) )->* }| TYPE 'I'.
    ELSE.
      MESSAGE |First 2 nodes to be expanded are: { CAST scarr-carrname( salv->get_nodes( )->get_node( node_key_table[ 1 ]
          )->get_item( 'CARRNAME' )->get_value( ) )->* } and { CAST scarr-carrname( salv->get_nodes( )->get_node( node_key_table[ 2 ]
          )->get_item( 'CARRNAME' )->get_value( ) )->* }| TYPE 'I'.
    ENDIF.
  ENDMETHOD.
  METHOD get_salv_tree_gui_control.
    DATA: splitter         TYPE REF TO cl_gui_splitter_container,
          simple_container TYPE REF TO cl_gui_simple_container,
          splitter_2       TYPE REF TO cl_gui_container,
          custom_container TYPE REF TO cl_gui_custom_container.
    " all this should be in a TRY-CATCH block because there's a lot of assumption...
    IF container IS BOUND.
      splitter = CAST #( container->children[ 1 ] ).
      splitter_2 = CAST #( splitter->children[ 2 ] ).
      result = CAST #( splitter_2->children[ 2 ] ).
    ELSE.
      custom_container = CAST #( cl_gui_container=>screen0->children[ 1 ] ).
      splitter = CAST #( custom_container->children[ 1 ] ).
      simple_container = CAST #( splitter->children[ 2 ] ).
      result = CAST #( simple_container->children[ 1 ] ).
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
