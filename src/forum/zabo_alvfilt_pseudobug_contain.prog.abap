*&---------------------------------------------------------------------*
*& Report ZABO_ALVFILT_PSEUDOBUG_CONTAIN
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://answers.sap.com/comments/13478237/view.html
*&---------------------------------------------------------------------*
REPORT zabo_alvfilt_pseudobug_contain.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    METHODS start_of_selection.
    METHODS at_selection_screen
      IMPORTING
        ucomm TYPE sscrfields-ucomm.
    METHODS screen_0100_pbo.
    METHODS screen_0100_pai.
  PRIVATE SECTION.
    DATA: carriers         TYPE STANDARD TABLE OF scarr,
          alv              TYPE REF TO cl_gui_alv_grid,
          custom_container TYPE REF TO cl_gui_custom_container.
ENDCLASS.
CLASS lcl_app IMPLEMENTATION.
  METHOD start_of_selection.
    CALL SELECTION-SCREEN 1010.
  ENDMETHOD.
  METHOD screen_0100_pbo.
    IF alv IS NOT BOUND.
      SELECT * FROM scarr INTO TABLE @carriers.
      custom_container = NEW cl_gui_custom_container( container_name = 'CONTAINER_DOC' ).
      alv = NEW cl_gui_alv_grid( i_parent = custom_container ).
      alv->set_table_for_first_display(
          EXPORTING i_structure_name = 'SCARR'
          CHANGING  it_outtab = carriers ).
    ENDIF.
    SET PF-STATUS space.
  ENDMETHOD.
  METHOD screen_0100_pai.
    IF sy-ucomm = 'BACK'.
* Without the following line, setting a filter in the ALV, navigating back one screen
* and then re-entering the ALV results in the filter being still present: this is NOT a bug!
******alv->free( ). FREE alv. custom_container->free( ). FREE custom_container.
      SET SCREEN 0.
    ENDIF.
  ENDMETHOD.
  METHOD at_selection_screen.
    CASE sy-dynnr.
      WHEN 1010.
        IF ucomm = 'VIEW_ALV'.
          CALL SCREEN 100.
        ENDIF.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

TABLES sscrfields.
SELECTION-SCREEN BEGIN OF SCREEN 1010.
SELECTION-SCREEN PUSHBUTTON /1(20) view_alv USER-COMMAND view_alv.
SELECTION-SCREEN END OF SCREEN 1010.

LOAD-OF-PROGRAM.
  DATA(app) = NEW lcl_app( ).
  view_alv = 'View ALV'.

AT SELECTION-SCREEN.
  app->at_selection_screen( sscrfields-ucomm ).

START-OF-SELECTION.
  app->start_of_selection( ).
  ASSERT 1 = 1. " debug helper

MODULE status_0100 OUTPUT.
  app->screen_0100_pbo( ).
ENDMODULE.
MODULE user_command_0100 INPUT.
  app->screen_0100_pai( ).
ENDMODULE.
