*&---------------------------------------------------------------------*
*& Report ZABO_ALVFILT_PSEUDOBUG_FULLSCR
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://answers.sap.com/comments/13477074/view.html
*&---------------------------------------------------------------------*
REPORT zabo_alvfilt_pseudobug_fullscr.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    METHODS start_of_selection.
    METHODS at_selection_screen_output.
    METHODS at_selection_screen
      IMPORTING
        ucomm TYPE sscrfields-ucomm.
    METHODS at_selection_screen_on_exit.
  PRIVATE SECTION.
    DATA: carriers TYPE STANDARD TABLE OF scarr,
          alv      TYPE REF TO cl_gui_alv_grid.
ENDCLASS.
CLASS lcl_app IMPLEMENTATION.
  METHOD start_of_selection.
    CALL SELECTION-SCREEN 1010.
  ENDMETHOD.
  METHOD at_selection_screen_output.
    SELECT * FROM scarr INTO TABLE @carriers.
    CASE sy-dynnr.
      WHEN 1020.
        alv = NEW cl_gui_alv_grid( i_parent = cl_gui_container=>screen0 ).
        alv->set_table_for_first_display(
            EXPORTING i_structure_name = 'SCARR'
            CHANGING  it_outtab = carriers ).
    ENDCASE.
  ENDMETHOD.
  METHOD at_selection_screen.
    CASE sy-dynnr.
      WHEN 1010.
        IF ucomm = 'VIEW_ALV'.
          CALL SELECTION-SCREEN 1020.
        ENDIF.
    ENDCASE.
  ENDMETHOD.
  METHOD at_selection_screen_on_exit.
    IF alv IS BOUND.
      alv->free( ).
      FREE alv.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

TABLES sscrfields.
SELECTION-SCREEN BEGIN OF SCREEN 1010.
SELECTION-SCREEN PUSHBUTTON /1(20) view_alv USER-COMMAND view_alv.
SELECTION-SCREEN END OF SCREEN 1010.
SELECTION-SCREEN BEGIN OF SCREEN 1020.
PARAMETERS dumm1020.
SELECTION-SCREEN END OF SCREEN 1020.

LOAD-OF-PROGRAM.
  DATA(app) = NEW lcl_app( ).
  view_alv = 'View ALV'.

AT SELECTION-SCREEN OUTPUT.
  app->at_selection_screen_output( ).

AT SELECTION-SCREEN.
  app->at_selection_screen( sscrfields-ucomm ).

AT SELECTION-SCREEN ON EXIT-COMMAND.
  app->at_selection_screen_on_exit( ).

START-OF-SELECTION.
  app->start_of_selection( ).
