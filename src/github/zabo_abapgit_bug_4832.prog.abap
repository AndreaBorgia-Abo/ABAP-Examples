*&---------------------------------------------------------------------*
*& Report ZABO_ABAPGIT_BUG_4832
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/abapGit/abapGit/issues/4832#issuecomment-903625964
*&---------------------------------------------------------------------*
REPORT ZABO_ABAPGIT_BUG_4832.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    METHODS at_selection_screen_output.
    METHODS at_selection_screen_exit.
    METHODS on_sapevent FOR EVENT sapevent OF cl_gui_html_viewer
          IMPORTING action frame getdata postdata query_table.
  PRIVATE SECTION.
    DATA o_html TYPE REF TO cl_gui_html_viewer.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD at_selection_screen_output.
    DATA: l_url  TYPE cndp_url,
          l_text TYPE string.

    IF o_html IS NOT BOUND.

      CONCATENATE '<html><head></head><body>'
      '<form id="form" method="POST" action="SAPEVENT:submit">'
      '<p>Text : <input name="text" type="text" length="50" value="'
      '1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890'
      '1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890'
* Triggers bug 4832 with 2 lines, while 3 lines are ok!
*      '1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890'
      '"></p>'
      '<p>Checkbox : <input name="checkbox" type="checkbox"></p><p><button type= "submit">ABAP call</button></p>'
      '</form>'
      '</body></html>'
      INTO l_text.

      o_html = new #( parent = cl_gui_container=>screen0 ).
      SET HANDLER on_sapevent FOR o_html.
      o_html->set_registered_events( events = value #( ( eventid = o_html->m_id_sapevent ) ) ).
      DATA(lt_text) = cl_bcs_convert=>string_to_soli( l_text ).
      o_html->load_data(
        EXPORTING type = 'text' subtype = 'html' size = strlen( l_text )
        IMPORTING assigned_url = l_url
        CHANGING  data_table = lt_text ).
      o_html->show_url( EXPORTING url = l_url ).
    ENDIF.
  ENDMETHOD.

  METHOD at_selection_screen_exit.
    IF o_html IS BOUND.
      o_html->free( ).
      FREE o_html.
    ENDIF.
  ENDMETHOD.

  METHOD on_sapevent.
    CONCATENATE LINES OF postdata INTO data(l_post_data) RESPECTING BLANKS.
    MESSAGE l_post_data TYPE 'I'.
  ENDMETHOD.

ENDCLASS.

PARAMETERS dummy.
DATA go_app TYPE REF TO lcl_app.

LOAD-OF-PROGRAM.
  CREATE OBJECT go_app.

AT SELECTION-SCREEN OUTPUT.
  go_app->at_selection_screen_output( ).

AT SELECTION-SCREEN ON EXIT-COMMAND.
  go_app->at_selection_screen_exit( ).
