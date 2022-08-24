REPORT zabo_alv_singleclass.
*----------------------------------------------------------------------*
* Author: Raghu Govindarajan / Andrea Borgia
* Source: https://blogs.sap.com/2017/05/12/reference-a-programs-internal-table-from-alv-event-handler-2-methods/
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_routines DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_routines DEFINITION.

  PUBLIC SECTION.
    METHODS: extract_data, display_alv.

  PRIVATE SECTION.
*   Attributes to store references on your local variables
    DATA:
*     Reference on the internal table with data
      data_table TYPE STANDARD TABLE OF sflight WITH DEFAULT KEY,
*     ALV object
      alv        TYPE REF TO cl_salv_table.

    METHODS:
     on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.

ENDCLASS.                    "lcl_routines DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_routines IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_routines IMPLEMENTATION.
  METHOD extract_data.
    SELECT * FROM sflight
     UP TO 30 ROWS
     INTO CORRESPONDING FIELDS OF TABLE data_table.
  ENDMETHOD.


  METHOD display_alv.

    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = alv
          CHANGING
            t_table      = data_table.

        alv->set_screen_status(
              pfstatus      =  'STANDARD'
              report        =  sy-repid
              set_functions = alv->c_functions_none ).

        " Register event handler
        DATA(lo_events) = alv->get_event( ).
        SET HANDLER me->on_user_command FOR lo_events.

        DATA: lr_selections TYPE REF TO cl_salv_selections.
        lr_selections = alv->get_selections( ).
        lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

        alv->display( ).

      CATCH cx_salv_msg.             " cl_salv_table=>factory()
        "Ideally raise a message instead of WRITE statements
        WRITE: / 'cx_salv_msg exception'.
      CATCH cx_salv_not_found.       " cl_salv_columns_table->get_column()
        WRITE: / 'cx_salv_not_found exception'.
    ENDTRY.
  ENDMETHOD.    "display_alv


  METHOD on_user_command.

    DATA: lr_selections TYPE REF TO cl_salv_selections,
          l_row_info   TYPE char128,
          l_row        TYPE i,
          lt_rows   TYPE salv_t_row.

    CASE e_salv_function.
      WHEN '&DEL'.
        lr_selections = alv->get_selections( ).
        lt_rows = lr_selections->get_selected_rows( ).
        CLEAR l_row_info.
        LOOP AT lt_rows INTO l_row.
          l_row_info = |{ l_row_info } { l_row ALIGN = LEFT }|.
        ENDLOOP.
        IF sy-subrc EQ 0.
          MESSAGE i000(0k) WITH 'Rows' l_row_info.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.                    "lcl_routines IMPLEMENTATION

*----------------------------------------------------------------------*
*       START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  DATA(lcl) = NEW lcl_routines( ).
  lcl->extract_data( ).
  lcl->display_alv( ).
