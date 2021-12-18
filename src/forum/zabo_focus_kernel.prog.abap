*&---------------------------------------------------------------------*
*& Report ZABO_FOCUS_KERNEL
*&---------------------------------------------------------------------*
*& Author: Clemens Li
*& Source: https://answers.sap.com/answers/13548278/view.html
*&---------------------------------------------------------------------*
REPORT zabo_focus_kernel.

DATA gt_t100 TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
CLASS zcl_bc_dialog DEFINITION
  CREATE PUBLIC .
  PUBLIC SECTION.
    CLASS-METHODS display_string
      IMPORTING
        !width     TYPE int4 DEFAULT 800
        !height    TYPE int4 DEFAULT 500
        !top       TYPE int4 DEFAULT 10
        !left      TYPE int4 DEFAULT 50
        !iv_string TYPE string .
    CLASS-METHODS handle_link_click FOR EVENT link_click OF cl_salv_events_table
      IMPORTING row column.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA mo_textedit TYPE REF TO cl_gui_textedit .
    CLASS-DATA mo_container TYPE REF TO cl_gui_dialogbox_container .
    CLASS-METHODS close
        FOR EVENT close OF cl_gui_dialogbox_container .
ENDCLASS.

CLASS zcl_bc_dialog IMPLEMENTATION.
  METHOD close.
    IF mo_textedit IS BOUND.
      mo_textedit->free( ).
      mo_container->free( ).
      FREE:
        mo_textedit,
        mo_container.
    ENDIF.
  ENDMETHOD.

  METHOD display_string.
    close( ).
    mo_container =
      NEW #(
        width  = width
        height = height
        top    = top
        left   = left   ).
    SET HANDLER close FOR mo_container.
    mo_textedit = NEW #(
      parent = mo_container ).
    mo_textedit->set_readonly_mode( 1 ).
    mo_textedit->set_textstream( iv_string ).
    cl_gui_control=>set_focus( mo_textedit )."also tried mo_container

* Author: Chaouki Akir
* Source: https://answers.sap.com/comments/13545485/view.html
* With: focus moves to text window when clicking hotspot
* Without: focus stays in ALV unless popup is clicked
* Set final focus (suppress other set focus commands)
    CALL 'DYNP_SET_STATUS' ID 'FUNCTION' FIELD 14
                           ID 'VALUE'    FIELD 1.         "#EC CI_CCALL
* Author: Sandra Rossi
* Source: https://answers.sap.com/comments/13547386/view.html
* 434257 - Performance Assistant in the background: final focus
* https://launchpad.support.sap.com/#/notes/434257/E

  ENDMETHOD.

  METHOD handle_link_click.
* whatever is clicked - just display something
    DATA lv_string TYPE string.
    LOOP AT gt_t100 ASSIGNING FIELD-SYMBOL(<t100>).
      lv_string = lv_string
        && <t100>-sprsl && cl_abap_char_utilities=>horizontal_tab
        && <t100>-arbgb && cl_abap_char_utilities=>horizontal_tab
        && <t100>-msgnr && cl_abap_char_utilities=>horizontal_tab
        && <t100>-text  && cl_abap_char_utilities=>cr_lf.
    ENDLOOP.
    display_string( lv_string ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  SELECT
    *
    FROM t100
    INTO TABLE gt_t100
  UP TO 50 ROWS.
  cl_salv_table=>factory(
    IMPORTING r_salv_table = DATA(lr_salv)
    CHANGING t_table = gt_t100 ).
  DATA:
  lo_col_tab  TYPE REF TO cl_salv_column_table.
  lo_col_tab ?= lr_salv->get_columns( )->get_column( 'ARBGB' ).
  lo_col_tab->set_cell_type( if_salv_c_cell_type=>hotspot ).
  SET HANDLER zcl_bc_dialog=>handle_link_click FOR lr_salv->get_event( ).
  lr_salv->display( ).
