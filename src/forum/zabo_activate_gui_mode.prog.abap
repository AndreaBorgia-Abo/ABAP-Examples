*&---------------------------------------------------------------------*
*& Report ZABO_ACTIVATE_GUI_MODE
*&---------------------------------------------------------------------*
*& Author: Enno Wulff
*& Source: https://blogs.sap.com/2021/06/11/bring-sapgui-mode-to-the-front/
*& Info:
*& The following demonstration report can be started in two overlapping
*& SAPGUI modes with about one second time shift. You will see that
*& rotationally the next mode will come to front.
*&---------------------------------------------------------------------*
REPORT zabo_activate_gui_mode.

DO 10 TIMES.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = |Mode: { sy-modno }  Index: { sy-index }|.

  CALL FUNCTION 'SAPGUI_SET_PROPERTY'
    DESTINATION 'SAPGUI'
    EXPORTING
      property = 'ACTIVATE'
      value    = 'X'
    EXCEPTIONS
      OTHERS   = 0.

  WAIT UP TO 3 SECONDS.
ENDDO.
