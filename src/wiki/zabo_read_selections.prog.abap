*&---------------------------------------------------------------------*
*& Report ZABO_READ_SELECTIONS
*&---------------------------------------------------------------------*
*& Author: Andrea Borgia
*&---------------------------------------------------------------------*
REPORT zabo_read_selections.


TABLES: sflight.


SELECT-OPTIONS s_carrid FOR sflight-carrid DEFAULT 'AA' TO 'LH'.
PARAMETERS: p_spras LIKE t002-spras DEFAULT 'EN'.


DATA: lt_param TYPE TABLE OF rsparams,
      ls_param TYPE rsparams.


CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
  EXPORTING
    curr_report     = sy-repid
  TABLES
    selection_table = lt_param[].


LOOP AT lt_param INTO ls_param.
  WRITE: / | Row { sy-tabix }, selname: { ls_param-selname } /|
        && | kind: { ls_param-kind } /|
        && | sign: { ls_param-sign } /|
        && | option: { ls_param-option } /|
        && | low: { ls_param-low } /|
        && | high: { ls_param-high }|.
ENDLOOP.
