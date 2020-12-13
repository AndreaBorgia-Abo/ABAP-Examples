*&---------------------------------------------------------------------*
*& Report ZABO_EDITORLOCK
*&---------------------------------------------------------------------*
*& Author: Gopi Narendra
*& Source: https://answers.sap.com/questions/3637816/editor-lock-se38.html?childToView=3637933#comment-3637933
*&---------------------------------------------------------------------*
REPORT ZABO_EDITORLOCK.
TABLES: trdir. "System table TRDIR

PARAMETERS: program LIKE trdir-name.
PARAMETERS: lock    LIKE trdir-edtx.

SELECT SINGLE * FROM trdir WHERE name = program.

trdir-edtx = lock.
MODIFY trdir.
IF sy-subrc EQ 0.
  WRITE: / 'Editor Lock update Successful ', trdir-name.
  IF trdir-edtx = 'X'.
    WRITE: ' Lock'.
  ELSE.
    WRITE: ' UnLock'.
  ENDIF.
ELSE.
  WRITE: / 'Editor Lock update Unsuccessful ', trdir-name.
ENDIF.
