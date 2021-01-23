*&---------------------------------------------------------------------*
*& Report zabo_itf_format
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://answers.sap.com/answers/13233277/view.html
*&---------------------------------------------------------------------*
REPORT zabo_itf_format.

BREAK-POINT.

DATA(stream_lines) = VALUE string_table( (
       |this is a very long text this is a very long text this|
    && | is a very long text this is a very long text this is a very long text|
    && |\n\nthis is a very long text this is a very long text this is a very long text|
    && | this is a very long text this is a very long text| ) ).
DATA(itf_text) = VALUE tline_tab( ).

CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
  EXPORTING
    stream_lines = stream_lines
    lf           = 'X'
  TABLES
    itf_text     = itf_text.

ASSERT itf_text = VALUE tline_tab(
    ( tdformat = '*' tdline = 'this is a very long text this is a very long text this is a very long' )
    ( tdformat = ' ' tdline = 'text this is a very long text this is a very long text' )
    ( tdformat = '*' tdline = '' )
    ( tdformat = '*' tdline = 'this is a very long text this is a very long text this is a very long' )
    ( tdformat = ' ' tdline = 'text this is a very long text this is a very long text' ) ).

BREAK-POINT.
