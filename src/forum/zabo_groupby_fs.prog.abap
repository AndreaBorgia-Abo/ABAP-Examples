*&---------------------------------------------------------------------*
*& Report ZABO_GROUPBY_FS
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://answers.sap.com/comments/13688077/view.html
*&---------------------------------------------------------------------*
REPORT ZABO_GROUPBY_FS.

TYPES ty_spfli_table TYPE STANDARD TABLE OF spfli WITH EMPTY KEY.
TYPES: BEGIN OF ty_group_by,
         carrid   TYPE spfli-carrid,
         distance TYPE spfli-distance,
       END OF ty_group_by,
       ty_group_bys TYPE STANDARD TABLE OF ty_group_by WITH EMPTY KEY.

DATA(spfli_table) = VALUE ty_spfli_table(
    ( carrid = 'LH' connid = '0002' distance = 145 )
    ( carrid = 'AF' connid = '0003' distance = 123 )
    ( carrid = 'LH' connid = '0001' distance = 100 )
    ( carrid = 'AF' connid = '0001' distance =  77 ) ).

DATA(group_bys) = VALUE ty_group_bys( ).
LOOP AT spfli_table ASSIGNING FIELD-SYMBOL(<spfli>)
      GROUP BY ( carrid = <spfli>-carrid )
      REFERENCE INTO DATA(group_spfli).
  DATA(group_by) = VALUE ty_group_by( carrid = group_spfli->carrid ).
  LOOP AT GROUP group_spfli ASSIGNING FIELD-SYMBOL(<group_spfli_line>).
    group_by-distance = group_by-distance + <group_spfli_line>-distance.
  ENDLOOP.
  APPEND group_by TO group_bys.
ENDLOOP.

ASSERT group_bys = VALUE ty_group_bys(
    ( carrid = 'LH' distance = 245 )
    ( carrid = 'AF' distance = 200 ) ).
