*&---------------------------------------------------------------------*
*& Report zabo_cte
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://answers.sap.com/comments/13436144/view.html
*&---------------------------------------------------------------------*
REPORT zabo_cte.

TYPES: BEGIN OF ty_itab,
         carrname LIKE scarr-carrname,
         connid   LIKE spfli-connid,
         cityfrom LIKE spfli-cityfrom,
         cityto   LIKE spfli-cityto,
         cnt      TYPE i,
       END OF ty_itab.

DATA: carrid LIKE scarr-carrid,
      itab   TYPE HASHED TABLE OF ty_itab WITH UNIQUE KEY carrname connid cityfrom cityto.


cl_demo_input=>request(
  EXPORTING
    text        = 'Carrier ID'
 CHANGING
    field       = carrid
).

WITH
      +conns AS (
        SELECT carrname, connid, cityfrom, cityto
              FROM spfli
                JOIN scarr ON spfli~carrid = scarr~carrid
              WHERE spfli~carrid = @carrid ),
      +cnts AS (
        SELECT COUNT(*) AS cnt
               FROM +conns )
      SELECT *
             FROM +cnts
               CROSS JOIN +conns
             ORDER BY carrname, connid
             INTO CORRESPONDING FIELDS OF TABLE @itab.

cl_demo_output=>display(  itab ).


WITH +texts AS (
      SELECT trkorr, langu
            FROM e07t
            WHERE langu = @sy-langu
      UNION
      SELECT trkorr, MIN( langu )
            FROM e07t
            WHERE NOT EXISTS ( SELECT * FROM e07t AS b WHERE trkorr = e07t~trkorr AND langu = @sy-langu )
            GROUP BY trkorr )
  SELECT e070~trkorr, e070~as4date, e07t~as4text
        FROM e070
          INNER JOIN e07t
            ON e07t~trkorr = e070~trkorr
          INNER JOIN +texts
            ON e07t~trkorr = +texts~trkorr
            AND e07t~langu = +texts~langu
        WHERE e070~trkorr NOT LIKE 'SAP%'
          AND e070~strkorr = ''
  INTO TABLE @DATA(requests).

cl_demo_output=>display( data = requests ).
