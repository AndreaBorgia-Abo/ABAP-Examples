*&---------------------------------------------------------------------*
*& Author: Michael Keller
*& Source: https://blogs.sap.com/2020/05/03/example-with-open-sql-test-double-framework/
*&---------------------------------------------------------------------*
CLASS zabo_cl_system_clients DEFINITION
                         PUBLIC
                         FINAL
                         CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES clients TYPE TABLE OF t000 WITH KEY mandt.

    METHODS get_client
      IMPORTING
        client        TYPE mandt
      RETURNING
        VALUE(result) TYPE clients.
ENDCLASS.

CLASS zabo_cl_system_clients IMPLEMENTATION.
  METHOD get_client.
    SELECT * FROM t000 AS clients
             INTO TABLE result
             WHERE mandt = client.
  ENDMETHOD.
ENDCLASS.
