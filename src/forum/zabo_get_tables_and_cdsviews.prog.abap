*&---------------------------------------------------------------------*
*& Report ZABO_GET_TABLES_AND_CDSVIEWS
*&---------------------------------------------------------------------*
*& Author: Bärbel Winkler
*& Source: https://blogs.sap.com/2022/02/11/how-to-find-the-table-cds-name-by-giving-field-name-in-sap-ecc-and-sap-s-4-hana/#comment-614831
*&---------------------------------------------------------------------*
REPORT zabo_get_tables_and_cdsviews.


*-----------------------------------------------------------------------
*        Table definitions
*-----------------------------------------------------------------------
TABLES: dd03l,                "Fields in tables
        dd02t,                "Tablenames
        ddldependency,        "CDS-Views
        boole.
*----------------------------------------------------------------------*
*       CLASS LCX_EXCEPTION DEFINITION                                 *
*----------------------------------------------------------------------*
* Class to Raise exception                                             *
* -- Inherited from cx_static_check                                    *
* -- Redefinition of method get_text                                   *
*----------------------------------------------------------------------*
CLASS lcx_exception DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    METHODS    : get_text REDEFINITION.
    CLASS-DATA : lv_result TYPE string.
ENDCLASS.                    "LCX_EXCEPTION DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCX_EXCEPTION IMPLEMENTATION                             *
*----------------------------------------------------------------------*
CLASS lcx_exception IMPLEMENTATION.
  METHOD : get_text.
    result = lv_result.
  ENDMETHOD.
ENDCLASS.                    "LCX_EXCEPTION IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_main DEFINITION                                      *
*----------------------------------------------------------------------*
CLASS lcl_main DEFINITION.

  "Accessible from Main program
  PUBLIC SECTION.

    DATA:
      "To allow double-click from ALV to DDIC
      mo_events TYPE REF TO cl_salv_events_table READ-ONLY.

    METHODS :
      constructor   IMPORTING  VALUE(report_id) TYPE rsvar-report
                    EXCEPTIONS no_input,

      "Authority check
      check_authority,

      "Select the needed data
      get_data      RAISING lcx_exception,

      "Generate ALV-output
      display_data.

    "Known within class
  PRIVATE SECTION.
*----------------------------------------------------------------------*
*   Definitions for data selection and processing                       *
*----------------------------------------------------------------------*
    "Selected data
    TYPES: BEGIN OF ty_table,
             tabname TYPE tabname,
             ddtext  TYPE ddtext,
           END OF ty_table.

    TYPES: tyt_table     TYPE STANDARD TABLE OF ty_table.

    TYPES: BEGIN OF ty_cdsview,
             ddlname    TYPE ddlname,
             objectname TYPE ddtext,
             tabname    TYPE tabname,
             fieldname  TYPE fieldname,
             rollname   TYPE rollname,
             ddtext     TYPE ddtext,
           END OF ty_cdsview.

    TYPES: tyt_cdsview     TYPE STANDARD TABLE OF ty_cdsview.

    "ALV-output
    TYPES: BEGIN OF ty_out,
             tabname    TYPE dd03l-tabname,
             ddlname    TYPE ddldependency-ddlname,
             objectname TYPE ddldependency-objectname,
             ddtext     TYPE ddtext,
             field1     TYPE dd03l-fieldname,
             field2     TYPE dd03l-fieldname,
             field3     TYPE dd03l-fieldname,
             field4     TYPE dd03l-fieldname,
           END OF ty_out .

    DATA: lt_out   TYPE STANDARD TABLE OF ty_out.

    "Definitions for selection screen data
    DATA: lt_param  TYPE STANDARD TABLE OF rsparams.

    "Deep structure for selection-screen content. Note: the fields need to be
    "typed and named like the corresponding selection-screen fields!
    "update as needed
    TYPES: BEGIN OF ty_selscreen_data,
             p_cds    TYPE          abap_bool,
             p_table  TYPE          abap_bool,
             s_field1 TYPE RANGE OF fieldname,
             s_field2 TYPE RANGE OF fieldname,
             s_field3 TYPE RANGE OF fieldname,
             s_field4 TYPE RANGE OF fieldname,
             s_exc    TYPE RANGE OF abap_bool,
           END OF ty_selscreen_data.

    DATA: ls_selscreen_data TYPE ty_selscreen_data.

    "Definitions for ALV
    DATA: lr_table     TYPE REF TO cl_salv_table,
          lr_key       TYPE salv_s_layout_key,
          zcx_salv_msg TYPE REF TO cx_salv_msg,
          lv_msg       TYPE string.

    "Definitions for method ALV_TOP_OF_PAGE
    DATA: lr_top_element  TYPE REF TO cl_salv_form_layout_grid,
          lr_end_element  TYPE REF TO cl_salv_form_layout_flow,
          lr_grid         TYPE REF TO cl_salv_form_layout_grid,
          lr_header       TYPE REF TO cl_salv_form_header_info,
          lr_action       TYPE REF TO cl_salv_form_action_info,
          lr_textview1    TYPE REF TO cl_salv_form_text,
          lr_icon         TYPE REF TO cl_salv_form_icon,
          lr_layout       TYPE REF TO cl_salv_layout,
          lr_layout_grid1 TYPE REF TO cl_salv_form_layout_data_grid.

    DATA: lv_text(50)    TYPE c,
          lv_cnt_txt(20) TYPE c.

    "Other variables
    DATA: lv_cnt_in      LIKE sy-index,
          lv_cnt_out     LIKE sy-index,
          lv_tech_fields TYPE abap_bool.

*----------------------------------------------------------------------*
*   Methods called from within other methods in the class              *
*----------------------------------------------------------------------*
    METHODS:

      "Put values from selection-screen into internal structures
      populate_parameters,

      "Get tablenames which contain specific fields
      get_standard_tables,

      "Get CDS-Views which contain specific fields
      get_cds_views,

      get_table_data  IMPORTING VALUE(iv_tab)       TYPE dd03l-tabname
                                VALUE(i_tabfield)   TYPE fieldname
                      EXPORTING VALUE(e_tablenames) TYPE tyt_table,

      get_cds_data    IMPORTING VALUE(iv_tab)     TYPE dd03l-tabname
                                VALUE(i_tabfield) TYPE fieldname
                      EXPORTING VALUE(e_cdsnames) TYPE tyt_cdsview,

      "Headings for ALV-output
      alv_top_of_page,

      "Custom column headings for ALV output
      alv_column_headings,

      "Jump to SE12
      on_link_click
        FOR EVENT link_click
        OF cl_salv_events_table
        IMPORTING row column.

ENDCLASS.                    "lcl_data DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_main IMPLEMENTATION                                  *
*----------------------------------------------------------------------*
CLASS lcl_main IMPLEMENTATION.

*----------------------------------------------------------------------*
* Constructor method to extract selection screen components & move the *
* details to internal table.  Call private method "POPULATE_PARAMETERS *
* to populate the components to appropriate variables                  *
*----------------------------------------------------------------------*
  METHOD constructor.

    "Get selection-screen content into internal fields and tables
    REFRESH : lt_param.

    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report     = report_id
      TABLES
        selection_table = lt_param
      EXCEPTIONS
        not_found       = 1
        no_report       = 2
        OTHERS          = 3.

    IF sy-subrc <> 0.
      RAISE no_input.
    ELSE.
      populate_parameters( ).
    ENDIF.

  ENDMETHOD.                    "constructor

*----------------------------------------------------------------------*
* Private method to Populate selection screen components               *
* to appropriate variables                                             *
*----------------------------------------------------------------------*
  METHOD populate_parameters.

    DATA: ls_param  TYPE rsparams.

    "To put selection-screen content into deep structure
    FIELD-SYMBOLS: <ls_substruct> TYPE any,
                   <lt_subtable>  TYPE STANDARD TABLE,
                   <ls_subtable>  TYPE any.

    LOOP AT lt_param INTO ls_param.

      IF ls_param-kind EQ 'S' AND
         ls_param-sign EQ space.
        "Don't fille range-table when nothing specified in sel-option
        CONTINUE.
      ENDIF.

      "Fill deep-structure with selection-screen content
      "This only works properly when the structure-names in the deep-structure
      "have the same names as the fields on the selection-screen!
      IF ls_param-kind = 'P'.
        "For a parameter only the field needs to be moved
        ASSIGN COMPONENT ls_param-selname OF STRUCTURE ls_selscreen_data TO <ls_substruct>.
        IF sy-subrc = 0.
          <ls_substruct> = ls_param-low.
        ENDIF.
      ELSEIF ls_param-kind = 'S'.
        "For a select-option, the "range" structure needs to be added as a new line
        ASSIGN COMPONENT ls_param-selname OF STRUCTURE ls_selscreen_data TO <lt_subtable>.
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO <lt_subtable> ASSIGNING <ls_subtable>.
          MOVE-CORRESPONDING ls_param TO <ls_subtable>.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "populate_parameters

*----------------------------------------------------------------------*
* Check authority                                                      *
*----------------------------------------------------------------------*
  METHOD check_authority.

    "Only users with dev-access can execute program
    AUTHORITY-CHECK OBJECT 'S_DEVELOP'
                    ID 'ACTVT'    FIELD '03'
                    ID 'DEVCLASS' DUMMY
                    ID 'OBJTYPE'  DUMMY
                    ID 'OBJNAME'  DUMMY
                    ID 'P_GROUP'  DUMMY.

    IF sy-subrc NE 0.
      MESSAGE e000(38) WITH 'S_DEVELOP authorization required!'(e02).
    ENDIF.

  ENDMETHOD.

*----------------------------------------------------------------------*
* Select data based on selection screen components                     *
*----------------------------------------------------------------------*
  METHOD get_data.

    IF ls_selscreen_data-p_table EQ 'X'.
      get_standard_tables( ).
    ELSE.
      get_cds_views( ).
    ENDIF.

    IF lt_out[]   IS INITIAL.
      lcx_exception=>lv_result = 'No data available'(e01).
      RAISE EXCEPTION TYPE lcx_exception.
    ENDIF.

  ENDMETHOD.                    "get_data

*----------------------------------------------------------------------*
* Get tablenames for fieldnames                                        *
*----------------------------------------------------------------------*
  METHOD get_standard_tables.

    DATA: ls_out            TYPE ty_out,
          tables_fieldname1 TYPE STANDARD TABLE OF ty_table,
          tables_fieldname2 TYPE STANDARD TABLE OF ty_table,
          tables_fieldname3 TYPE STANDARD TABLE OF ty_table,
          tables_fieldname4 TYPE STANDARD TABLE OF ty_table,
          lv_tab            TYPE dd03l-tabname.

    FIELD-SYMBOLS: <fs_tab13> TYPE mandt.

    IF ls_selscreen_data-s_exc[ 1 ]-low IS NOT INITIAL.
      lv_tab = '/%'.
    ENDIF.

    IF ls_selscreen_data-s_field1 IS NOT INITIAL.
      get_table_data( EXPORTING iv_tab      = lv_tab
                                i_tabfield = ls_selscreen_data-s_field1[ 1 ]-low
                      IMPORTING e_tablenames = tables_fieldname1 ).
    ENDIF.

    IF ls_selscreen_data-s_field2 IS NOT INITIAL.
      get_table_data( EXPORTING iv_tab      = lv_tab
                                i_tabfield = ls_selscreen_data-s_field2[ 1 ]-low
                      IMPORTING e_tablenames = tables_fieldname2 ).
    ENDIF.

    IF ls_selscreen_data-s_field3 IS NOT INITIAL.
      get_table_data( EXPORTING iv_tab      = lv_tab
                                i_tabfield = ls_selscreen_data-s_field3[ 1 ]-low
                      IMPORTING e_tablenames = tables_fieldname3 ).
    ENDIF.

    IF ls_selscreen_data-s_field4 IS NOT INITIAL.
      get_table_data( EXPORTING iv_tab      = lv_tab
                                i_tabfield = ls_selscreen_data-s_field4[ 1 ]-low
                      IMPORTING e_tablenames = tables_fieldname4 ).
    ENDIF.

    IF ls_selscreen_data-s_field1[ 1 ]-low IS NOT INITIAL.

      LOOP AT tables_fieldname1 INTO DATA(table_fieldname1).
        IF tables_fieldname2[] IS NOT INITIAL.
          READ TABLE tables_fieldname2 INTO DATA(table_fieldname2)
                            WITH KEY tabname = table_fieldname1-tabname.
          IF sy-subrc = 0.
            IF tables_fieldname3[] IS NOT INITIAL.
              READ TABLE tables_fieldname3 INTO DATA(table_fieldname3)
                                WITH KEY tabname = table_fieldname1-tabname.
              IF sy-subrc = 0.
                IF tables_fieldname4[] IS NOT INITIAL.
                  READ TABLE tables_fieldname4 INTO DATA(table_fieldname4)
                                    WITH KEY tabname = table_fieldname1-tabname.
                  IF sy-subrc = 0.
                    MOVE-CORRESPONDING table_fieldname1 TO ls_out.
                    ls_out-field1 =  ls_selscreen_data-s_field1[ 1 ]-low.
                    ls_out-field2 =  ls_selscreen_data-s_field2[ 1 ]-low.
                    ls_out-field3 =  ls_selscreen_data-s_field3[ 1 ]-low.
                    ls_out-field4 =  ls_selscreen_data-s_field4[ 1 ]-low.
                    APPEND ls_out TO lt_out.
                    CLEAR ls_out.
                  ELSE.
                    MOVE-CORRESPONDING table_fieldname1 TO ls_out.
                    ls_out-field1 =  ls_selscreen_data-s_field1[ 1 ]-low.
                    ls_out-field2 =  ls_selscreen_data-s_field2[ 1 ]-low.
                    ls_out-field3 =  ls_selscreen_data-s_field3[ 1 ]-low.
                    APPEND ls_out TO lt_out.
                    CLEAR ls_out.
                  ENDIF.
                ELSE.
                  MOVE-CORRESPONDING table_fieldname1 TO ls_out.
                  ls_out-field1 =  ls_selscreen_data-s_field1[ 1 ]-low.
                  ls_out-field2 =  ls_selscreen_data-s_field2[ 1 ]-low.
                  ls_out-field3 =  ls_selscreen_data-s_field3[ 1 ]-low.
                  APPEND ls_out TO lt_out.
                  CLEAR ls_out.
                ENDIF.
              ELSE.
                MOVE-CORRESPONDING table_fieldname1 TO ls_out.
                ls_out-field1 =  ls_selscreen_data-s_field1[ 1 ]-low.
                ls_out-field2 =  ls_selscreen_data-s_field2[ 1 ]-low.
                APPEND ls_out TO lt_out.
                CLEAR ls_out.
              ENDIF.
            ELSE.
              MOVE-CORRESPONDING table_fieldname1 TO ls_out.
              ls_out-field1 =  ls_selscreen_data-s_field1[ 1 ]-low.
              ls_out-field2 =  ls_selscreen_data-s_field2[ 1 ]-low.
              APPEND ls_out TO lt_out.
              CLEAR table_fieldname2.
              CLEAR ls_out.
            ENDIF.
          ELSE.
            MOVE-CORRESPONDING table_fieldname1 TO ls_out.
            ls_out-field1 =  ls_selscreen_data-s_field1[ 1 ]-low.
            APPEND ls_out TO lt_out.
            CLEAR ls_out.
          ENDIF.
        ELSE.
          MOVE-CORRESPONDING table_fieldname1 TO ls_out.
          ls_out-field1 =  ls_selscreen_data-s_field1[ 1 ]-low.
          APPEND ls_out TO lt_out.
          CLEAR table_fieldname1.
          CLEAR ls_out.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

*----------------------------------------------------------------------*
* Get CDS-Views for fieldnames                                        *
*----------------------------------------------------------------------*
  METHOD get_cds_views.

    DATA: ls_out              TYPE ty_out,
          cdsviews_fieldname1 TYPE STANDARD TABLE OF ty_cdsview,
          cdsviews_fieldname2 TYPE STANDARD TABLE OF ty_cdsview,
          cdsviews_fieldname3 TYPE STANDARD TABLE OF ty_cdsview,
          cdsviews_fieldname4 TYPE STANDARD TABLE OF ty_cdsview,
          lv_tab              TYPE dd03l-tabname.


    IF ls_selscreen_data-s_exc[ 1 ]-low IS NOT INITIAL.
      lv_tab = '/%'.
    ENDIF.

    IF ls_selscreen_data-s_field1 IS NOT INITIAL.
      get_cds_data( EXPORTING iv_tab     = lv_tab
                              i_tabfield = ls_selscreen_data-s_field1[ 1 ]-low
                    IMPORTING e_cdsnames = cdsviews_fieldname1 ).
    ENDIF.

    IF ls_selscreen_data-s_field2 IS NOT INITIAL.
      get_cds_data( EXPORTING iv_tab     = lv_tab
                              i_tabfield = ls_selscreen_data-s_field2[ 1 ]-low
                    IMPORTING e_cdsnames = cdsviews_fieldname2 ).
    ENDIF.

    IF ls_selscreen_data-s_field3 IS NOT INITIAL.
      get_cds_data( EXPORTING iv_tab     = lv_tab
                              i_tabfield = ls_selscreen_data-s_field3[ 1 ]-low
                    IMPORTING e_cdsnames = cdsviews_fieldname3 ).
    ENDIF.

    IF ls_selscreen_data-s_field4 IS NOT INITIAL.
      get_cds_data( EXPORTING iv_tab     = lv_tab
                              i_tabfield = ls_selscreen_data-s_field4[ 1 ]-low
                    IMPORTING e_cdsnames = cdsviews_fieldname4 ).
    ENDIF.

    IF ls_selscreen_data-s_field1[ 1 ]-low IS NOT INITIAL.

      LOOP AT cdsviews_fieldname1 INTO DATA(cdsview_fieldname1).
        IF cdsviews_fieldname2[] IS NOT INITIAL.
          READ TABLE cdsviews_fieldname2 INTO DATA(cdsview_fieldname2)
                            WITH KEY tabname = cdsview_fieldname1-tabname.
          IF sy-subrc = 0.
            IF cdsviews_fieldname3[] IS NOT INITIAL.
              READ TABLE cdsviews_fieldname3 INTO DATA(cdsview_fieldname3)
                                WITH KEY tabname = cdsview_fieldname1-tabname.
              IF sy-subrc = 0.
                IF cdsviews_fieldname4[] IS NOT INITIAL.
                  READ TABLE cdsviews_fieldname4 INTO DATA(cdsview_fieldname4)
                                    WITH KEY tabname = cdsview_fieldname1-tabname.
                  IF sy-subrc = 0.
                    MOVE-CORRESPONDING cdsview_fieldname1 TO ls_out.
                    ls_out-field1 =  ls_selscreen_data-s_field1[ 1 ]-low.
                    ls_out-field2 =  ls_selscreen_data-s_field2[ 1 ]-low.
                    ls_out-field3 =  ls_selscreen_data-s_field3[ 1 ]-low.
                    ls_out-field4 =  ls_selscreen_data-s_field4[ 1 ]-low.
                    APPEND ls_out TO lt_out.
                    CLEAR ls_out.
                  ELSE.
                    MOVE-CORRESPONDING cdsview_fieldname1 TO ls_out.
                    ls_out-field1 =  ls_selscreen_data-s_field1[ 1 ]-low.
                    ls_out-field2 =  ls_selscreen_data-s_field2[ 1 ]-low.
                    ls_out-field3 =  ls_selscreen_data-s_field3[ 1 ]-low.
                    APPEND ls_out TO lt_out.
                    CLEAR ls_out.
                  ENDIF.
                ELSE.
                  MOVE-CORRESPONDING cdsview_fieldname1 TO ls_out.
                  ls_out-field1 =  ls_selscreen_data-s_field1[ 1 ]-low.
                  ls_out-field2 =  ls_selscreen_data-s_field2[ 1 ]-low.
                  ls_out-field3 =  ls_selscreen_data-s_field3[ 1 ]-low.
                  APPEND ls_out TO lt_out.
                  CLEAR ls_out.
                ENDIF.
              ELSE.
                MOVE-CORRESPONDING cdsview_fieldname1 TO ls_out.
                ls_out-field1 =  ls_selscreen_data-s_field1[ 1 ]-low.
                ls_out-field2 =  ls_selscreen_data-s_field2[ 1 ]-low.
                APPEND ls_out TO lt_out.
                CLEAR ls_out.
              ENDIF.
            ELSE.
              MOVE-CORRESPONDING cdsview_fieldname1 TO ls_out.
              ls_out-field1 =  ls_selscreen_data-s_field1[ 1 ]-low.
              ls_out-field2 =  ls_selscreen_data-s_field2[ 1 ]-low.
              APPEND ls_out TO lt_out.
              CLEAR cdsview_fieldname2.
              CLEAR ls_out.
            ENDIF.
          ELSE.
            MOVE-CORRESPONDING cdsview_fieldname1 TO ls_out.
            ls_out-field1 =  ls_selscreen_data-s_field1[ 1 ]-low.
            APPEND ls_out TO lt_out.
            CLEAR ls_out.
          ENDIF.
        ELSE.
          MOVE-CORRESPONDING cdsview_fieldname1 TO ls_out.
          ls_out-field1 =  ls_selscreen_data-s_field1[ 1 ]-low.
          APPEND ls_out TO lt_out.
          CLEAR cdsview_fieldname1.
          CLEAR ls_out.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

*----------------------------------------------------------------------*
* Select tables which contain fields in their definition               *
*----------------------------------------------------------------------*
  METHOD get_table_data.

    SELECT DISTINCT c~tabname, t~ddtext
      FROM dd03l AS c
      JOIN dd02v AS p
        ON p~tabname  = c~tabname
       AND p~tabclass = 'TRANSP'
      JOIN dd09l AS f
        ON f~tabname = p~tabname
      LEFT OUTER JOIN dd02t AS t
        ON t~tabname    = c~tabname
       AND t~ddlanguage = @sy-langu
      INTO CORRESPONDING FIELDS OF TABLE @e_tablenames
     WHERE ( c~rollname  EQ @i_tabfield
          OR c~fieldname EQ @i_tabfield )
       AND p~tabname NOT LIKE @iv_tab
     ORDER BY c~tabname.

  ENDMETHOD.

*----------------------------------------------------------------------*
* Select CDS-view defintions which contain fields in their definition  *
*----------------------------------------------------------------------*
  METHOD get_cds_data.

    SELECT DISTINCT d~ddlname, d~objectname, p~tabname, p~fieldname, p~rollname, t~ddtext
      FROM ddldependency AS d
      JOIN dd03l AS p
        ON p~tabname  = d~objectname
       AND p~as4local = d~state
      LEFT OUTER JOIN ddddlsrct AS t
        ON t~ddlname    = d~ddlname
       AND t~ddlanguage = @sy-langu
      INTO CORRESPONDING FIELDS OF TABLE @e_cdsnames
     WHERE ( p~rollname  EQ @i_tabfield
          OR p~fieldname EQ @i_tabfield )
       AND p~tabname NOT LIKE @iv_tab
     ORDER BY d~ddlname.

  ENDMETHOD.

*----------------------------------------------------------------------*
* Display extracted data using SALV Classes                            *
*----------------------------------------------------------------------*
  METHOD display_data.

    DESCRIBE TABLE lt_out LINES lv_cnt_out.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = lr_table
          CHANGING
            t_table        = lt_out ).

        "Activate ALV generic Functions
        "Note: PF-Status STANDARD needs to be copied from FG SALV as SALV_STANDARD!
        lr_table->set_screen_status(
          pfstatus      = 'SALV_STANDARD'
          report        = sy-repid
          set_functions = lr_table->c_functions_all ).

        "Add information at top of page in ALV-output
        alv_top_of_page( ).

        lr_layout = lr_table->get_layout( ).

        lr_key-report = sy-repid.
        lr_layout->set_key( lr_key ).

        lr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

        "Overwrite heading for column DIFF
        alv_column_headings(  ).

        "Register event handlers
        mo_events = lr_table->get_event( ).

        SET HANDLER on_link_click FOR mo_events.

        "Generate ALV-output
        lr_table->display( ).

      CATCH cx_salv_msg INTO zcx_salv_msg.
        lv_msg = zcx_salv_msg->get_text( ).
        WRITE :/ lv_msg.
    ENDTRY.

  ENDMETHOD.                    "display_data

*----------------------------------------------------------------------*
* Add information at top of ALV-output                                 *
*----------------------------------------------------------------------*
  METHOD alv_top_of_page.

    CREATE OBJECT lr_top_element
      EXPORTING
        columns = 2.

    lr_header = lr_top_element->create_header_information(
      row    = 1
      column = 1
      text   =  sy-title ).

    lv_cnt_txt = lv_cnt_out.

    CONCATENATE '# of items selected:'(c01)
                lv_cnt_txt
           INTO lv_text.

    lr_grid = lr_top_element->create_grid( row = 2
                                           column = 1 ).

    lr_textview1 = lr_grid->create_text(
      row     = 1
      column  = 1
      text    = lv_text ).

    lr_layout_grid1 ?= lr_textview1->get_layout_data( ).
    lr_layout_grid1->set_h_align( if_salv_form_c_h_align=>left ).

    lr_table->set_top_of_list( lr_top_element ).

  ENDMETHOD.

*----------------------------------------------------------------------*
* Change ALV column headings as needed                                 *
* Note: SHORT_TEXT cannot be longer than 10 characters. This may need  *
*       to be taken into account for text-elements which are generated *
*       to twice the needed length.                                    *
*----------------------------------------------------------------------*
  METHOD alv_column_headings.

    DATA: long_text   TYPE        scrtext_l,
          medium_text TYPE        scrtext_m,
          short_text  TYPE        scrtext_s.

    DATA: lr_columns TYPE REF TO cl_salv_columns,
          lr_column  TYPE REF TO cl_salv_column_table.

    TRY.

        lr_columns = lr_table->get_columns( ).
        lr_columns->set_optimize( 'X' ).


        "Hide columns not relevant for CDS- or table-selection and set hotspot column
        IF ls_selscreen_data-p_table EQ abap_true.

          lr_column ?= lr_columns->get_column( 'TABNAME' ).
          lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

          lr_column ?= lr_columns->get_column( 'DDLNAME' ).
          lr_column->set_technical(  ).

          lr_column ?= lr_columns->get_column( 'OBJECTNAME' ).
          lr_column->set_technical(  ).

        ELSE.
          lr_column ?= lr_columns->get_column( 'TABNAME' ).
          lr_column->set_technical(  ).

          lr_column ?= lr_columns->get_column( 'OBJECTNAME' ).
          lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

        ENDIF.

        lr_column ?= lr_columns->get_column( 'FIELD1' ).
        lr_column->set_long_text( 'Fieldname 1'(h01) ).
        lr_column->set_medium_text( 'Fieldname 1'(h01) ).

        lr_column ?= lr_columns->get_column( 'FIELD2' ).
        lr_column->set_long_text( 'Fieldname 2'(h02) ).
        lr_column->set_medium_text( 'Fieldname 2'(h02) ).

        lr_column ?= lr_columns->get_column( 'FIELD3' ).
        lr_column->set_long_text( 'Fieldname 3'(h03) ).
        lr_column->set_medium_text( 'Fieldname 3'(h03) ).

        lr_column ?= lr_columns->get_column( 'FIELD4' ).
        lr_column->set_long_text( 'Fieldname 4'(h04) ).
        lr_column->set_medium_text( 'Fieldname 4'(h04) ).

      CATCH cx_salv_not_found INTO DATA(lcx_salv_not_found).
    ENDTRY.

  ENDMETHOD.                     "alv_column_headings

*----------------------------------------------------------------------*
* Jump to other transaction to display transport or delivery           *
*----------------------------------------------------------------------*
  METHOD on_link_click.
    TRY.
        DATA(selected_entry) = lt_out[ row ].
        CASE column.
          WHEN 'TABNAME'.
            "Call DDIC
            SET PARAMETER ID 'DTB' FIELD selected_entry-tabname.
            CALL TRANSACTION 'SE12' AND SKIP FIRST SCREEN.

          WHEN 'OBJECTNAME'.
            "Call DDIC
            SET PARAMETER ID 'DTB' FIELD selected_entry-objectname.
            CALL TRANSACTION 'SE12' AND SKIP FIRST SCREEN.

          WHEN OTHERS.
        ENDCASE.
      CATCH cx_sy_itab_line_not_found.
        "Error message goes here
    ENDTRY.
  ENDMETHOD.

ENDCLASS.                    "lcl_order IMPLEMENTATION

*-----------------------------------------------------------------------
*      Object references and variables
*-----------------------------------------------------------------------
DATA : zcl_main      TYPE REF TO lcl_main,
       zcx_exception TYPE REF TO lcx_exception.

DATA : lv_result     TYPE string.

*-----------------------------------------------------------------------
*           Selection-Screen Handling
*-----------------------------------------------------------------------
TYPE-POOLS: sscr.

*-----------------------------------------------------------------------
*          Selection-Screen
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01.
  PARAMETERS: p_table RADIOBUTTON GROUP rdg1 DEFAULT 'X',
              p_cds   RADIOBUTTON GROUP rdg1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-s02.
  SELECT-OPTIONS: s_field1 FOR dd03l-fieldname NO-EXTENSION NO INTERVALS OBLIGATORY,
                  s_field2 FOR dd03l-fieldname NO-EXTENSION NO INTERVALS ,
                  s_field3 FOR dd03l-fieldname NO-EXTENSION NO INTERVALS ,
                  s_field4 FOR dd03l-fieldname NO-EXTENSION NO INTERVALS ,
                  s_exc    FOR boole-boole NO-EXTENSION NO INTERVALS DEFAULT'/' NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b2.

*-----------------------------------------------------------------------
*        INITIALIZATION
*-----------------------------------------------------------------------
INITIALIZATION.

*-----------------------------------------------------------------------
*       AT-SELECTION-SCREEN OUTPUT
*-----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.

*-----------------------------------------------------------------------
*       AT-SELECTION-SCREEN
*-----------------------------------------------------------------------
AT SELECTION-SCREEN.

*-----------------------------------------------------------------------
*       START-OF-SELECTION
*-----------------------------------------------------------------------
START-OF-SELECTION.

  CREATE OBJECT zcl_main
    EXPORTING
      report_id = sy-repid
    EXCEPTIONS
      no_input  = 1.

  IF sy-subrc EQ 0.
    TRY.
        "Check authority
        zcl_main->check_authority( ).

        "Select data for Tables and CDS-Views and prepare output
        zcl_main->get_data( ).

        "Generate ALV-output
        zcl_main->display_data( ).

        "More logic as needed

      CATCH lcx_exception INTO zcx_exception.
        lv_result = zcx_exception->get_text( ).
        WRITE:/ lv_result.
    ENDTRY.
  ENDIF.
