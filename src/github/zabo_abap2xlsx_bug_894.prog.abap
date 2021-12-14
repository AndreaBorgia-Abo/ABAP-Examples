*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_BUG_894
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/abap2xlsx/abap2xlsx/files/7577209/abap_program_to_compare_writer_and_huge_writer.txt
*&---------------------------------------------------------------------*
REPORT zabo_abap2xlsx_bug_894.

CLASS ltc_get_zip_files_at_path DEFINITION DEFERRED.
CLASS ltc_compare_zip_files DEFINITION DEFERRED.

INTERFACE lif_demo.
  METHODS generate
    IMPORTING
      iv_huge        TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(rv_xlsx) TYPE xstring
    RAISING
      zcx_excel.
ENDINTERFACE.

CLASS lcl_app DEFINITION FRIENDS ltc_get_zip_files_at_path ltc_compare_zip_files.
  PUBLIC SECTION.
    METHODS set_sscrfields IMPORTING sscrfields TYPE REF TO sscrfields.
    METHODS at_selection_screen_output.
    METHODS at_selection_screen.

  PRIVATE SECTION.

    TYPES : BEGIN OF ty_diff_file,
              name      TYPE string,
              "! <ul>
              "! <li>' ' : no change</li>
              "! <li>'only in 1' : file has been deleted</li>
              "! <li>'only in 2' : file has been created</li>
              "! <li>'same' : </li>
              "! </ul>
              diff      TYPE string,
              is_folder TYPE abap_bool,
              sub_files TYPE REF TO data, "create data sub_files type ty_diff_files
            END OF ty_diff_file,
            ty_diff_files TYPE HASHED TABLE OF ty_diff_file WITH UNIQUE KEY name,
            ty_tree_nodes TYPE STANDARD TABLE OF ixmltree1 WITH EMPTY KEY,
            BEGIN OF ty_tree_file_link,
              node_key  TYPE tv_nodekey,
              diff      TYPE ty_diff_file-diff,
              file_path TYPE string,
            END OF ty_tree_file_link,
            ty_tree_file_links TYPE STANDARD TABLE OF ty_tree_file_link WITH EMPTY KEY.

    METHODS load_binary_file
      IMPORTING
        path           TYPE csequence
      RETURNING
        VALUE(content) TYPE xstring.
    METHODS compare_zip_files
      IMPORTING
        zip_1             TYPE REF TO cl_abap_zip
        zip_2             TYPE REF TO cl_abap_zip
        path              TYPE string OPTIONAL
      RETURNING
        VALUE(diff_files) TYPE ty_diff_files.
    METHODS get_zip_files_at_path
      IMPORTING
        files             TYPE cl_abap_zip=>t_files
        path              TYPE string OPTIONAL
      RETURNING
        VALUE(path_files) TYPE string_table.
    METHODS compare_files
      IMPORTING
        zip_1       TYPE REF TO cl_abap_zip
        zip_2       TYPE REF TO cl_abap_zip
        name_1      TYPE string
        name_2      TYPE string
      RETURNING
        VALUE(diff) TYPE ty_diff_file-diff.
    METHODS display_tree
      IMPORTING
        io_container TYPE REF TO cl_gui_container.
    METHODS add_tree_nodes
      IMPORTING
        parent_node_key TYPE tv_nodekey
        path            TYPE string
        diff_files      TYPE ty_diff_files
      CHANGING
        tree_nodes      TYPE ty_tree_nodes.
    METHODS gui_download
      IMPORTING
        i_content   TYPE xstring
        i_file_path TYPE string.
    METHODS xml_pretty_print
      CHANGING
        c_content TYPE xstring.
    METHODS get_zip_from_xstring
      IMPORTING
        xdata         TYPE xstring
      RETURNING
        VALUE(ro_zip) TYPE REF TO cl_abap_zip.
    METHODS on_selection_changed
                FOR EVENT selection_changed OF cl_tree_control_base
      IMPORTING node_key.

    DATA: sscrfields           TYPE REF TO sscrfields,
          go_docking_container TYPE REF TO cl_gui_docking_container,
          xdata                TYPE xstring,
          t_rawdata            TYPE solix_tab,
          bytecount            TYPE i,
          go_tree              TYPE REF TO cl_gui_simple_tree,
          gt_tree              TYPE ty_tree_nodes,
          diff_files           TYPE lcl_app=>ty_diff_files,
          tree_file_links      TYPE ty_tree_file_links,
          zip_old              TYPE REF TO cl_abap_zip,
          zip_new              TYPE REF TO cl_abap_zip,
          temp_dir             TYPE string.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD at_selection_screen_output.

    IF go_docking_container IS NOT BOUND.

      CREATE OBJECT go_docking_container
        EXPORTING
          repid     = sy-repid
          dynnr     = sy-dynnr
          side      = go_docking_container->dock_at_left
          extension = 300
        EXCEPTIONS
          OTHERS    = 6.

      CALL METHOD cl_gui_frontend_services=>get_temp_directory
        CHANGING
          temp_dir     = temp_dir
        EXCEPTIONS
          cntl_error   = 1
          error_no_gui = 2.
      IF sy-subrc <> 0.
* Error handling
      ENDIF.
* flush to send previous call to frontend
      CALL METHOD cl_gui_cfw=>flush
        EXCEPTIONS
          cntl_system_error = 1
          cntl_error        = 2
          OTHERS            = 3.
      IF sy-subrc <> 0.
* Error handling
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD at_selection_screen.

    CASE sscrfields->ucomm.

      WHEN 'ONLI'.

        DATA: lt_dummy   TYPE TABLE OF rsparams,
              selections TYPE TABLE OF rsparamsl_255.

        CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
          EXPORTING
            curr_report         = sy-repid
          TABLES
            selection_table     = lt_dummy
            selection_table_255 = selections
          EXCEPTIONS
            not_found           = 1
            no_report           = 2
            OTHERS              = 3.

        IF line_exists( selections[ selname = 'P_FILES' low = abap_true ] ).

          zip_old = get_zip_from_xstring( load_binary_file( selections[ selname = 'P_FILE1' ]-low ) ).
          zip_new = get_zip_from_xstring( load_binary_file( selections[ selname = 'P_FILE2' ]-low ) ).

          diff_files = compare_zip_files( zip_1 = zip_old zip_2 = zip_new ).
          display_tree( go_docking_container ).

        ELSE.

          DATA(selection) = VALUE #( selections[ low = abap_true ] OPTIONAL ).
          ASSERT selection(6) = 'P_DEMO'.
          DATA(class_name) = 'LCL_' && selection-selname+2.

          DATA demo TYPE REF TO lif_demo.
          CREATE OBJECT demo TYPE (class_name).
          zip_old = get_zip_from_xstring( demo->generate( iv_huge = abap_false ) ).
          zip_new = get_zip_from_xstring( demo->generate( iv_huge = abap_true ) ).

          diff_files = compare_zip_files( zip_1 = zip_old zip_2 = zip_new ).
          display_tree( go_docking_container ).

        ENDIF.

        " don't go to START-OF-SELECTION otherwise the program is restarted
        CLEAR sscrfields->ucomm.

    ENDCASE.

  ENDMETHOD.



  METHOD load_binary_file.
    DATA l_filename TYPE string.
    DATA l_length TYPE i.
    DATA lt_x255 TYPE TABLE OF x255.

    l_filename = path.

    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename   = l_filename
        filetype   = 'BIN'
      IMPORTING
        filelength = l_length
      CHANGING
        data_tab   = lt_x255
      EXCEPTIONS
        OTHERS     = 1.

    IF sy-subrc = 0.

      CALL METHOD cl_swf_utl_convert_xstring=>table_to_xstring
        EXPORTING
          i_table  = lt_x255
          i_size   = l_length
        RECEIVING
          r_stream = content
        EXCEPTIONS
          OTHERS   = 3.

    ENDIF.
  ENDMETHOD.



  METHOD compare_zip_files.

    DATA: diff_file TYPE lcl_app=>ty_diff_file.
    FIELD-SYMBOLS:
        <sub_files> TYPE ty_diff_files.

    DATA(zip_1_files) = get_zip_files_at_path( files = zip_1->files path = path ).
    DATA(zip_2_files) = get_zip_files_at_path( files = zip_2->files path = path ).

    DATA(zip_1_index) = 0.
    DATA(zip_2_index) = 0.
    DATA(zip_1_read) = abap_true.
    DATA(zip_2_read) = abap_true.

    WHILE zip_1_read = abap_true OR zip_2_read = abap_true.

      IF zip_1_read = abap_true.
        zip_1_index = zip_1_index + 1.
        IF zip_1_index > lines( zip_1_files ).
          zip_1_read = abap_false.
        ELSE.
          DATA(file_1) = REF #( zip_1_files[ zip_1_index ] OPTIONAL ).
        ENDIF.
      ENDIF.
      IF zip_2_read = abap_true.
        zip_2_index = zip_2_index + 1.
        IF zip_2_index > lines( zip_2_files ).
          zip_2_read = abap_false.
        ELSE.
          DATA(file_2) = REF #( zip_2_files[ zip_2_index ] OPTIONAL ).
        ENDIF.
      ENDIF.

      CLEAR diff_file.
      IF zip_1_index <= lines( zip_1_files ) AND zip_2_index <= lines( zip_2_files ).
        zip_1_read = abap_false.
        zip_2_read = abap_false.
        IF file_1->* < file_2->*.
          diff_file = VALUE #( name = file_1->* diff = 'only in 1' ).
          zip_1_read = abap_true.
        ELSEIF file_1->* > file_2->*.
          diff_file = VALUE #( name = file_2->* diff = 'only in 2' ).
          zip_2_read = abap_true.
        ELSE.
          " same file -> compare the contents of the 2 files
          zip_1_read = abap_true.
          zip_2_read = abap_true.
          IF contains( val = file_1->* end = '/' ).
            " folder
            diff_file = VALUE #( name = file_1->* diff = 'same' ).
          ELSE.
            diff_file = VALUE #(
                name = file_1->*
                diff = compare_files( zip_1 = zip_1 zip_2 = zip_2 name_1 = |{ path }{ file_1->* }| name_2 = |{ path }{ file_2->* }| ) ).
          ENDIF.
        ENDIF.
      ELSEIF zip_1_index <= lines( zip_1_files ).
        diff_file = VALUE #( name = file_1->* diff = 'only in 1' ).
        zip_1_read = abap_true.
      ELSEIF zip_2_index <= lines( zip_2_files ).
        diff_file = VALUE #( name = file_2->* diff = 'only in 2' ).
        zip_2_read = abap_true.
      ENDIF.

      IF diff_file IS NOT INITIAL.
        IF contains( val = diff_file-name end = '/' ).
          diff_file-is_folder = abap_true.
        ENDIF.
        IF diff_file-diff = 'same' AND diff_file-is_folder = abap_true.
          CREATE DATA diff_file-sub_files TYPE ty_diff_files.
          ASSIGN diff_file-sub_files->* TO <sub_files>.
          <sub_files> = compare_zip_files( zip_1 = zip_1 zip_2 = zip_2 path = |{ path }{ diff_file-name }| ).
          LOOP AT <sub_files> TRANSPORTING NO FIELDS WHERE diff <> 'same'.
            EXIT.
          ENDLOOP.
          IF sy-subrc = 0.
            diff_file-diff = 'diff'.
          ENDIF.
        ENDIF.
        INSERT diff_file INTO TABLE diff_files.
      ENDIF.

    ENDWHILE.

  ENDMETHOD.

  METHOD get_zip_files_at_path.

    DATA: starter        TYPE string,
          file_or_folder TYPE string VALUE '^[^/]*(?:/|$)'.

    LOOP AT files ASSIGNING FIELD-SYMBOL(<file>).
      IF path = ``.
        starter = substring_to( val = <file>-name regex = file_or_folder ).
        COLLECT starter INTO path_files.
      ELSEIF contains( val = <file>-name start = path ).
        starter = substring_to( val = substring( val = <file>-name off = strlen( path ) ) regex = file_or_folder ).
        COLLECT starter INTO path_files.
      ENDIF.
    ENDLOOP.

    SORT path_files BY table_line.

  ENDMETHOD.


  METHOD compare_files.

    zip_1->get(
      EXPORTING
        name                    = name_1
      IMPORTING
        content                 = DATA(content_1)
      EXCEPTIONS
        zip_index_error         = 1
        zip_decompression_error = 2
        OTHERS                  = 3 ).
    IF sy-subrc <> 0.
      " todo
    ENDIF.
    zip_2->get(
      EXPORTING
        name                    = name_2
      IMPORTING
        content                 = DATA(content_2)
      EXCEPTIONS
        zip_index_error         = 1
        zip_decompression_error = 2
        OTHERS                  = 3 ).
    IF sy-subrc <> 0.
      " todo
    ENDIF.

    diff = COND #( WHEN content_1 <> content_2 THEN 'diff' ELSE 'same' ).

  ENDMETHOD.

  METHOD set_sscrfields.

    me->sscrfields = sscrfields.

  ENDMETHOD.


  METHOD display_tree.

    CLEAR gt_tree.

    APPEND INITIAL LINE TO gt_tree ASSIGNING FIELD-SYMBOL(<gs_tree>).
    <gs_tree>-node_key = '1'.
    <gs_tree>-text = 'root'.
    <gs_tree>-isfolder = 'X'.

    tree_file_links = VALUE #( ).
    add_tree_nodes(
        EXPORTING diff_files      = diff_files
                  path            = ''
                  parent_node_key = '1'
        CHANGING  tree_nodes      = gt_tree ).

    IF go_tree IS NOT BOUND.
      CREATE OBJECT go_tree
        EXPORTING
          parent                      = io_container
          node_selection_mode         = cl_gui_simple_tree=>node_sel_mode_single
        EXCEPTIONS
          lifetime_error              = 1
          cntl_system_error           = 2
          create_error                = 3
          failed                      = 4
          illegal_node_selection_mode = 5
          OTHERS                      = 6.
      IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ELSE.
      go_tree->delete_all_nodes(
        EXCEPTIONS
          failed            = 1
          cntl_system_error = 2
          OTHERS            = 3 ).
      IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
    go_tree->add_nodes(
        EXPORTING
          table_structure_name = 'IXMLTREE1'
          node_table           = gt_tree
        EXCEPTIONS
          error_in_node_table            = 1
          failed                         = 2
          dp_error                       = 3
          table_structure_name_not_found = 4
          OTHERS                         = 5 ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    go_tree->expand_root_nodes(
      EXPORTING
        expand_subtree      = abap_true
      EXCEPTIONS
        failed              = 1
        illegal_level_count = 2
        cntl_system_error   = 3
        OTHERS              = 4 ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    SET HANDLER on_selection_changed FOR go_tree.
    go_tree->set_registered_events(
      EXPORTING
        events                    = VALUE #( ( eventid = go_tree->eventid_selection_changed appl_event = abap_false ) )
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3
        OTHERS                    = 4 ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    cl_gui_cfw=>update_view(
*      EXPORTING
*        called_by_system  =     " Internal Use! Do not deliver!
      EXCEPTIONS
        cntl_system_error = 1
        cntl_error        = 2
        OTHERS            = 3 ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD add_tree_nodes.

    FIELD-SYMBOLS:
        <sub_files> TYPE ty_diff_files.

    LOOP AT diff_files REFERENCE INTO DATA(diff_file).

      APPEND INITIAL LINE TO tree_nodes ASSIGNING FIELD-SYMBOL(<gs_tree>).
      <gs_tree>-node_key = |{ sy-tabix }|.
      <gs_tree>-relatkey = parent_node_key.
      <gs_tree>-relatship = cl_tree_control_base=>relat_last_child.
      <gs_tree>-text = diff_file->name.
      <gs_tree>-style = SWITCH #( diff_file->diff
          WHEN 'only in 1' THEN cl_tree_control_base=>style_emphasized_negative
          WHEN 'only in 2' THEN cl_tree_control_base=>style_emphasized_positive
          WHEN 'diff' THEN COND #( WHEN diff_file->is_folder = abap_false
                                   THEN cl_tree_control_base=>style_emphasized
                                   ELSE cl_tree_control_base=>style_intensified )
          ELSE cl_tree_control_base=>style_default ).

      tree_file_links = VALUE #( BASE tree_file_links
        ( node_key  = <gs_tree>-node_key
          diff      = diff_file->diff
          file_path = path && diff_file->name ) ).


      IF diff_file->sub_files IS BOUND.
        ASSIGN diff_file->sub_files->* TO <sub_files>.
        IF lines( <sub_files> ) > 0.
          <gs_tree>-isfolder = abap_true.
        ENDIF.
        add_tree_nodes(
            EXPORTING diff_files      = <sub_files>
                      path            = path && diff_file->name
                      parent_node_key = <gs_tree>-node_key
            CHANGING  tree_nodes      = gt_tree ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD on_selection_changed.
    DATA: content      TYPE xstring,
          solix_tab    TYPE solix_tab,
          xml_document TYPE REF TO if_ixml_document.

    ASSIGN tree_file_links[ node_key = node_key ] TO FIELD-SYMBOL(<tree_file_link>).
    IF <tree_file_link>-diff = 'diff'.

      zip_old->get(
        EXPORTING
          name                    = <tree_file_link>-file_path
        IMPORTING
          content                 = content
        EXCEPTIONS
          zip_index_error         = 1
          zip_decompression_error = 2
          OTHERS                  = 3 ).
      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      SPLIT <tree_file_link>-file_path AT '/' INTO TABLE DATA(parts).
      DATA(new_parts) = VALUE string_table( ).
      LOOP AT parts REFERENCE INTO DATA(part).
        INSERT part->* INTO new_parts INDEX 1.
      ENDLOOP.
      DATA(file_old) = temp_dir && '\old_' && concat_lines_of( table = new_parts sep = '_' ) && '.xml'.
      IF <tree_file_link>-file_path CS '.xml'.
        xml_pretty_print( CHANGING c_content = content ).
      ENDIF.
      gui_download(
            i_content   = content
            i_file_path = file_old ).

      zip_new->get(
        EXPORTING
          name                    = <tree_file_link>-file_path
        IMPORTING
          content                 = content
        EXCEPTIONS
          zip_index_error         = 1
          zip_decompression_error = 2
          OTHERS                  = 3 ).
      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      DATA(file_new) = temp_dir && '\new_' && concat_lines_of( table = new_parts sep = '_' ) && '.xml'.
      IF <tree_file_link>-file_path CS '.xml'.
        xml_pretty_print( CHANGING c_content = content ).
      ENDIF.
      gui_download(
            i_content   = content
            i_file_path = file_new ).

*      cl_gui_frontend_services=>execute( document = file_new ).
      cl_gui_frontend_services=>execute(
            application = 'code'
            parameter   = |-d "{ file_old }" "{ file_new }"|
            minimized   = 'X'
            synchronous = '' ).

    ENDIF.


  ENDMETHOD.


  METHOD gui_download.

    DATA(solix_tab) = cl_bcs_convert=>xstring_to_solix( i_content ).

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = xstrlen( i_content )
        filename                  = i_file_path
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = solix_tab
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24
    ).
    IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD xml_pretty_print.

    DATA xml_document TYPE REF TO if_ixml_document.

    CALL FUNCTION 'SDIXML_XML_TO_DOM'
      EXPORTING
        xml      = c_content
      IMPORTING
        document = xml_document
      EXCEPTIONS
        OTHERS   = 1.
    CALL FUNCTION 'SDIXML_DOM_TO_XML'
      EXPORTING
        document      = xml_document
        pretty_print  = abap_true
      IMPORTING
        xml_as_string = c_content
      EXCEPTIONS
        OTHERS        = 2.

  ENDMETHOD.


  METHOD get_zip_from_xstring.

    CREATE OBJECT ro_zip.
    CALL METHOD ro_zip->load
      EXPORTING
        zip             = xdata
      EXCEPTIONS
        zip_parse_error = 1
        OTHERS          = 2.

  ENDMETHOD.

ENDCLASS.

CLASS ltc_compare_zip_files DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS only_in_one FOR TESTING.
    METHODS only_in_two FOR TESTING.
    METHODS identical_file FOR TESTING.
    METHODS different_file FOR TESTING.
    METHODS folder FOR TESTING RAISING cx_static_check.
    METHODS one_more_in_one FOR TESTING RAISING cx_static_check.
    TYPES ty_diff_files TYPE lcl_app=>ty_diff_files.
    TYPES ty_ref_sub_files TYPE REF TO lcl_app=>ty_diff_files.
    DATA ref_sub_files TYPE ty_ref_sub_files.
ENDCLASS.

CLASS ltc_compare_zip_files IMPLEMENTATION.
  METHOD only_in_one.
    DATA(zip_1) = NEW cl_abap_zip( ).
    zip_1->add( name = 'file1.txt' content = cl_abap_codepage=>convert_to( 'abcdef' ) ).
    zip_1->add( name = 'file2.txt' content = cl_abap_codepage=>convert_to( 'abcdef' ) ).
    DATA(zip_2) = NEW cl_abap_zip( ).
    DATA(diff_files) = NEW lcl_app( )->compare_zip_files( zip_1 = zip_1 zip_2 = zip_2 path = `` ).
    cl_abap_unit_assert=>assert_equals( act = diff_files exp = VALUE lcl_app=>ty_diff_files(
        ( name = 'file1.txt' diff = 'only in 1' )
        ( name = 'file2.txt' diff = 'only in 1' ) ) ).
  ENDMETHOD.
  METHOD only_in_two.
    DATA(zip_1) = NEW cl_abap_zip( ).
    DATA(zip_2) = NEW cl_abap_zip( ).
    zip_2->add( name = 'file1.txt' content = cl_abap_codepage=>convert_to( 'abcdef' ) ).
    zip_2->add( name = 'file2.txt' content = cl_abap_codepage=>convert_to( 'abcdef' ) ).
    DATA(diff_files) = NEW lcl_app( )->compare_zip_files( zip_1 = zip_1 zip_2 = zip_2 path = `` ).
    cl_abap_unit_assert=>assert_equals( act = diff_files exp = VALUE lcl_app=>ty_diff_files(
        ( name = 'file1.txt' diff = 'only in 2' )
        ( name = 'file2.txt' diff = 'only in 2' ) ) ).
  ENDMETHOD.
  METHOD identical_file.
    DATA(zip_1) = NEW cl_abap_zip( ).
    zip_1->add( name = 'file.txt' content = cl_abap_codepage=>convert_to( 'abcdef' ) ).
    DATA(zip_2) = NEW cl_abap_zip( ).
    zip_2->add( name = 'file.txt' content = cl_abap_codepage=>convert_to( 'abcdef' ) ).
    DATA(diff_files) = NEW lcl_app( )->compare_zip_files( zip_1 = zip_1 zip_2 = zip_2 path = `` ).
    cl_abap_unit_assert=>assert_equals( act = diff_files exp = VALUE lcl_app=>ty_diff_files(
        ( name = 'file.txt' diff = 'same' ) ) ).
  ENDMETHOD.
  METHOD different_file.
    DATA(zip_1) = NEW cl_abap_zip( ).
    zip_1->add( name = 'file.txt' content = cl_abap_codepage=>convert_to( 'abcdef' ) ).
    DATA(zip_2) = NEW cl_abap_zip( ).
    zip_2->add( name = 'file.txt' content = cl_abap_codepage=>convert_to( 'abcdefghij' ) ).
    DATA(diff_files) = NEW lcl_app( )->compare_zip_files( zip_1 = zip_1 zip_2 = zip_2 path = `` ).
    cl_abap_unit_assert=>assert_equals( act = diff_files exp = VALUE lcl_app=>ty_diff_files(
        ( name = 'file.txt' diff = 'diff' ) ) ).
  ENDMETHOD.
  METHOD one_more_in_one.
    DATA(zip_1) = NEW cl_abap_zip( ).
    zip_1->add( name = 'file1.txt' content = cl_abap_codepage=>convert_to( 'abcdef' ) ).
    zip_1->add( name = 'file2.txt' content = cl_abap_codepage=>convert_to( 'abcdef' ) ).
    zip_1->add( name = 'file3.txt' content = cl_abap_codepage=>convert_to( 'abcdef' ) ).
    DATA(zip_2) = NEW cl_abap_zip( ).
    zip_2->add( name = 'file1.txt' content = cl_abap_codepage=>convert_to( 'abcdef' ) ).
    zip_2->add( name = 'file3.txt' content = cl_abap_codepage=>convert_to( 'abcdef' ) ).
    DATA(diff_files) = NEW lcl_app( )->compare_zip_files( zip_1 = zip_1 zip_2 = zip_2 path = `` ).
    cl_abap_unit_assert=>assert_equals( act = diff_files exp = VALUE lcl_app=>ty_diff_files(
        ( name = 'file1.txt' diff = 'same' )
        ( name = 'file2.txt' diff = 'only in 1' )
        ( name = 'file3.txt' diff = 'same' ) ) ).
  ENDMETHOD.
  METHOD folder.
    DATA(zip_1) = NEW cl_abap_zip( ).
    zip_1->add( name = 'test/file.txt' content = cl_abap_codepage=>convert_to( 'abcdef' ) ).
    DATA(zip_2) = NEW cl_abap_zip( ).
    zip_2->add( name = 'test/file.txt' content = cl_abap_codepage=>convert_to( 'abcdefghij' ) ).
    DATA(diff_files) = NEW lcl_app( )->compare_zip_files( zip_1 = zip_1 zip_2 = zip_2 path = `` ).
    cl_abap_unit_assert=>assert_equals(
        act = CORRESPONDING ty_diff_files( diff_files EXCEPT sub_files )
        exp = VALUE ty_diff_files(
            ( name = 'test/' diff = 'diff' is_folder = 'X' ) ) ).
    ref_sub_files = CAST #( diff_files[ name = 'test/' ]-sub_files ).
    cl_abap_unit_assert=>assert_equals(
        act = ref_sub_files->*
        exp = VALUE ty_diff_files(
            ( name = 'file.txt' diff = 'diff' ) ) ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_get_zip_files_at_path DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS level_1 FOR TESTING.
    METHODS level_2 FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltc_get_zip_files_at_path IMPLEMENTATION.

  METHOD level_1.
    DATA(path_files) = NEW lcl_app( )->get_zip_files_at_path( path = || files = VALUE #(
        ( name = |zfile.txt| )
        ( name = |test/file1.txt| )
        ( name = |test/file2.txt| ) ) ).
    cl_abap_unit_assert=>assert_equals( act = path_files exp = VALUE string_table(
        ( |test/| )
        ( |zfile.txt| ) ) ).
  ENDMETHOD.

  METHOD level_2.
    DATA(path_files) = NEW lcl_app( )->get_zip_files_at_path( path = |test/| files = VALUE #(
        ( name = |zfile.txt| )
        ( name = |test/test/file.txt| )
        ( name = |test/file1.txt| )
        ( name = |test/file2.txt| ) ) ).
    cl_abap_unit_assert=>assert_equals( act = path_files exp = VALUE string_table(
        ( |file1.txt| )
        ( |file2.txt| )
        ( |test/| ) ) ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_demo1 DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_demo.
ENDCLASS.

CLASS lcl_demo1 IMPLEMENTATION.

  METHOD lif_demo~generate.

    DATA: lo_excel     TYPE REF TO zcl_excel,
          lo_worksheet TYPE REF TO zcl_excel_worksheet,
          lo_hyperlink TYPE REF TO zcl_excel_hyperlink,
          lo_column    TYPE REF TO zcl_excel_column,
          lo_writer    TYPE REF TO zif_excel_writer.

    CREATE OBJECT lo_excel.

    " Get active sheet
    lo_worksheet = lo_excel->get_active_worksheet( ).
*  lo_worksheet->set_title( ip_title = 'Sheet1' ).
    lo_worksheet->set_cell( ip_column = 'B' ip_row = 2 ip_value = 'Hello world' ).
    lo_worksheet->set_cell( ip_column = 'B' ip_row = 3 ip_value = sy-datum ).
    lo_worksheet->set_cell( ip_column = 'C' ip_row = 3 ip_value = sy-uzeit ).
    lo_hyperlink = zcl_excel_hyperlink=>create_external_link( iv_url = 'https://sapmentors.github.io/abap2xlsx' ).
    lo_worksheet->set_cell( ip_column = 'B' ip_row = 4 ip_value = 'Click here to visit abap2xlsx homepage' ip_hyperlink = lo_hyperlink ).

    lo_worksheet->set_cell( ip_column = 'B' ip_row =  6 ip_value = '你好，世界' ).
    lo_worksheet->set_cell( ip_column = 'C' ip_row =  6 ip_value = '(Chinese)' ).
    lo_worksheet->set_cell( ip_column = 'B' ip_row =  7 ip_value = 'नमस्ते दुनिया' ).
    lo_worksheet->set_cell( ip_column = 'C' ip_row =  7 ip_value = '(Hindi)' ).
    lo_worksheet->set_cell( ip_column = 'B' ip_row =  8 ip_value = 'Hola Mundo' ).
    lo_worksheet->set_cell( ip_column = 'C' ip_row =  8 ip_value = '(Spanish)' ).
    lo_worksheet->set_cell( ip_column = 'B' ip_row =  9 ip_value = 'مرحبا بالعالم' ).
    lo_worksheet->set_cell( ip_column = 'C' ip_row =  9 ip_value = '(Arabic)' ).
    lo_worksheet->set_cell( ip_column = 'B' ip_row = 10 ip_value = 'ওহে বিশ্ব ' ).
    lo_worksheet->set_cell( ip_column = 'C' ip_row = 10 ip_value = '(Bengali)' ).
    lo_worksheet->set_cell( ip_column = 'B' ip_row = 11 ip_value = 'Bonjour le monde' ).
    lo_worksheet->set_cell( ip_column = 'C' ip_row = 11 ip_value = '(French)' ).
    lo_worksheet->set_cell( ip_column = 'B' ip_row = 12 ip_value = 'Olá Mundo' ).
    lo_worksheet->set_cell( ip_column = 'C' ip_row = 12 ip_value = '(Portuguese)' ).
    lo_worksheet->set_cell( ip_column = 'B' ip_row = 13 ip_value = 'Привет, мир' ).
    lo_worksheet->set_cell( ip_column = 'C' ip_row = 13 ip_value = '(Russian)' ).
    lo_worksheet->set_cell( ip_column = 'B' ip_row = 14 ip_value = 'ہیلو دنیا' ).
    lo_worksheet->set_cell( ip_column = 'C' ip_row = 14 ip_value = '(Urdu)' ).
    lo_worksheet->set_cell( ip_column = 'B' ip_row = 15 ip_value = '👋🌎, 👋🌍, 👋🌏' ).
    lo_worksheet->set_cell( ip_column = 'C' ip_row = 15 ip_value = '(Emoji waving hand + 3 parts of the world)' ).

    lo_column = lo_worksheet->get_column( ip_column = 'B' ).
    lo_column->set_width( ip_width = 11 ).

    IF iv_huge = abap_false.
      CREATE OBJECT lo_writer TYPE zcl_excel_writer_2007.
    ELSE.
      CREATE OBJECT lo_writer TYPE zcl_excel_writer_huge_file.
    ENDIF.
    rv_xlsx = lo_writer->write_file( lo_excel ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_demo2 DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_demo.
ENDCLASS.

CLASS lcl_demo2 IMPLEMENTATION.

  METHOD lif_demo~generate.

    DATA: lo_excel           TYPE REF TO zcl_excel,
          lo_worksheet       TYPE REF TO zcl_excel_worksheet,
          lo_style_bold      TYPE REF TO zcl_excel_style,
          lo_style_underline TYPE REF TO zcl_excel_style,
          lo_style_filled    TYPE REF TO zcl_excel_style,
          lo_style_border    TYPE REF TO zcl_excel_style,
          lo_style_button    TYPE REF TO zcl_excel_style,
          lo_border_dark     TYPE REF TO zcl_excel_style_border,
          lo_border_light    TYPE REF TO zcl_excel_style_border.

    DATA: lv_style_bold_guid             TYPE zexcel_cell_style,
          lv_style_underline_guid        TYPE zexcel_cell_style,
          lv_style_filled_guid           TYPE zexcel_cell_style,
          lv_style_filled_green_guid     TYPE zexcel_cell_style,
          lv_style_border_guid           TYPE zexcel_cell_style,
          lv_style_button_guid           TYPE zexcel_cell_style,
          lv_style_filled_turquoise_guid TYPE zexcel_cell_style,
          lv_style_gr_cornerlb_guid      TYPE zexcel_cell_style,
          lv_style_gr_cornerlt_guid      TYPE zexcel_cell_style,
          lv_style_gr_cornerrb_guid      TYPE zexcel_cell_style,
          lv_style_gr_cornerrt_guid      TYPE zexcel_cell_style,
          lv_style_gr_horizontal90_guid  TYPE zexcel_cell_style,
          lv_style_gr_horizontal270_guid TYPE zexcel_cell_style,
          lv_style_gr_horizontalb_guid   TYPE zexcel_cell_style,
          lv_style_gr_vertical_guid      TYPE zexcel_cell_style,
          lv_style_gr_vertical2_guid     TYPE zexcel_cell_style,
          lv_style_gr_fromcenter_guid    TYPE zexcel_cell_style,
          lv_style_gr_diagonal45_guid    TYPE zexcel_cell_style,
          lv_style_gr_diagonal45b_guid   TYPE zexcel_cell_style,
          lv_style_gr_diagonal135_guid   TYPE zexcel_cell_style,
          lv_style_gr_diagonal135b_guid  TYPE zexcel_cell_style,
          lv_file                        TYPE xstring,
          lv_bytecount                   TYPE i,
          lt_file_tab                    TYPE solix_tab,
          lv_full_path                   TYPE string,
          lv_workdir                     TYPE string,
          lv_file_separator              TYPE c,
          lo_row                         TYPE REF TO zcl_excel_row,
          lo_writer                      TYPE REF TO zif_excel_writer.

    CREATE OBJECT lo_excel.

    " Create border object
    CREATE OBJECT lo_border_dark.
    lo_border_dark->border_color-rgb = zcl_excel_style_color=>c_black.
    lo_border_dark->border_style = zcl_excel_style_border=>c_border_thin.
    CREATE OBJECT lo_border_light.
    lo_border_light->border_color-rgb = zcl_excel_style_color=>c_gray.
    lo_border_light->border_style = zcl_excel_style_border=>c_border_thin.
    " Create a bold / italic style
    lo_style_bold               = lo_excel->add_new_style( ).
    lo_style_bold->font->bold   = abap_true.
    lo_style_bold->font->italic = abap_true.
    lo_style_bold->font->name   = zcl_excel_style_font=>c_name_arial.
    lo_style_bold->font->scheme = zcl_excel_style_font=>c_scheme_none.
    lo_style_bold->font->color-rgb  = zcl_excel_style_color=>c_red.
    lv_style_bold_guid          = lo_style_bold->get_guid( ).
    " Create an underline double style
    lo_style_underline                        = lo_excel->add_new_style( ).
    lo_style_underline->font->underline       = abap_true.
    lo_style_underline->font->underline_mode  = zcl_excel_style_font=>c_underline_double.
    lo_style_underline->font->name            = zcl_excel_style_font=>c_name_roman.
    lo_style_underline->font->scheme          = zcl_excel_style_font=>c_scheme_none.
    lo_style_underline->font->family          = zcl_excel_style_font=>c_family_roman.
    lv_style_underline_guid                   = lo_style_underline->get_guid( ).
    " Create filled style yellow
    lo_style_filled                 = lo_excel->add_new_style( ).
    lo_style_filled->fill->filltype = zcl_excel_style_fill=>c_fill_solid.
    lo_style_filled->fill->fgcolor-theme  = zcl_excel_style_color=>c_theme_accent6.
    lv_style_filled_guid            = lo_style_filled->get_guid( ).
    " Create border with button effects
    lo_style_button                   = lo_excel->add_new_style( ).
    lo_style_button->borders->right   = lo_border_dark.
    lo_style_button->borders->down    = lo_border_dark.
    lo_style_button->borders->left    = lo_border_light.
    lo_style_button->borders->top     = lo_border_light.
    lv_style_button_guid              = lo_style_button->get_guid( ).
    "Create style with border
    lo_style_border                         = lo_excel->add_new_style( ).
    lo_style_border->borders->allborders    = lo_border_dark.
    lo_style_border->borders->diagonal      = lo_border_dark.
    lo_style_border->borders->diagonal_mode = zcl_excel_style_borders=>c_diagonal_both.
    lv_style_border_guid                    = lo_style_border->get_guid( ).
    " Create filled style green
    lo_style_filled                     = lo_excel->add_new_style( ).
    lo_style_filled->fill->filltype     = zcl_excel_style_fill=>c_fill_solid.
    lo_style_filled->fill->fgcolor-rgb  = zcl_excel_style_color=>c_green.
    lo_style_filled->font->name         = zcl_excel_style_font=>c_name_cambria.
    lo_style_filled->font->scheme       = zcl_excel_style_font=>c_scheme_major.
    lv_style_filled_green_guid          = lo_style_filled->get_guid( ).

    " Create filled with gradients
    lo_style_filled                     = lo_excel->add_new_style( ).
    lo_style_filled->fill->filltype     = zcl_excel_style_fill=>c_fill_gradient_cornerlb.
    lo_style_filled->fill->fgcolor-rgb  = zcl_excel_style_color=>c_blue.
    lo_style_filled->fill->bgcolor-rgb  = zcl_excel_style_color=>c_white.
    lo_style_filled->font->name         = zcl_excel_style_font=>c_name_cambria.
    lo_style_filled->font->scheme       = zcl_excel_style_font=>c_scheme_major.
    lv_style_gr_cornerlb_guid           = lo_style_filled->get_guid( ).

    lo_style_filled                     = lo_excel->add_new_style( ).
    lo_style_filled->fill->filltype     = zcl_excel_style_fill=>c_fill_gradient_cornerlt.
    lo_style_filled->fill->fgcolor-rgb  = zcl_excel_style_color=>c_blue.
    lo_style_filled->fill->bgcolor-rgb  = zcl_excel_style_color=>c_white.
    lo_style_filled->font->name         = zcl_excel_style_font=>c_name_cambria.
    lo_style_filled->font->scheme       = zcl_excel_style_font=>c_scheme_major.
    lv_style_gr_cornerlt_guid           = lo_style_filled->get_guid( ).

    lo_style_filled                     = lo_excel->add_new_style( ).
    lo_style_filled->fill->filltype     = zcl_excel_style_fill=>c_fill_gradient_cornerrb.
    lo_style_filled->fill->fgcolor-rgb  = zcl_excel_style_color=>c_blue.
    lo_style_filled->fill->bgcolor-rgb  = zcl_excel_style_color=>c_white.
    lo_style_filled->font->name         = zcl_excel_style_font=>c_name_cambria.
    lo_style_filled->font->scheme       = zcl_excel_style_font=>c_scheme_major.
    lv_style_gr_cornerrb_guid           = lo_style_filled->get_guid( ).

    lo_style_filled                     = lo_excel->add_new_style( ).
    lo_style_filled->fill->filltype     = zcl_excel_style_fill=>c_fill_gradient_cornerrt.
    lo_style_filled->fill->fgcolor-rgb  = zcl_excel_style_color=>c_blue.
    lo_style_filled->fill->bgcolor-rgb  = zcl_excel_style_color=>c_white.
    lo_style_filled->font->name         = zcl_excel_style_font=>c_name_cambria.
    lo_style_filled->font->scheme       = zcl_excel_style_font=>c_scheme_major.
    lv_style_gr_cornerrt_guid           = lo_style_filled->get_guid( ).

    lo_style_filled                     = lo_excel->add_new_style( ).
    lo_style_filled->fill->filltype     = zcl_excel_style_fill=>c_fill_gradient_horizontal90.
    lo_style_filled->fill->fgcolor-rgb  = zcl_excel_style_color=>c_blue.
    lo_style_filled->fill->bgcolor-rgb  = zcl_excel_style_color=>c_white.
    lo_style_filled->font->name         = zcl_excel_style_font=>c_name_cambria.
    lo_style_filled->font->scheme       = zcl_excel_style_font=>c_scheme_major.
    lv_style_gr_horizontal90_guid       = lo_style_filled->get_guid( ).

    lo_style_filled                     = lo_excel->add_new_style( ).
    lo_style_filled->fill->filltype     = zcl_excel_style_fill=>c_fill_gradient_horizontal270.
    lo_style_filled->fill->fgcolor-rgb  = zcl_excel_style_color=>c_blue.
    lo_style_filled->fill->bgcolor-rgb  = zcl_excel_style_color=>c_white.
    lo_style_filled->font->name         = zcl_excel_style_font=>c_name_cambria.
    lo_style_filled->font->scheme       = zcl_excel_style_font=>c_scheme_major.
    lv_style_gr_horizontal270_guid      = lo_style_filled->get_guid( ).


    lo_style_filled                     = lo_excel->add_new_style( ).
    lo_style_filled->fill->filltype     = zcl_excel_style_fill=>c_fill_gradient_horizontalb.
    lo_style_filled->fill->fgcolor-rgb  = zcl_excel_style_color=>c_blue.
    lo_style_filled->fill->bgcolor-rgb  = zcl_excel_style_color=>c_white.
    lo_style_filled->font->name         = zcl_excel_style_font=>c_name_cambria.
    lo_style_filled->font->scheme       = zcl_excel_style_font=>c_scheme_major.
    lv_style_gr_horizontalb_guid        = lo_style_filled->get_guid( ).


    lo_style_filled                     = lo_excel->add_new_style( ).
    lo_style_filled->fill->filltype     = zcl_excel_style_fill=>c_fill_gradient_vertical.
    lo_style_filled->fill->fgcolor-rgb  = zcl_excel_style_color=>c_blue.
    lo_style_filled->fill->bgcolor-rgb  = zcl_excel_style_color=>c_white.
    lo_style_filled->font->name         = zcl_excel_style_font=>c_name_cambria.
    lo_style_filled->font->scheme       = zcl_excel_style_font=>c_scheme_major.
    lv_style_gr_vertical_guid           = lo_style_filled->get_guid( ).



    lo_style_filled                     = lo_excel->add_new_style( ).
    lo_style_filled->fill->filltype     = zcl_excel_style_fill=>c_fill_gradient_vertical.
    lo_style_filled->fill->fgcolor-rgb  = zcl_excel_style_color=>c_white.
    lo_style_filled->fill->bgcolor-rgb  = zcl_excel_style_color=>c_blue.
    lo_style_filled->font->name         = zcl_excel_style_font=>c_name_cambria.
    lo_style_filled->font->scheme       = zcl_excel_style_font=>c_scheme_major.
    lv_style_gr_vertical2_guid          = lo_style_filled->get_guid( ).


    lo_style_filled                     = lo_excel->add_new_style( ).
    lo_style_filled->fill->filltype     = zcl_excel_style_fill=>c_fill_gradient_fromcenter.
    lo_style_filled->fill->fgcolor-rgb  = zcl_excel_style_color=>c_blue.
    lo_style_filled->fill->bgcolor-rgb  = zcl_excel_style_color=>c_white.
    lo_style_filled->font->name         = zcl_excel_style_font=>c_name_cambria.
    lo_style_filled->font->scheme       = zcl_excel_style_font=>c_scheme_major.
    lv_style_gr_fromcenter_guid         = lo_style_filled->get_guid( ).


    lo_style_filled                     = lo_excel->add_new_style( ).
    lo_style_filled->fill->filltype     = zcl_excel_style_fill=>c_fill_gradient_diagonal45.
    lo_style_filled->fill->fgcolor-rgb  = zcl_excel_style_color=>c_blue.
    lo_style_filled->fill->bgcolor-rgb  = zcl_excel_style_color=>c_white.
    lo_style_filled->font->name         = zcl_excel_style_font=>c_name_cambria.
    lo_style_filled->font->scheme       = zcl_excel_style_font=>c_scheme_major.
    lv_style_gr_diagonal45_guid         = lo_style_filled->get_guid( ).


    lo_style_filled                     = lo_excel->add_new_style( ).
    lo_style_filled->fill->filltype     = zcl_excel_style_fill=>c_fill_gradient_diagonal45b.
    lo_style_filled->fill->fgcolor-rgb  = zcl_excel_style_color=>c_blue.
    lo_style_filled->fill->bgcolor-rgb  = zcl_excel_style_color=>c_white.
    lo_style_filled->font->name         = zcl_excel_style_font=>c_name_cambria.
    lo_style_filled->font->scheme       = zcl_excel_style_font=>c_scheme_major.
    lv_style_gr_diagonal45b_guid        = lo_style_filled->get_guid( ).

    lo_style_filled                     = lo_excel->add_new_style( ).
    lo_style_filled->fill->filltype     = zcl_excel_style_fill=>c_fill_gradient_diagonal135.
    lo_style_filled->fill->fgcolor-rgb  = zcl_excel_style_color=>c_blue.
    lo_style_filled->fill->bgcolor-rgb  = zcl_excel_style_color=>c_white.
    lo_style_filled->font->name         = zcl_excel_style_font=>c_name_cambria.
    lo_style_filled->font->scheme       = zcl_excel_style_font=>c_scheme_major.
    lv_style_gr_diagonal135_guid        = lo_style_filled->get_guid( ).

    lo_style_filled                     = lo_excel->add_new_style( ).
    lo_style_filled->fill->filltype     = zcl_excel_style_fill=>c_fill_gradient_diagonal135b.
    lo_style_filled->fill->fgcolor-rgb  = zcl_excel_style_color=>c_blue.
    lo_style_filled->fill->bgcolor-rgb  = zcl_excel_style_color=>c_white.
    lo_style_filled->font->name         = zcl_excel_style_font=>c_name_cambria.
    lo_style_filled->font->scheme       = zcl_excel_style_font=>c_scheme_major.
    lv_style_gr_diagonal135b_guid       = lo_style_filled->get_guid( ).



    " Create filled style turquoise using legacy excel ver <= 2003 palette. (https://code.sdn.sap.com/spaces/abap2xlsx/tickets/92)
    lo_style_filled                 = lo_excel->add_new_style( ).
    lo_excel->legacy_palette->set_color( "replace built-in color from palette with out custom RGB turquoise
        ip_index =     16
        ip_color =     '0040E0D0' ).

    lo_style_filled->fill->filltype = zcl_excel_style_fill=>c_fill_solid.
    lo_style_filled->fill->fgcolor-indexed  = 16.
    lv_style_filled_turquoise_guid            = lo_style_filled->get_guid( ).

    " Get active sheet
    lo_worksheet = lo_excel->get_active_worksheet( ).
    lo_worksheet->set_title( ip_title = 'Styles' ).
    lo_worksheet->set_cell( ip_column = 'B' ip_row = 2 ip_value = 'Hello world' ).
    lo_worksheet->set_cell( ip_column = 'C' ip_row = 3 ip_value = 'Bold text'            ip_style = lv_style_bold_guid ).
    lo_worksheet->set_cell( ip_column = 'D' ip_row = 4 ip_value = 'Underlined text'      ip_style = lv_style_underline_guid ).
    lo_worksheet->set_cell( ip_column = 'B' ip_row = 5 ip_value = 'Filled text'          ip_style = lv_style_filled_guid ).
    lo_worksheet->set_cell( ip_column = 'C' ip_row = 6 ip_value = 'Borders'              ip_style = lv_style_border_guid ).
    lo_worksheet->set_cell( ip_column = 'D' ip_row = 7 ip_value = 'I''m not a button :)' ip_style = lv_style_button_guid ).
    lo_worksheet->set_cell( ip_column = 'B' ip_row = 9 ip_value = 'Modified color for Excel 2003' ip_style = lv_style_filled_turquoise_guid ).
    " Fill the cell and apply one style
    lo_worksheet->set_cell( ip_column = 'B' ip_row = 6 ip_value = 'Filled text'          ip_style = lv_style_filled_guid ).
    " Change the style
    lo_worksheet->set_cell_style( ip_column = 'B' ip_row = 6 ip_style = lv_style_filled_green_guid ).
    " Add Style to an empty cell to test Fix for Issue
    "#44 Exception ZCX_EXCEL thrown when style is set for an empty cell
    " https://code.sdn.sap.com/spaces/abap2xlsx/tickets/44-exception-zcx_excel-thrown-when-style-is-set-for-an-empty-cell
    lo_worksheet->set_cell_style( ip_column = 'E' ip_row = 6 ip_style = lv_style_filled_green_guid ).


    lo_worksheet->set_cell( ip_column = 'B' ip_row = 10  ip_style = lv_style_gr_cornerlb_guid ip_value = zcl_excel_style_fill=>c_fill_gradient_cornerlb ).
    lo_row = lo_worksheet->get_row( ip_row = 10 ).
    lo_row->set_row_height( ip_row_height = 30 ).
    lo_worksheet->set_cell( ip_column = 'C' ip_row = 11  ip_style = lv_style_gr_cornerlt_guid ip_value = zcl_excel_style_fill=>c_fill_gradient_cornerlt ).
    lo_row = lo_worksheet->get_row( ip_row = 11 ).
    lo_row->set_row_height( ip_row_height = 30 ).
    lo_worksheet->set_cell( ip_column = 'B' ip_row = 12  ip_style = lv_style_gr_cornerrb_guid ip_value = zcl_excel_style_fill=>c_fill_gradient_cornerrb ).
    lo_row = lo_worksheet->get_row( ip_row = 12 ).
    lo_row->set_row_height( ip_row_height = 30 ).
    lo_worksheet->set_cell( ip_column = 'C' ip_row = 13  ip_style = lv_style_gr_cornerrt_guid ip_value = zcl_excel_style_fill=>c_fill_gradient_cornerrt ).
    lo_row = lo_worksheet->get_row( ip_row = 13 ).
    lo_row->set_row_height( ip_row_height = 30 ).
    lo_worksheet->set_cell( ip_column = 'B' ip_row = 14  ip_style = lv_style_gr_horizontal90_guid ip_value = zcl_excel_style_fill=>c_fill_gradient_horizontal90 ).
    lo_row = lo_worksheet->get_row( ip_row = 14 ).
    lo_row->set_row_height( ip_row_height = 30 ).
    lo_worksheet->set_cell( ip_column = 'C' ip_row = 15  ip_style = lv_style_gr_horizontal270_guid ip_value = zcl_excel_style_fill=>c_fill_gradient_horizontal270 ).
    lo_row = lo_worksheet->get_row( ip_row = 15 ).
    lo_row->set_row_height( ip_row_height = 30 ).
    lo_worksheet->set_cell( ip_column = 'B' ip_row = 16  ip_style = lv_style_gr_horizontalb_guid ip_value = zcl_excel_style_fill=>c_fill_gradient_horizontalb ).
    lo_row = lo_worksheet->get_row( ip_row = 16 ).
    lo_row->set_row_height( ip_row_height = 30 ).
    lo_worksheet->set_cell( ip_column = 'C' ip_row = 17  ip_style = lv_style_gr_vertical_guid ip_value = zcl_excel_style_fill=>c_fill_gradient_vertical ).
    lo_row = lo_worksheet->get_row( ip_row = 17 ).
    lo_row->set_row_height( ip_row_height = 30 ).
    lo_worksheet->set_cell( ip_column = 'B' ip_row = 18  ip_style = lv_style_gr_vertical2_guid ip_value = zcl_excel_style_fill=>c_fill_gradient_vertical ).
    lo_row = lo_worksheet->get_row( ip_row = 18 ).
    lo_row->set_row_height( ip_row_height = 30 ).
    lo_worksheet->set_cell( ip_column = 'C' ip_row = 19  ip_style = lv_style_gr_fromcenter_guid ip_value = zcl_excel_style_fill=>c_fill_gradient_fromcenter ).
    lo_row = lo_worksheet->get_row( ip_row = 19 ).
    lo_row->set_row_height( ip_row_height = 30 ).
    lo_worksheet->set_cell( ip_column = 'B' ip_row = 20  ip_style = lv_style_gr_diagonal45_guid ip_value = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
    lo_row = lo_worksheet->get_row( ip_row = 20 ).
    lo_row->set_row_height( ip_row_height = 30 ).
    lo_worksheet->set_cell( ip_column = 'C' ip_row = 21  ip_style = lv_style_gr_diagonal45b_guid ip_value = zcl_excel_style_fill=>c_fill_gradient_diagonal45b ).
    lo_row = lo_worksheet->get_row( ip_row = 21 ).
    lo_row->set_row_height( ip_row_height = 30 ).
    lo_worksheet->set_cell( ip_column = 'B' ip_row = 22  ip_style = lv_style_gr_diagonal135_guid ip_value = zcl_excel_style_fill=>c_fill_gradient_diagonal135 ).
    lo_row = lo_worksheet->get_row( ip_row = 22 ).
    lo_row->set_row_height( ip_row_height = 30 ).
    lo_worksheet->set_cell( ip_column = 'C' ip_row = 23  ip_style = lv_style_gr_diagonal135b_guid ip_value = zcl_excel_style_fill=>c_fill_gradient_diagonal135b ).
    lo_row = lo_worksheet->get_row( ip_row = 23 ).
    lo_row->set_row_height( ip_row_height = 30 ).

    IF iv_huge = abap_false.
      CREATE OBJECT lo_writer TYPE zcl_excel_writer_2007.
    ELSE.
      CREATE OBJECT lo_writer TYPE zcl_excel_writer_huge_file.
    ENDIF.
    rv_xlsx = lo_writer->write_file( lo_excel ).

  ENDMETHOD.
ENDCLASS.

CLASS ltc_demo1 DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS test FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltc_demo1 IMPLEMENTATION.
  METHOD test.
    DATA: lv_writer_xlsx      TYPE xstring,
          lv_writer_huge_xlsx TYPE xstring.
    DATA(demo) = CAST lif_demo( NEW lcl_demo1( ) ).
    lv_writer_xlsx = demo->generate( iv_huge = abap_false ).
    lv_writer_huge_xlsx = demo->generate( iv_huge = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = lv_writer_xlsx exp = lv_writer_huge_xlsx ).
  ENDMETHOD.
ENDCLASS.

TABLES sscrfields.
PARAMETERS p_files RADIOBUTTON GROUP rb1 DEFAULT 'X'.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE title_b1.
PARAMETERS p_file1 TYPE string LOWER CASE.
PARAMETERS p_file2 TYPE string LOWER CASE.
SELECTION-SCREEN END OF BLOCK b1.
PARAMETERS p_demos RADIOBUTTON GROUP rb1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE title_b2.
PARAMETERS p_demo1 RADIOBUTTON GROUP rb2.
PARAMETERS p_demo2 RADIOBUTTON GROUP rb2.
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.
  DATA(app) = NEW lcl_app( ).
  app->set_sscrfields( REF #( sscrfields ) ).
  title_b1 = 'Compare 2 files'(b01).
  title_b2 = 'Compare Writer_2007 versus Writer_Huge_File'(b02).

AT SELECTION-SCREEN OUTPUT.
  app->at_selection_screen_output( ).

AT SELECTION-SCREEN.
  app->at_selection_screen( ).
