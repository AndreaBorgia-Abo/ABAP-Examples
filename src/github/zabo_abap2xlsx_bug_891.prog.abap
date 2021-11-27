*&---------------------------------------------------------------------*
*& Report ZABO_ABAP2XLSX_BUG_891
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://github.com/sapmentors/abap2xlsx/issues/891#issuecomment-974650556
*&---------------------------------------------------------------------*
REPORT ZABO_ABAP2XLSX_BUG_891.

CLASS ltc_get_zip_files_at_path DEFINITION DEFERRED.
CLASS ltc_compare_zip_files DEFINITION DEFERRED.

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

    METHODS display_excel
      IMPORTING
        container TYPE REF TO cl_gui_container.
    METHODS get_excel
      RETURNING
        VALUE(result) TYPE xstring.
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
    METHODS get_last_and_previous_zip
      EXPORTING
        eo_zip_old TYPE REF TO cl_abap_zip
        eo_zip     TYPE REF TO cl_abap_zip.
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
    METHODS on_selection_changed
                  FOR EVENT selection_changed OF cl_tree_control_base
      IMPORTING node_key.

    DATA: sscrfields            TYPE REF TO sscrfields,
          go_splitter_container TYPE REF TO cl_gui_splitter_container,
          go_container_left     TYPE REF TO cl_gui_container,
          go_container_right    TYPE REF TO cl_gui_container,
          error                 TYPE REF TO i_oi_error,
          t_errors              TYPE STANDARD TABLE OF REF TO i_oi_error WITH NON-UNIQUE DEFAULT KEY,
          cl_control            TYPE REF TO i_oi_container_control,
          cl_document           TYPE REF TO i_oi_document_proxy,
          xdata                 TYPE xstring,
          t_rawdata             TYPE solix_tab,
          bytecount             TYPE i,
          go_tree               TYPE REF TO cl_gui_simple_tree,
          gt_tree               TYPE ty_tree_nodes,
          diff_files            TYPE lcl_app=>ty_diff_files,
          retcode               TYPE soi_ret_string,
          tree_file_links       TYPE ty_tree_file_links,
          zip_old               TYPE REF TO cl_abap_zip,
          zip_new               TYPE REF TO cl_abap_zip,
          temp_dir              TYPE string.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD at_selection_screen_output.

    IF go_splitter_container IS NOT BOUND.

      CREATE OBJECT go_splitter_container
        EXPORTING
          parent  = cl_gui_container=>screen0
          rows    = 1
          columns = 2.
      go_container_left = go_splitter_container->get_container( row = 1 column = 1 ).
      go_container_right = go_splitter_container->get_container( row = 1 column = 2 ).
      sscrfields->functxt_01 = '@46@Compare'.
      display_excel( go_container_left ).
      xdata = get_excel( ).

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


  METHOD display_excel.

    c_oi_container_control_creator=>get_container_control( IMPORTING control = cl_control
                                                                     error   = error ).
    APPEND error TO t_errors.

    cl_control->init_control( EXPORTING  inplace_enabled     = 'X'
                                         no_flush            = 'X'
                                         r3_application_name = 'Demo Document Container'
                                         parent              = container
                              IMPORTING  error               = error
                              EXCEPTIONS OTHERS              = 2 ).
    APPEND error TO t_errors.

    cl_control->get_document_proxy( EXPORTING document_type  = 'Excel.Sheet'                " EXCEL
                                              no_flush       = ' '
                                    IMPORTING document_proxy = cl_document
                                              error          = error ).
    APPEND error TO t_errors.

    cl_document->create_document(
      EXPORTING
        open_inplace = 'X'
      IMPORTING
        error        = error
        retcode      = retcode ).
    APPEND error TO t_errors.

  ENDMETHOD.

  METHOD at_selection_screen.

    CASE sscrfields->ucomm.

      WHEN 'FC01'. " Compare

        get_last_and_previous_zip(
            IMPORTING
              eo_zip_old = zip_old
              eo_zip     = zip_new ).
        diff_files = compare_zip_files( zip_1 = zip_old zip_2 = zip_new ).
        display_tree( go_container_right ).

    ENDCASE.

  ENDMETHOD.

  METHOD get_last_and_previous_zip.

    DATA(old_xdata) = xdata.
    xdata = get_excel( ).
    DATA: lo_zip_old TYPE REF TO cl_abap_zip,
          lo_zip     TYPE REF TO cl_abap_zip.
    CREATE OBJECT eo_zip_old.
    CALL METHOD eo_zip_old->load
      EXPORTING
        zip             = old_xdata
      EXCEPTIONS
        zip_parse_error = 1
        OTHERS          = 2.
    CREATE OBJECT eo_zip.
    CALL METHOD eo_zip->load
      EXPORTING
        zip             = xdata
      EXCEPTIONS
        zip_parse_error = 1
        OTHERS          = 2.


  ENDMETHOD.



  METHOD get_excel.

    cl_document->save_document_to_table(
      IMPORTING
        error          = error
        retcode        = retcode
      CHANGING
        document_size  = bytecount
        document_table = t_rawdata ).

    result = cl_bcs_convert=>solix_to_xstring(
        it_solix   = t_rawdata
        iv_size    = bytecount ).

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
  ENDMETHOD.


  METHOD add_tree_nodes.

    FIELD-SYMBOLS:
        <sub_files> TYPE ty_diff_files.

    LOOP AT diff_files REFERENCE INTO DATA(diff_file).

      APPEND INITIAL LINE TO tree_nodes ASSIGNING FIELD-SYMBOL(<gs_tree>).
      <gs_tree>-node_key = |{ sy-tabix }|.
      <gs_tree>-relatkey = parent_node_key.
      <gs_tree>-relatship = cl_tree_control_base=>relat_first_child.
      <gs_tree>-text = diff_file->name.
      <gs_tree>-style = SWITCH #( diff_file->diff
          WHEN 'only in 1' THEN cl_tree_control_base=>style_emphasized_negative
          WHEN 'only in 2' THEN cl_tree_control_base=>style_emphasized_positive
          WHEN 'diff' THEN cl_tree_control_base=>style_emphasized
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

      data(solix_tab) = cl_bcs_convert=>xstring_to_solix( i_content ).

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

TABLES sscrfields.
PARAMETERS dummy.
SELECTION-SCREEN FUNCTION KEY 1.

INITIALIZATION.
  DATA(app) = NEW lcl_app( ).
  app->set_sscrfields( REF #( sscrfields ) ).

AT SELECTION-SCREEN OUTPUT.
  app->at_selection_screen_output( ).

AT SELECTION-SCREEN.
  app->at_selection_screen( ).
