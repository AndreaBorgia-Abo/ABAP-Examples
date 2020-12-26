*&---------------------------------------------------------------------*
*& Report ZABO_OOP_SUBMIT_CALLING
*&---------------------------------------------------------------------*
*& Author: Raymond Giuseppi (mhtml conversion)
*& Source: https://answers.sap.com/answers/375341/view.html
*&---------------------------------------------------------------------*
*& Related useful material:
*& MB52 output capture: https://answers.sap.com/answers/13186247/view.html
*&---------------------------------------------------------------------*
REPORT zabo_oop_submit_calling.


DATA: gr_submit_output   TYPE REF TO data,
      gs_output_metadata TYPE cl_salv_bs_runtime_info=>s_type_metadata,
      gr_submit_data     TYPE REF TO cl_salv_ex_result_data_table,
      e_xml              TYPE xstring,
* MEMO: the type can be anything, even standard types,
* as long as it matches that of the called program!
      gt_square          TYPE STANDARD TABLE OF zabo_submit_demo,
      wa_square          TYPE zabo_submit_demo,
      g_subline          TYPE REF TO data
      .

FIELD-SYMBOLS: <gfs_table> TYPE table,
               <gfs_tline> TYPE any.


PARAMETERS:
  p_report LIKE trdir-name DEFAULT `ZABO_OOP_SUBMIT_CALLED`,
  p_var    LIKE varid-variant DEFAULT `V3`.


START-OF-SELECTION.

  zcl_aab=>break_point( 'ZABO_DEMO' ).

* Set Handler
  CALL METHOD cl_salv_bs_runtime_info=>set
    EXPORTING
      display  = abap_false
      metadata = abap_true
      data     = abap_true.

* Call report
  SUBMIT (p_report)
   USING SELECTION-SET p_var
   AND RETURN.

* get data back
  TRY.
      cl_salv_bs_runtime_info=>get_data_ref(
        IMPORTING r_data      = gr_submit_output ).
      ASSIGN gr_submit_output->* TO <gfs_table>.
    CATCH cx_salv_bs_sc_runtime_info.
      MESSAGE: TEXT-m01 TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
  ENDTRY.

* get medtadata back
  gs_output_metadata = cl_salv_bs_runtime_info=>get_metadata( ).

* simulate the ALV
  gr_submit_data = cl_salv_ex_util=>factory_result_data_table(
    r_data                 = gr_submit_output
    s_layout               = gs_output_metadata-s_layout
    t_fieldcatalog         = gs_output_metadata-t_fcat
    t_sort                 = gs_output_metadata-t_sort
    t_filter               = gs_output_metadata-t_filter
  ).

* convert to mhtml
  cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform(
    EXPORTING
      xml_version   = if_salv_bs_xml=>version
      r_result_data = gr_submit_data
      xml_type      = if_salv_bs_xml=>c_type_mhtml
      xml_flavour   = if_salv_bs_c_tt=>c_tt_xml_flavour_export
      gui_type      = if_salv_bs_xml=>c_gui_type_gui
    IMPORTING
      xml           = e_xml ).

* be clean
  cl_salv_bs_runtime_info=>clear_all( ).


* MEMO: here you write e_xml to a file, for example
  zcl_aab=>break_point( 'ZABO_DEMO' ).


* MEMO: here you process the data recovered from the SUBMIT
  IF <gfs_table> IS ASSIGNED.

* We need a data object with the correct type...
    CREATE DATA g_subline LIKE LINE OF <gfs_table>.
    ASSIGN g_subline->* TO <gfs_tline>.

* ... or this will dump in Unicode-enabled systems
    LOOP AT <gfs_table> INTO <gfs_tline>.
      MOVE-CORRESPONDING <gfs_tline> TO wa_square.
      APPEND wa_square TO gt_square.
    ENDLOOP.

* MEMO: the ALV display is just to prove it works, it's not needed!
    cl_salv_table=>factory(
            IMPORTING
              r_salv_table = DATA(g_salv)
            CHANGING
              t_table      = gt_square ).

    g_salv->display( ).
  ENDIF.
