*&---------------------------------------------------------------------*
*& Report ZABO_MULTI_ALV_TO_EXCEL_2_MAIL
*&---------------------------------------------------------------------*
*& Author: James E. McDonough
*& source: https://blogs.sap.com/2018/06/16/using-abap2xlsx-to-send-alv-table-output-as-excel-spreadsheet-via-internet-email/
*&---------------------------------------------------------------------*
REPORT ZABO_MULTI_ALV_TO_EXCEL_2_MAIL.
  types          : row_counter    type n length 02.
  types          : email_recipient
                                  type adr6-smtp_addr.
  data           : excel          type ref to zcl_excel ##NEEDED.
  parameters     : rowcount       type row_counter.
  parameters     : recipien       type email_recipient.
initialization.
  select single smtp_addr
    into recipien
    from adr6 ##WARN_OK
           inner join
         usr21 on usr21~persnumber eq adr6~persnumber
   where usr21~bname              eq sy-uname.
start-of-selection.
  perform display_flight_rows using rowcount.
  perform display_carrier_rows using rowcount.
  perform display_booking_rows using rowcount.
  perform send_excel_via_email using recipien.
form display_flight_rows using row_count
                                  type row_counter
                       raising zcx_excel.
    data         : flight_stack   type standard table of sflight
                 , alv_report     type ref to cl_salv_table
                 .
    try.
      call method cl_salv_table=>factory
        importing
          r_salv_table            = alv_report
        changing
          t_table                 = flight_stack.
    catch cx_salv_msg.
      return.
    endtry.
    select *
      into table flight_stack
      from sflight
             up to row_count rows.
    alv_report->display( ).
    perform copy_table_to_excel_worksheet using flight_stack 'Flights'.
endform.
form display_carrier_rows using row_count
                                  type row_counter
                        raising zcx_excel.
    data         : carrier_stack  type standard table of scarr
                 , alv_report     type ref to cl_salv_table
                 .
    try.
      call method cl_salv_table=>factory
        importing
          r_salv_table            = alv_report
        changing
          t_table                 = carrier_stack.
    catch cx_salv_msg.
      return.
    endtry.
    select *
      into table carrier_stack
      from scarr
             up to row_count rows.
    alv_report->display( ).
    perform copy_table_to_excel_worksheet using carrier_stack 'Carriers'.
endform.
form display_booking_rows using row_count
                                  type row_counter
                        raising zcx_excel.
    data         : booking_stack  type standard table of sbook
                 , alv_report     type ref to cl_salv_table
                 .
    try.
      call method cl_salv_table=>factory
        importing
          r_salv_table            = alv_report
        changing
          t_table                 = booking_stack.
    catch cx_salv_msg.
      return.
    endtry.
    select *
      into table booking_stack
      from sbook
             up to row_count rows.
    alv_report->display( ).
    perform copy_table_to_excel_worksheet using booking_stack 'Bookings'.
endform.
form copy_table_to_excel_worksheet using source_stack
                                           type standard table
                                         source_description
                                           type string
                                 raising zcx_excel.
    constants    : first_column   type char1     value 'A'
                 .
    data         : worksheet      type ref to zcl_excel_worksheet
                 , worksheet_title
                                  type zexcel_sheet_title
                 , table_settings type zexcel_s_table_settings
                 .
    table_settings-table_style    = zcl_excel_table=>builtinstyle_medium2.
    table_settings-show_row_stripes
                                  = abap_true.
    table_settings-nofilters      = abap_true.
    table_settings-top_left_column
                                  = first_column.
    table_settings-top_left_row   = 01.
    if excel is not bound.
      create object excel.
      worksheet                   = excel->get_active_worksheet( ).
    else.
      worksheet                   = excel->add_new_worksheet( ).
    endif.
    worksheet_title               = source_description.
    worksheet->set_title( worksheet_title ).
    worksheet->bind_table(
      ip_table                    = source_stack
      is_table_settings           = table_settings
      ).
endform.
form send_excel_via_email using recipient type email_recipient.
    constants    : excel_file_type
                                 type string value '.xlsx'
                 , file_name_parameter
                                  type string value '&SO_FILENAME='
                 .
    data         : excel_writer   type ref to zif_excel_writer
                 , excel_as_xstring
                                  type xstring
                 , excel_as_xstring_bytecount
                                  type i
                 , excel_as_solix_stack
                                  type solix_tab
                 , mail_send_request
                                  type ref to cl_bcs
                 , mail_message   type ref to cl_document_bcs
                 , any_bcs_exception
                                  type ref to cx_bcs
                 , diagnostic     type string
                 , mail_title     type so_obj_des
                 , mail_text_stack
                                  type soli_tab
                 , mail_text_entry
                                  like line
                                    of mail_text_stack
                 , mail_attachment_subject
                                  type sood-objdes
                 , mail_attachment_bytecount
                                  type sood-objlen
                 , mail_attachment_header_stack
                                  type soli_tab
                 , mail_attachment_header_entry
                                  like line of mail_attachment_header_stack
                 , internet_email_recipient
                                  type ref to if_recipient_bcs
                 , successful_send
                                  type abap_bool
                 , file_name      type string
                 .
    " Much of the code here was lifted from method send_mail of
    " class lcl_ouput, defined in object ZDEMO_EXCEL_OUTPUTOPT_INCL:
    concatenate sy-repid          " this report name
                sy-datum          " current date
                sy-uzeit          " current time
                excel_file_type   " excel file extension
           into file_name.
    mail_title                    = file_name.
    mail_attachment_subject       = file_name.
    mail_text_entry               = 'See attachment'.
    append mail_text_entry
        to mail_text_stack.
    concatenate file_name_parameter
                file_name
           into mail_attachment_header_entry.
    append mail_attachment_header_entry
        to mail_attachment_header_stack.
    create object excel_writer type zcl_excel_writer_2007.
    excel_as_xstring              = excel_writer->write_file( excel ).
    excel_as_solix_stack          = cl_bcs_convert=>xstring_to_solix( iv_xstring = excel_as_xstring ).
    excel_as_xstring_bytecount    = xstrlen( excel_as_xstring ).
    mail_attachment_bytecount     = excel_as_xstring_bytecount.
    try.
      mail_message                = cl_document_bcs=>create_document(
                                      i_type    = 'RAW' "#EC NOTEXT
                                      i_text    = mail_text_stack
                                      i_subject = mail_title
                                      ).
      mail_message->add_attachment(
        i_attachment_type         = 'XLS' "#EC NOTEXT
        i_attachment_subject      = mail_attachment_subject
        i_attachment_size         = mail_attachment_bytecount
        i_att_content_hex         = excel_as_solix_stack
        i_attachment_header       = mail_attachment_header_stack
        ).
      mail_send_request           = cl_bcs=>create_persistent( ).
      mail_send_request->set_document( mail_message ).
      internet_email_recipient    = cl_cam_address_bcs=>create_internet_address( recipient ).
      mail_send_request->add_recipient( internet_email_recipient ).
      successful_send             = mail_send_request->send( ).
      commit work.
      if successful_send eq abap_false.
        message i500(sbcoms) with recipient.
      else.
        message s022(so).
        message 'Document ready to be sent - Check SOST' type 'I'.
      endif.
    catch cx_bcs into any_bcs_exception.
      diagnostic                  = any_bcs_exception->if_message~get_text( ).
      message diagnostic type 'I'.
    endtry.
endform.
