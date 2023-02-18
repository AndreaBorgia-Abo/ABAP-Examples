*&---------------------------------------------------------------------*
*& Report ZABO_ATTACH_PICTURE
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://answers.sap.com/answers/13821582/view.html
*&---------------------------------------------------------------------*
REPORT ZABO_ATTACH_PICTURE.

PARAMETERS dummy. " dummy code to display a screen

AT SELECTION-SCREEN OUTPUT.
  DATA l_string TYPE string.
  CONCATENATE
        '89504E470D0A1A0A0000000D49484452000000120000001108020000005F386BAE000000'
        '097048597300000EC400000EC401952B0E1B000000864944415478DA63BC79FD1803E980'
        '89812CC002A1D435AD88D773F3FA311638A7EFEC0762F414190B206C8309F1C3D9FFFEBD'
        '877A834910494927F17EEB243B4856630F122CE6310962B5079F3698C7D21818189898F6'
        '10EB48D460B847B5E81E0CDAE071CDC0C0F0EF5F1A03C33DB43860C11AB34C584CBB871C'
        '1F2C68898D48C0485E7E03007A9E1FADA3FE1BE00000000049454E44AE426082'
        INTO l_string.
  DATA(xstring) = CONV xstring( l_string ).
  TYPES: tp_binary TYPE x LENGTH 256 .
  DATA: it_binary_tab TYPE TABLE OF tp_binary .
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer     = xstring
    TABLES
      binary_tab = it_binary_tab.
  DATA:     url TYPE cndp_url .
  "binary -> url
  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type                 = 'image'
      subtype              = 'png'
    TABLES
      data                 = it_binary_tab
    CHANGING
      url                  = url
    EXCEPTIONS
      dp_invalid_parameter = 1
      dp_error_put_table   = 2
      dp_error_general     = 3
      OTHERS               = 4.
  DATA : ob_gui_container TYPE REF TO cl_gui_docking_container.
  DATA(ob_gui_picture_1) = NEW cl_gui_picture( parent = ob_gui_container ).
  ob_gui_picture_1->load_picture_from_url( url = url ).
  ob_gui_picture_1->set_position( width = 20 height = 10 left = 10 top = 10 ).
