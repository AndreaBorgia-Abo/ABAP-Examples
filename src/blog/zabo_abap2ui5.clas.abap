CLASS zabo_abap2ui5 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_http_extension.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZABO_ABAP2UI5 IMPLEMENTATION.


METHOD if_http_extension~handle_request.

  DATA lt_header TYPE tihttpnvp.
  server->request->get_header_fields( CHANGING fields = lt_header ).

  DATA lt_param TYPE tihttpnvp.
  server->request->get_form_fields( CHANGING fields = lt_param ).

  z2ui5_cl_http_handler=>client = VALUE #(
     t_header = lt_header
     t_param  = lt_param
     body     = server->request->get_cdata( ) ).

  DATA(lv_resp) = SWITCH #( server->request->get_method( )
     WHEN 'GET'  THEN z2ui5_cl_http_handler=>http_get( )
     WHEN 'POST' THEN z2ui5_cl_http_handler=>http_post( ) ).

  server->response->set_cdata( lv_resp ).
  server->response->set_status( code = 200 reason = 'success' ).

ENDMETHOD.
ENDCLASS.
