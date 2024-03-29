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

   DATA(lv_resp) = SWITCH #( server->request->get_method( )
      WHEN 'GET'  THEN z2ui5_cl_http_handler=>http_get( )
      WHEN 'POST' THEN z2ui5_cl_http_handler=>http_post( server->request->get_cdata( ) ) ).

   server->response->set_header_field( name = `cache-control` value = `no-cache` ).
   server->response->set_cdata( lv_resp ).
   server->response->set_status( code = 200 reason = `success` ).

ENDMETHOD.
ENDCLASS.
