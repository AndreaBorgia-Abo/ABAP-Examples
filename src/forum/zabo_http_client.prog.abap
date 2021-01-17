*&---------------------------------------------------------------------*
*& Report zabo_http_client
*&---------------------------------------------------------------------*
*& Author: Andrea Borgia, based on an example by Sergio Serra
*& Source: https://answers.sap.com/answers/12637215/view.html
*&---------------------------------------------------------------------*
* Summary:
* I've changed the example to call a generic test service[1] so that it
* can be used anywhere to try the ICF client recorder [2].
* I have also added a clean de/serialization with a cloud-safe[3] class[4]
* [1] https://jsonplaceholder.typicode.com/guide/
* [2] https://blogs.sap.com/2006/04/05/icf-recording-a-possibility-for-analysing/
* [3] https://answers.sap.com/answers/12948279/view.html
* [4[ https://wiki.scn.sap.com/wiki/display/Snippets/One+more+ABAP+to+JSON+Serializer+and+Deserializer
*&---------------------------------------------------------------------*
* Misc notes:
* curl --trace-ascii trace.txt --data @posts.json -H "Content-Type: application/json"
*   http://jsonplaceholder.typicode.com/posts
* posts.json:
*{
*    "title": "foo",
*    "body": "bar",
*    "userId": 1
*}
*&---------------------------------------------------------------------*
REPORT zabo_http_client.

TYPES: BEGIN OF post_entity,
         title  TYPE string,
         body   TYPE string,
         userid TYPE int2,
         id     TYPE int2,
       END OF post_entity.

DATA: lo_response    TYPE REF TO     if_rest_entity.

DATA: http_client TYPE REF TO if_http_client,
      rest_client TYPE REF TO cl_rest_http_client,
      url         TYPE string.

url = `http://jsonplaceholder.typicode.com/posts`.

cl_http_client=>create_by_url(
  EXPORTING
    url                = url
*    proxy_host         = 'xxxxxxxxx'
*    proxy_service      = 'xxxx'
  IMPORTING
    client             = http_client
  EXCEPTIONS
    argument_not_found = 1
    plugin_not_active  = 2
    internal_error     = 3
    OTHERS             = 4
).
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

cl_http_utility=>set_request_uri(
  EXPORTING
    request = http_client->request
    uri     = url
).

http_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).

CREATE OBJECT rest_client
  EXPORTING
    io_http_client = http_client.

DATA: rest_entity TYPE REF TO if_rest_entity.

TRY.
    rest_entity ?= rest_client->if_rest_client~create_request_entity( ).

    rest_entity->set_header_field(
      EXPORTING
        iv_name  = if_http_header_fields=>content_type
        iv_value = if_rest_media_type=>gc_appl_json
    ).

* MEMO: quick and dirty way...
*    DATA(jsonbody) = `{"title": "foo", "body": "bar", "userId": 1}`.

* MEMO: ... or better way
    DATA(abapbody) = VALUE post_entity(
      title = `foo`
      body = `bar`
      userid = 1
    ).
    DATA(jsonbody) = /ui2/cl_json=>serialize(
      data = abapbody
      pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).
    WRITE: / |JSON body: { jsonbody }|.

    rest_entity->set_string_data( jsonbody ).

    rest_client->if_rest_client~post( io_entity = rest_entity ).

    lo_response = rest_client->if_rest_client~get_response_entity( ).

    DATA(http_status) = lo_response->get_header_field( '~status_code' ).
    DATA(response_string) = lo_response->get_string_data( ).

    /ui2/cl_json=>deserialize(
      EXPORTING
        json = response_string
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data = abapbody ).

    WRITE:/ |Status code: { http_status } ; new ID = { abapbody-id } ; response: { response_string } |.
  CATCH cx_rest_client_exception INTO DATA(rest_client_exception).

    DATA(error) = rest_client_exception->get_longtext( ).
    WRITE:/ error.

ENDTRY.
