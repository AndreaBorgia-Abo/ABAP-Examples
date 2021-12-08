*&---------------------------------------------------------------------*
*& Report ZABO_ABAPGIT_BUG_4647
*&---------------------------------------------------------------------*
*& Author: Marc Bernard
*& Source: https://github.com/abapGit/abapGit/issues/4647#issuecomment-800934086
*&---------------------------------------------------------------------*
REPORT zabo_abapgit_bug_4647.

CONSTANTS:
  c_query TYPE string VALUE 'key=00%2601&param=abc%3defg&value=xxx%3fyyy&option=%25%24123%5f'.

DATA:
  gv_url    TYPE string,
  gs_fields TYPE ihttpnvp,
  gt_fields TYPE tihttpnvp.

* ZCL_ABAPGIT_HTML_ACTION_UTILS
gt_fields = zcl_abapgit_html_action_utils=>parse_fields( c_query ).
LOOP AT gt_fields INTO gs_fields.
  WRITE: / gs_fields-name, gs_fields-value.
ENDLOOP.
SKIP.

gt_fields = zcl_abapgit_html_action_utils=>parse_fields( to_upper( c_query ) ).
LOOP AT gt_fields INTO gs_fields.
  WRITE: / gs_fields-name, gs_fields-value.
ENDLOOP.
SKIP.

* CL_HTTP_UTILITY
gt_fields = cl_http_utility=>string_to_fields( c_query ).
LOOP AT gt_fields INTO gs_fields.
  WRITE: / gs_fields-name, gs_fields-value.
ENDLOOP.
SKIP.

gt_fields = cl_http_utility=>string_to_fields( to_upper( c_query ) ).
LOOP AT gt_fields INTO gs_fields.
  WRITE: / gs_fields-name, gs_fields-value.
ENDLOOP.
SKIP.
