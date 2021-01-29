class ZVIESCO_CHECK_VAT_PORT_TYPE definition
  public
  inheriting from CL_PROXY_CLIENT
  create public .

public section.

  methods CHECK_VAT
    importing
      !CHECK_VAT_REQUEST type ZVIESCHECK_VAT_REQUEST
    exporting
      !CHECK_VAT_RESPONSE type ZVIESCHECK_VAT_RESPONSE
    raising
      CX_AI_SYSTEM_FAULT .
  methods CHECK_VAT_APPROX
    importing
      !CHECK_VAT_APPROX_REQUEST type ZVIESCHECK_VAT_APPROX_REQUEST
    exporting
      !CHECK_VAT_APPROX_RESPONSE type ZVIESCHECK_VAT_APPROX_RESPONSE
    raising
      CX_AI_SYSTEM_FAULT .
  methods CONSTRUCTOR
    importing
      !LOGICAL_PORT_NAME type PRX_LOGICAL_PORT_NAME optional
    raising
      CX_AI_SYSTEM_FAULT .
protected section.
private section.
ENDCLASS.



CLASS ZVIESCO_CHECK_VAT_PORT_TYPE IMPLEMENTATION.


  method CHECK_VAT.

  data:
    ls_parmbind type abap_parmbind,
    lt_parmbind type abap_parmbind_tab.

  ls_parmbind-name = 'CHECK_VAT_REQUEST'.
  ls_parmbind-kind = cl_abap_objectdescr=>importing.
  get reference of CHECK_VAT_REQUEST into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  ls_parmbind-name = 'CHECK_VAT_RESPONSE'.
  ls_parmbind-kind = cl_abap_objectdescr=>exporting.
  get reference of CHECK_VAT_RESPONSE into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  if_proxy_client~execute(
    exporting
      method_name = 'CHECK_VAT'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.


  method CHECK_VAT_APPROX.

  data:
    ls_parmbind type abap_parmbind,
    lt_parmbind type abap_parmbind_tab.

  ls_parmbind-name = 'CHECK_VAT_APPROX_REQUEST'.
  ls_parmbind-kind = cl_abap_objectdescr=>importing.
  get reference of CHECK_VAT_APPROX_REQUEST into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  ls_parmbind-name = 'CHECK_VAT_APPROX_RESPONSE'.
  ls_parmbind-kind = cl_abap_objectdescr=>exporting.
  get reference of CHECK_VAT_APPROX_RESPONSE into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  if_proxy_client~execute(
    exporting
      method_name = 'CHECK_VAT_APPROX'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.


  method CONSTRUCTOR.

  super->constructor(
    class_name          = 'ZVIESCO_CHECK_VAT_PORT_TYPE'
    logical_port_name   = logical_port_name
  ).

  endmethod.
ENDCLASS.
