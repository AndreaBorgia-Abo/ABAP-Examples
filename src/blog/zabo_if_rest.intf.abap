interface ZABO_IF_REST
  public .


  data RESPONSE type ref to IF_HTTP_RESPONSE .
  data REQUEST type ref to IF_HTTP_REQUEST .

  methods HANDLE_REQUEST .
  methods SET_RESPONSE
    importing
      !IS_DATA type XSTRING .
endinterface.
