FUNCTION zabo_testfunction.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_INPUT) TYPE  CHAR1
*"  EXPORTING
*"     VALUE(O_OUTPUT) TYPE  CHAR1
*"----------------------------------------------------------------------
* Author: Andrea Borgia
* Note: not much to see here, I only need it for SOAP & REST practice
* SOAP: ZABO_TESTSOAP (enterprise service definition)
* REST: ZABO_CL_REST_TEST_GET (class)
*"----------------------------------------------------------------------

  IF i_input = abap_true.
    o_output = abap_false.
  ELSE.
    o_output = abap_true.
  ENDIF.

* For extra details in the output, use RZ11 to set is/HTTP/show_detailed_errors = TRUE
* Logs are in SRT_UTIL, "Error log" button.
* ( source: https://answers.sap.com/answers/13554580/view.html )
*    MESSAGE e001(zabo) WITH 'Message in a bottle'.

* Unfinished work, on SOAP exception handling:
* see https://answers.sap.com/comments/13556538/view.html
* see https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abapraise_exception_class.htm
ENDFUNCTION.
