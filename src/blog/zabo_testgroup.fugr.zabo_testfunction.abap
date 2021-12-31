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
*"----------------------------------------------------------------------

  IF i_input = abap_true.
    o_output = abap_false.
  ELSE.
    o_output = abap_true.
* See https://answers.sap.com/answers/13554580/view.html
* For extra details in the output, use RZ11 to set is/HTTP/show_detailed_errors = TRUE
* Logs are in SRT_UTIL, "Error log" button.
    MESSAGE e001(zabo) WITH 'Message in a bottle'.
  ENDIF.

ENDFUNCTION.
