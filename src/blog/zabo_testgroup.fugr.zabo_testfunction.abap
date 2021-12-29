FUNCTION zabo_testfunction.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_INPUT) TYPE  CHAR1
*"  EXPORTING
*"     VALUE(O_OUTPUT) TYPE  CHAR1
*"  EXCEPTIONS
*"      ZEXCEPTION
*"----------------------------------------------------------------------
* Author: Andrea Borgia
* Note: not much to see here, I only need it for SOAP & REST practice
*"----------------------------------------------------------------------

  IF i_input = abap_true.
    o_output = abap_false.
  ELSE.
    o_output = abap_true.
* See https://answers.sap.com/questions/1921537/abap-web-services-exception-valorization-in-soap-f.html
* Apparently, the text of the message / exception is ignored: E->500, I->200 and that's all.
*    MESSAGE e001(zabo). "RAISING zexception.
  ENDIF.

ENDFUNCTION.
