*&---------------------------------------------------------------------*
*& Report ZABO_INVISIBLE_TRICK
*&---------------------------------------------------------------------*
*& Author: Peter Inotai
*& Source: https://blogs.sap.com/2021/06/11/bring-sapgui-mode-to-the-front/#comment-575729
*&---------------------------------------------------------------------*
REPORT zabo_invisible_trick.


*************************************************************
START-OF-SELECTION.
*SAPGUI screen vanishes from user screen list

  CALL FUNCTION 'SAPGUI_SET_PROPERTY'
    DESTINATION 'SAPGUI'
    EXPORTING
      property              = 'VISIBLE'
      value                 = ' '
    EXCEPTIONS
      system_failure        = 1
      communication_failure = 2
      OTHERS                = 3.

************************************************************************


  DO 10000 TIMES.

*doing some work which takes lot of time
    DO 1000 TIMES.
    ENDDO.
*Resetting time counter of dialog process so that time-out does not
*happen. Use this fm within your programs at appropriate locations to
*reset time counter.

    CALL FUNCTION 'TH_REDISPATCH'.

  ENDDO.

*************************************************************
*Work is complete and now wake up the SAPGUI screen

  CALL FUNCTION 'SAPGUI_SET_PROPERTY'
    DESTINATION 'SAPGUI'
    EXPORTING
      property              = 'VISIBLE'
      value                 = 'X'
    EXCEPTIONS
      system_failure        = 1
      communication_failure = 2
      OTHERS                = 3.
