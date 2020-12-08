*&---------------------------------------------------------------------*
*& Report  Z_TRIGGER_EVENT                                             *
*&                                                                     *
*&---------------------------------------------------------------------*
*& Author: unknown
*& Source: https://wiki.scn.sap.com/wiki/display/ABAP/Scheduling+background+job+by+triggering+an+event
*&---------------------------------------------------------------------*
REPORT  z_trigger_event                         .

* Note: I've tried with SAP_SYSTEM_START, it won't work!
CALL FUNCTION 'BP_EVENT_RAISE'
  EXPORTING
    eventid                = 'Z_TRIGGER_JOB'
  EXCEPTIONS
    bad_eventid            = 1
    eventid_does_not_exist = 2
    eventid_missing        = 3
    raise_failed           = 4
    OTHERS                 = 5.
IF sy-subrc <> 0.
  WRITE: 'Event failed to trigger'.
ELSE.
  WRITE: 'Event triggered'.
ENDIF.
