class ZCL_AAB definition
  public
  final
  create public .

*”* public components of class ZCL_AAB
*”* do not include other source files here!!!
public section.

  class-methods BREAK_POINT
    importing
      !IV_AAB_ID type AAB_ID_NAME .
protected section.
*”* protected components of class ZCL_AAB
*”* do not include other source files here!!!
private section.
*”* private components of class ZCL_AAB
*”* do not include other source files here!!!

  class-methods EXISTS
    importing
      !IV_AAB_ID type AAB_ID_NAME
    returning
      value(RT_EXISTS) type CHAR1 .
  class-methods IS_BREAK_POINT_ACTIVE
    importing
      !IV_AAB_ID type AAB_ID_NAME
    returning
      value(RT_BREAK_POINT_IS_ACTIVE) type CHAR1 .
  class-methods TIME_DELAY .
ENDCLASS.



CLASS ZCL_AAB IMPLEMENTATION.


METHOD break_point.
  DATA: w_text TYPE string.

  IF exists( iv_aab_id ) = space.
    CONCATENATE `checkpoint group` iv_aab_id `does not exist`
      INTO w_text SEPARATED BY space.
    MESSAGE w_text TYPE `i`.
    EXIT.
  ENDIF.

  IF is_break_point_active( iv_aab_id ) = `x`.
    IF cl_gui_alv_grid=>offline( ) IS INITIAL.
*     Foreground
      BREAK-POINT.
    ELSE.
*     Background
      time_delay( ).
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD exists.
DATA: w_aab_id TYPE  aab_id_name.

  SELECT SINGLE name
  INTO w_aab_id
  FROM aab_id_prop
  WHERE name = iv_aab_id.
  CASE sy-subrc.
  WHEN 0.
    rt_exists = `X`.
  WHEN OTHERS.
    CLEAR rt_exists.
  ENDCASE.

ENDMETHOD.


METHOD is_break_point_active.
DATA: wa_aab_id_act TYPE aab_id_act,
      wt_aab_id_act TYPE aab_id_act_tab.
DATA w_bit_value TYPE i.
FIELD-SYMBOLS <mode_x> TYPE x.
CONSTANTS: c_breakpoint TYPE i VALUE 8.

  SELECT * FROM aab_id_act INTO TABLE wt_aab_id_act
  WHERE name       = iv_aab_id
    AND is_program = SPACE.
*
  LOOP AT wt_aab_id_act INTO wa_aab_id_act
                        WHERE username = SPACE
                           OR username = sy-uname.
    ASSIGN wa_aab_id_act-actmode TO <mode_x> CASTING.
    GET BIT c_breakpoint OF <mode_x> INTO w_bit_value.
    IF NOT w_bit_value IS INITIAL.
      rt_break_point_is_active = `X`.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD time_delay.
DATA: w_time_curr TYPE tims,
      w_time_end   TYPE tims.
DATA: w_timestamp TYPE timestampl.

  GET TIME STAMP FIELD w_timestamp.
  CONVERT TIME STAMP w_timestamp TIME ZONE sy-zonlo
  INTO TIME w_time_curr.
  w_time_end = w_time_curr  + 60.
  WHILE w_time_curr < w_time_end.
    GET TIME STAMP FIELD w_timestamp.
    CONVERT TIME STAMP w_timestamp TIME ZONE sy-zonlo
    INTO TIME w_time_curr.
  ENDWHILE.

ENDMETHOD.
ENDCLASS.
