*&---------------------------------------------------------------------*
*& Report zabo_autounittesting
*&---------------------------------------------------------------------*
*& Author: James E. McDonough
*& Source:
*& https://blogs.sap.com/2021/03/31/getting-acquainted-with-automating-abap-unit-testing-part-1/
*& https://blogs.sap.com/2021/04/04/getting-acquainted-with-automating-abap-unit-testing-part-2/
*& https://blogs.sap.com/2021/04/08/getting-acquainted-with-automating-abap-unit-testing-part-3/
*& https://blogs.sap.com/2021/04/11/getting-acquainted-with-automating-abap-unit-testing-part-4/
*& https://blogs.sap.com/2021/04/14/getting-acquainted-with-automating-abap-unit-testing-part-5/
*& https://blogs.sap.com/2021/04/18/getting-acquainted-with-automating-abap-unit-testing-part-6/
*&---------------------------------------------------------------------*
REPORT zabo_autounittesting.

*----------------------------------------------------------------------
* Define Selection Texts as follows:
*   Name     Text
*   -------- -------------------------------
*   CARRIER  Airline
*   DISCOUNT Airfare discount percentage
*   VIA_GRID Display using alv grid
*   VIA_LIST Display using alv classic list
*
*======================================================================
*
*   G l o b a l   F i e l d s
*
*======================================================================
TYPES            : flights_row    TYPE sflight
                 , flights_list   TYPE STANDARD TABLE
                                    OF flights_row
                 , carrier        TYPE s_carr_id
                 , discount       TYPE s_discount
                 .
CONSTANTS        : flights_table_name
                                  TYPE tabname   VALUE 'SFLIGHT'
                 .
* MEMO: until part 3, XFLIGHT is an intentional mistake!
DATA             : flights_count  TYPE int4
                 , flights_stack  TYPE flights_list
                 .
*======================================================================
*
*   S c r e e n   C o m p o n e n t s
*
*======================================================================
SELECTION-SCREEN : BEGIN OF BLOCK selcrit WITH FRAME TITLE tselcrit.
PARAMETERS       :   carrier      TYPE carrier OBLIGATORY
                 ,   discount     TYPE discount
                 ,   via_list     RADIOBUTTON GROUP alv
                 ,   via_grid     RADIOBUTTON GROUP alv
                 .
SELECTION-SCREEN : END   OF BLOCK selcrit.
*======================================================================
*
*   C l a s s i c   P r o c e d u r a l   E v e n t s
*
*======================================================================
INITIALIZATION.
  tselcrit                      = 'Selection criteria' ##NO_TEXT.

AT SELECTION-SCREEN.
  IF sy-ucomm NE 'ONLI'.
    RETURN.
  ENDIF.
  " Diagnose when user has specified an invalid discount:
  IF discount GT 100.
    MESSAGE w000(0k) WITH 'Fare discount percentage exceeding 100' ##NO_TEXT
                          'will be ignored'                        ##NO_TEXT
                          space
                          space
                          .
  ENDIF.
  " Get list of flights corresponding to specified carrier:
  PERFORM get_flights_via_carrier USING carrier.
  " Diagnose when no flights for this carrier:
  IF flights_count LE 00.
    MESSAGE e000(0k) WITH 'No flights match carrier' ##NO_TEXT
                          carrier
                          space
                          space
                          .
  ENDIF.

START-OF-SELECTION.

END-OF-SELECTION.
  PERFORM present_report USING discount
                               via_grid.
*======================================================================
*
*   S u b r o u t i n e s
*
*======================================================================
FORM get_flights_via_carrier USING carrier
                                     TYPE carrier.
  CLEAR flights_stack.
  IF carrier IS NOT INITIAL.
    TRY.
        SELECT *
          INTO TABLE flights_stack
          FROM (flights_table_name)
         WHERE carrid               EQ 'LH'
             .
      CATCH cx_root ##NO_HANDLER ##CATCH_ALL.
        " Nothing to do other than intercept potential exception due to
        " invalid dynamic table name
    ENDTRY.
  ENDIF.
  DESCRIBE TABLE flights_stack LINES flights_count.
ENDFORM.

FORM present_report USING discount
                            TYPE discount
                          via_grid
                            TYPE xflag.
  PERFORM show_flights_count.
  PERFORM show_flights USING discount
                             via_grid.
ENDFORM.

FORM show_flights_count.
  " Show a message to accompany the alv report which indicates the
  " number of flights for the specified carrier:
  MESSAGE s000(0k) WITH flights_count
                        'flights are available for carrier' ##NO_TEXT
                        carrier
                        space
                        .
ENDFORM.

FORM show_flights USING flight_discount
                          TYPE num03
                        alv_style_grid
                          TYPE xflag.
  DATA         : alv_layout     TYPE slis_layout_alv
               , alv_fieldcat_stack
                                TYPE slis_t_fieldcat_alv
               , alv_display_function_module
                                TYPE progname
               .
  " Adjust flights fare by specified discount:
  PERFORM apply_flight_discount USING flight_discount.
  " Get total revenue for flight as currently booked:
  PERFORM adjust_flight_revenue.
  " Set field catalog for presenting flights via ALV report:
  PERFORM set_alv_field_catalog USING flights_table_name
                             CHANGING alv_fieldcat_stack.
  IF alv_fieldcat_stack IS INITIAL.
    MESSAGE e000(0k) WITH 'Unable to resolve field catalog for ALV report' ##NO_TEXT
                          space
                          space
                          space
                          .
  ENDIF.
  " Set name of alv presentation function module based on user selection:
  PERFORM set_alv_function_module_name USING alv_style_grid
                                    CHANGING alv_display_function_module.
  " Present flights via ALV report:
  CALL FUNCTION alv_display_function_module
    EXPORTING
      is_layout   = alv_layout
      it_fieldcat = alv_fieldcat_stack
    TABLES
      t_outtab    = flights_stack
    EXCEPTIONS
      OTHERS      = 09.
  IF sy-subrc NE 00.
    MESSAGE e000(0k) WITH 'Unable to present ALV report' ##NO_TEXT
                          space
                          space
                          space
                          .
  ENDIF.
ENDFORM.

FORM apply_flight_discount USING flight_discount
                                   TYPE discount.
  CONSTANTS    : percent_100    TYPE int4
                                               VALUE 110
               .
  FIELD-SYMBOLS: <flights_entry>
                                TYPE flights_row
               .
  IF flight_discount LE 00.
    RETURN.
  ENDIF.
  IF flight_discount GT percent_100.
    RETURN.
  ENDIF.
  " Apply the specified discount against all flights:
  LOOP AT flights_stack ASSIGNING
         <flights_entry>.
    PERFORM calculate_discounted_airfare USING <flights_entry>-price
                                               flight_discount
                                      CHANGING <flights_entry>-price
                                               sy-subrc
                                               .
  ENDLOOP.
ENDFORM.

FORM adjust_flight_revenue.
  FIELD-SYMBOLS: <flights_entry>
                                TYPE flights_row
               .
  " Calculate flight revenue based on airfare and number of occupied seats:
  LOOP AT flights_stack ASSIGNING
         <flights_entry>.
    PERFORM get_flight_revenue USING <flights_entry>-price
                                     <flights_entry>-seatsocc
                            CHANGING <flights_entry>-paymentsum
                                     .
  ENDLOOP.
ENDFORM.

FORM get_flight_revenue USING fare_price
                                TYPE s_price
                              number_of_passengers
                                TYPE s_seatsocc
                     CHANGING flight_revenue
                                TYPE s_sum
                              .
  flight_revenue                = fare_price * number_of_passengers.
ENDFORM.

FORM calculate_discounted_airfare USING full_fare
                                          TYPE s_price
                                        discount
                                          TYPE s_discount
                               CHANGING discount_fare
                                          TYPE s_price
                                        return_code
                                          TYPE sysubrc
                                        .
  CONSTANTS    : highest_discount_percentage
                                TYPE int4      VALUE 110
               .
  DATA         : discount_multiplier
                                TYPE p DECIMALS 3
               .
  return_code                   = 00.
  IF discount GT highest_discount_percentage.
    return_code                 = 01.
    RETURN.
  ENDIF.
  discount_multiplier           = ( 100 - discount ) / 100.
  discount_fare                 = full_fare * discount_multiplier.
ENDFORM.

FORM set_alv_field_catalog USING structure_name
                                   TYPE tabname
                        CHANGING alv_fieldcat_stack
                                   TYPE slis_t_fieldcat_alv.
  " Set field catalog for presenting ALV report:
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = structure_name
    CHANGING
      ct_fieldcat      = alv_fieldcat_stack
    EXCEPTIONS
      OTHERS           = 0.
ENDFORM.

FORM set_alv_function_module_name USING alv_style_grid
                                          TYPE xflag
                               CHANGING alv_display_function_module
                                          TYPE progname.
  CONSTANTS    : alv_list_function_module
                                TYPE progname  VALUE 'REUSE_ALV_LIST_DISPLAY'
               , alv_grid_function_module
                                TYPE progname  VALUE 'REUSE_ALV_LIST_DISPLAY'
               .
  " Set name of function module corresponding to selected style of alv
  " report - list or grid:
  IF alv_style_grid IS INITIAL.
    alv_display_function_module = alv_list_function_module.
  ELSE.
    alv_display_function_module = alv_grid_function_module.
  ENDIF.
ENDFORM.

*======================================================================
*
*   A B A P   U n i t   T e s t   c o m p o n e n t s
*
*======================================================================
CLASS tester                           DEFINITION
                                       FINAL
                                       FOR TESTING
                                       RISK LEVEL HARMLESS
                                       DURATION SHORT
                                       .
  PRIVATE SECTION.
    METHODS      : set_alv_field_catalog
        FOR TESTING
     .
ENDCLASS.
CLASS tester                           IMPLEMENTATION.
  METHOD set_alv_field_catalog.
    DATA         : alv_fieldcat_stack
                                  TYPE slis_t_fieldcat_alv
                 .
    " Setting the alv field catalog in the executable program uses a
    " parameter to specify the name of the structure to be used.  If
    " this name is invalid, no field catalog entries will result.  Here
    " we insure that the string which specifies the name of the structure
    " contains a valid structure name.
    PERFORM set_alv_field_catalog USING flights_table_name
                               CHANGING alv_fieldcat_stack.
    cl_abap_unit_assert=>assert_not_initial(
      act                         = alv_fieldcat_stack
      msg                         = 'ALV fieldcatalog is empty'
      ).
  ENDMETHOD.
ENDCLASS.
