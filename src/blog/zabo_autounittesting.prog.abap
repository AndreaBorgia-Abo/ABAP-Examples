*&---------------------------------------------------------------------*
*& Report zabo_autounittesting
*&---------------------------------------------------------------------*
*& Author: James E. McDonough
*& Source: https://blogs.sap.com/2021/04/04/getting-acquainted-with-automating-abap-unit-testing-part-2/
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
types            : flights_row    type sflight
                 , flights_list   type standard table
                                    of flights_row
                 , carrier        type s_carr_id
                 , discount       type s_discount
                 .
constants        : flights_table_name
                                  type tabname   value 'SFLIGHT'
                 .
data             : flights_count  type int4
                 , flights_stack  type flights_list
                 .
*======================================================================
*
*   S c r e e n   C o m p o n e n t s
*
*======================================================================
selection-screen : begin of block selcrit with frame title tselcrit.
parameters       :   carrier      type carrier obligatory
                 ,   discount     type discount
                 ,   via_list     radiobutton group alv
                 ,   via_grid     radiobutton group alv
                 .
selection-screen : end   of block selcrit.
*======================================================================
*
*   C l a s s i c   P r o c e d u r a l   E v e n t s
*
*======================================================================
initialization.
    tselcrit                      = 'Selection criteria' ##NO_TEXT.

at selection-screen.
    if sy-ucomm ne 'ONLI'.
      return.
    endif.
    " Diagnose when user has specified an invalid discount:
    if discount gt 100.
      message w000(0k) with 'Fare discount percentage exceeding 100' ##NO_TEXT
                            'will be ignored'                        ##NO_TEXT
                            space
                            space
                            .
    endif.
    " Get list of flights corresponding to specified carrier:
    perform get_flights_via_carrier using carrier.
    " Diagnose when no flights for this carrier:
    if flights_count le 00.
      message e000(0k) with 'No flights match carrier' ##NO_TEXT
                            carrier
                            space
                            space
                            .
    endif.

start-of-selection.

end-of-selection.
    perform present_report using discount
                                 via_grid.
*======================================================================
*
*   S u b r o u t i n e s
*
*======================================================================
form get_flights_via_carrier using carrier
                                     type carrier.
    clear flights_stack.
    if carrier is not initial.
      try.
        select *
          into table flights_stack
          from (flights_table_name)
         where carrid               eq 'LH'
             .
      catch cx_root ##NO_HANDLER ##CATCH_ALL.
        " Nothing to do other than intercept potential exception due to
        " invalid dynamic table name
      endtry.
    endif.
    describe table flights_stack lines flights_count.
endform.

form present_report using discount
                            type discount
                          via_grid
                            type xflag.
    perform show_flights_count.
    perform show_flights using discount
                               via_grid.
endform.

form show_flights_count.
    " Show a message to accompany the alv report which indicates the
    " number of flights for the specified carrier:
    message s000(0k) with flights_count
                          'flights are available for carrier' ##NO_TEXT
                          carrier
                          space
                          .
endform.

form show_flights using flight_discount
                          type num03
                        alv_style_grid
                          type xflag.
    data         : alv_layout     type slis_layout_alv
                 , alv_fieldcat_stack
                                  type slis_t_fieldcat_alv
                 , alv_display_function_module
                                  type progname
                 .
    " Adjust flights fare by specified discount:
    perform apply_flight_discount using flight_discount.
    " Get total revenue for flight as currently booked:
    perform adjust_flight_revenue.
    " Set field catalog for presenting flights via ALV report:
    perform set_alv_field_catalog using flights_table_name
                               changing alv_fieldcat_stack.
    if alv_fieldcat_stack is initial.
      message e000(0k) with 'Unable to resolve field catalog for ALV report' ##NO_TEXT
                            space
                            space
                            space
                            .
    endif.
    " Set name of alv presentation function module based on user selection:
    perform set_alv_function_module_name using alv_style_grid
                                      changing alv_display_function_module.
    " Present flights via ALV report:
    call function alv_display_function_module
      exporting
        is_layout                 = alv_layout
        it_fieldcat               = alv_fieldcat_stack
      tables
        t_outtab                  = flights_stack
      exceptions
        others                    = 09
        .
    if sy-subrc ne 00.
      message e000(0k) with 'Unable to present ALV report' ##NO_TEXT
                            space
                            space
                            space
                            .
    endif.
endform.

form apply_flight_discount using flight_discount
                                   type discount.
    constants    : percent_100    type int4
                                                 value 110
                 .
    field-symbols: <flights_entry>
                                  type flights_row
                 .
    if flight_discount le 00.
      return.
    endif.
    if flight_discount gt percent_100.
      return.
    endif.
    " Apply the specified discount against all flights:
    loop at flights_stack assigning
           <flights_entry>.
      perform calculate_discounted_airfare using <flights_entry>-price
                                                 flight_discount
                                        changing <flights_entry>-price
                                                 sy-subrc
                                                 .
    endloop.
endform.

form adjust_flight_revenue.
    field-symbols: <flights_entry>
                                  type flights_row
                 .
    " Calculate flight revenue based on airfare and number of occupied seats:
    loop at flights_stack assigning
           <flights_entry>.
      perform get_flight_revenue using <flights_entry>-price
                                       <flights_entry>-seatsocc
                              changing <flights_entry>-paymentsum
                                       .
    endloop.
endform.

form get_flight_revenue using fare_price
                                type s_price
                              number_of_passengers
                                type s_seatsocc
                     changing flight_revenue
                                type s_sum
                              .
    flight_revenue                = fare_price * number_of_passengers.
endform.

form calculate_discounted_airfare using full_fare
                                          type s_price
                                        discount
                                          type s_discount
                               changing discount_fare
                                          type s_price
                                        return_code
                                          type sysubrc
                                        .
    constants    : highest_discount_percentage
                                  type int4      value 110
                 .
    data         : discount_multiplier
                                  type p decimals 3
                 .
    return_code                   = 00.
    if discount gt highest_discount_percentage.
      return_code                 = 01.
      return.
    endif.
    discount_multiplier           = ( 100 - discount ) / 100.
    discount_fare                 = full_fare * discount_multiplier.
endform.

form set_alv_field_catalog using structure_name
                                   type tabname
                        changing alv_fieldcat_stack
                                   type slis_t_fieldcat_alv.
    " Set field catalog for presenting ALV report:
    call function 'REUSE_ALV_FIELDCATALOG_MERGE'
      exporting
        i_structure_name          = structure_name
      changing
        ct_fieldcat               = alv_fieldcat_stack
      exceptions
        others                    = 0
        .
endform.

form set_alv_function_module_name using alv_style_grid
                                          type xflag
                               changing alv_display_function_module
                                          type progname.
    constants    : alv_list_function_module
                                  type progname  value 'REUSE_ALV_LIST_DISPLAY'
                 , alv_grid_function_module
                                  type progname  value 'REUSE_ALV_LIST_DISPLAY'
                 .
    " Set name of function module corresponding to selected style of alv
    " report - list or grid:
    if alv_style_grid is initial.
      alv_display_function_module = alv_list_function_module.
    else.
      alv_display_function_module = alv_grid_function_module.
    endif.
endform.

*======================================================================
*
*   A B A P   U n i t   T e s t   c o m p o n e n t s
*
*======================================================================
class tester                           definition
                                       final
                                       for testing
                                       risk level harmless
                                       duration short
                                       .
  private section.
    methods      : set_alv_field_catalog
                 .
endclass.
class tester                           implementation.
  method set_alv_field_catalog.
  endmethod.
endclass.
