*&---------------------------------------------------------------------*
*& Report zabo_atdf_demo
*&---------------------------------------------------------------------*
*& Author: James E. McDonough
*& Source: https://blogs.sap.com/2018/05/08/exploring-the-abap-test-double-framework/
*&---------------------------------------------------------------------*
REPORT zabo_atdf_demo.
"----------------------------------------------------------------------
" ABAP Test Double Framework (ATDF) demo program.
"
" Jim McDonough - May 6, 2018
"
" Purpose -
"
" This program illustrates the use of the ABAP Test Double Framework with
" ABAP Unit tests for providing test doubles (mock objects, stubs, spies,
" fakes, etc.) as the component(s) on which the code under test depends.
"
" Source material used -
"
" Most of the source code was copied and extensively modified from the
" example ABAP source found in the following blog by Prajul Meyana:
" o https://blogs.sap.com/2015/01/05/abap-test-double-framework-an-introduction/
" Indeed, the source code in that blog is itself mostly a copy of the local
" ABAP Unit test associated with global class cl_td_expense_manager, a class
" provided for the purpose of demonstrating ATDF.  To see the ABAP Unit test
" code written for class cl_td_expense_manager, do the following:
"   o Invoke transaction SE24
"   o Specify object type cl_td_expense_manager, then press Display
"   o From the menu select:
"       Goto > Local Definitions/Implementations > Local Test Classes
"
" In this case the code under test is an instance of standard SAP global class
" cl_td_expense_manager and the corresponding component on which it depends, for
" which a test double is provided in these examples, is a currency converter object
" that implements the standard SAP global interface if_td_currency_converter.
"
" This demo program represents a simple example of how the ABAP Test Double
" Framework can be used, taking advantage of the following characteristics
" of the components used:
"   o The instance of the code under test -- an expense manager of the type
"     cl_td_expense_manager -- has a simple set of only 2 public methods
"     that can be explicitly invoked:
"     - ADD_EXPENSE_ITEM
"     - CALCULATE_TOTAL_EXPENSE
"     In addition, it has only a single component on which it depends --
"     an instance of a currency converter implementing the interface
"     if_td_currency_converter, an interface which also has a simple set
"     of only 2 public methods:
"     - CONVERT
"     - CONVERT_TO_BASE_CURRENCY
"   o Class cl_td_expense_manager has a constructor method which enables
"     dependency injection of its currency converter, allowing it to be
"     provided with a test double for the currency converter during these
"     ABAP Unit tests.
"
" Current constraints of ATDF -
"
" Currently the ABAP Test Double Framework is based upon the use of a global
" interface to provide the definition of the methods to be simulated by the
" test doubles. Specifically, there is no support for defining test doubles
" for any of these other options:
"   o global classes
"   o local interfaces
"   o local classes
"
" General steps for using ATDF -
"
" The use of ATDF relies on a test double being configured to return
" specific values based on a specific set of parameters accompanying a
" call to a method provided by the test double. This is generally a
" 4-step process:
"   o In step 1, the static method "create" of class cl_abap_testdouble
"     is invoked, passing the name of a global interface implemented by a
"     component on which the code under test is dependent, to create an
"     instance of a test double that can accommodate all the methods
"     defined by the global interface.
"   o In step 2, an instance of the code under test is created, making
"     available to it the test double created in step 1.
"   o In step 3, the static method "configure_call" of class cl_abap_testdouble
"     is invoked, passing the test double instance, and, in its simplest
"     format (see further explanation below), specifying the return values to
"     be used with a test double method call. The actual method call for which
"     these value are to be returned is specified in the next step.
"   o In step 4, the method of the test double that is to return the values
"     specified in step 3 is invoked specifying the parameter values that
"     are to trigger the return values specified in step 3.
" Once established accordingly, calls by the code under test to the methods
" of the component on which it depends will be intercepted by the test double
" and provided with the configured return values. In effect, the code under
" test is oblivious to the fact that it is invoking the methods of a test double.
"
" Technical aspects of cl_abap_testdouble -
"
" There are a variety of ways to invoke static method "configure_call" of class
" cl_abap_testdouble, the simplest of which includes specfying a return value to
" be provided to the caller of the test double method. In fact, a call to
" method "configure_call" does not configure anything, but it does return a
" reference to an instance of if_abap_testdouble_config (that is, an object
" implementing the global interface if_abap_testdouble_config). It is this
" object that gets called to apply some aspect of how the test double is to
" respond to method calls. Interface if_abap_testdouble_config provides the
" following methods that can be invoked against an instance implementing it:
"   o returning
"   o set_parameter
"   o raise exception
"   o raise event
"   o times
"   o ignore_parameter
"   o ignore_all_parameters
"   o and_expect
"   o set_answer
"   o set_matcher
" Most of these methods have a signature indicating to return an instance of
" if_abap_testdouble_config. Accordingly, such an instance can invoke any
" of these methods, and an instance will be returned to the caller reflecting
" the effect of the method call. The returned instance can be the same instance
" updated accordingly or can be a new instance. A subsequent call using the
" instance to a different method will return a new/updated instance also
" reflecting the effect of the called method. Indeed, this process can be
" repeated as long as is necessary to fully configure the response the code
" under test should receive upon invoking a method of the test double.
"
" The Builder design pattern -
"
" An object accommodating such calls conforms to the object-oriented design
" pattern known as the Builder, where a sequence of such method calls results
" in an object becoming "built" to the necessary end state. So, for instance,
" to configure a test double response to indicate the following attributes
" (one that you will see used in this demo):
"   - returning = 80
"   - times = 2
" we could use the following sequence of ABAP statements to construct such
" an object:
"
"   data currency_converter_double type ref to if_td_currency_converter.
"   data test_double_configuration type ref to if_abap_testdouble_config.
"
"   test_double_configuration = cl_abap_testdouble=>configure_call( currency_converter_double ).
"   test_double_configuration = test_double_configuration->returning( 80 ).
"   test_double_configuration = test_double_configuration->times( 2 ).
"
" In the example code above, the field test_double_configuration is a helper variable
" holding the object created/updated with each new method invocation, however ABAP
" enables calling such methods to be strung together without the need for the
" helper variable at all, as in:
"
" cl_abap_testdouble=>configure_call( currency_converter_double )->returning( 80 )->times( 2 ).
"
" With this format, the object returned from the call to cl_abap_testdouble=>configure_call
" is used as the object on which the call to method returning( 80 ) is made, and its returning
" object is subsequently used on the call to method times( 2 ).
"
" Your turn -
"
" All of the tests in this ABAP Unit test demo are currently set to fail.
" In each case, the correction to make the ABAP Unit test pass is a commented
" line following the failing line.  Your task is to run all the ABAP Unit
" tests, see that they fail, and they correct them, one by one, to see that
" the tests all pass.  So follow these steps:
"   o Use the ABAP editor SE38 to make a copy of this program.
"   o Activate the copied program.
"   o Run the ABAP Unit tests of this program in one of the following ways:
"     - Via SE38, select from the menu: Program > Execute > Unit Tests.
"     - Via SE38, press the key combination Ctrl+Shift+F10.
"   o Make a single correction by adjusting the ABAP code.  Each failing line
"     or set of lines is marked with "ABAP Unit failure", and each correct
"     line or set of lines is marked with "ABAP Unit success".
"   o Run the ABAP Unit test again to see that a failing test you changed
"     now passes.
"   o Repeat this process until all test pass.
"----------------------------------------------------------------------

class new_currency_code_observer        definition
                                        final
                                        .
  " This class exists solely to act as an observer responding to the
  " event new_currency_code of interface if_td_currency_converter.
  public section.
    methods      : constructor
                 , get_currency_code
                     returning
                       value(currency_code)
                         type string
                 , handle_new_currency_code
                     for event new_currency_code
                            of if_td_currency_converter
                     importing
                       currency_code
                 .
  private section.
    data         : new_currency_code
                                  type string
                 .
endclass.
class new_currency_code_observer       implementation.
  method constructor.
    " At the start, we want our own value for new currency
    " code to be cleared ...
    clear me->new_currency_code.
    " ... and we want to be able to respond to the raised event
    " new_currency_code of if_td_currency_converter:
    set handler handle_new_currency_code for all instances.
  endmethod.
  method handle_new_currency_code.
    " We are now responding to the raised event
    " new_currency_code of if_td_currency_converter and
    " we want to overwrite our own value for new currency code
    " using the value accompanying the raised event:
    me->new_currency_code         = currency_code.
  endmethod.
  method get_currency_code.
    " Some caller is requesting the new currency code this object
    " has, presumably obtained after having responded to the raised
    " event new_currency_code of if_td_currency_converter:
    currency_code                 = me->new_currency_code.
  endmethod.
endclass.

class currency_exchanger               definition
                                       final
                                       .
  " This class exists only because global class cl_td_expense_manager
  " does not invoke method convert_to_base_currency of interface
  " if_td_currency_converter, one of only two methods this interface
  " specifies.  Whereas the local ABAP Unit test accompanying class
  " cl_td_expense_manager seems to establish a test for invoking method
  " convert_to_base_currency of interface if_td_currency_converter, it
  " is incomplete because the code implemented for cl_td_expense_manager
  " never invokes this interface method.  As such, the ABAP Unit
  " test code written for cl_td_expense_manager to test this call
  " (lines 73 through 80) is never executed to the point where it can be
  " determined whether the test double would respond accordingly.
  public section.
    methods      : constructor
                     importing
                       currency_converter
                         type ref to if_td_currency_converter
                 , exchange_currency
                    importing
                      amount
                        type i
                      source_currency
                        type string
                    exporting
                      base_currency
                        type string
                      base_curr_amount
                        type i
                 .
  private section.
    data         : currency_converter
                     type ref
                       to if_td_currency_converter
                 .
endclass.
class currency_exchanger               implementation.
  method constructor.
    me->currency_converter        = currency_converter.
  endmethod.
  method exchange_currency.
    me->currency_converter->convert_to_base_currency(
      exporting
        amount                    = amount
        source_currency           = source_currency
      importing
        base_currency             = base_currency
        base_curr_amount          = base_curr_amount
      ).
  endmethod.
endclass.

class custom_matcher                   definition.
  public section.
    interfaces   : if_abap_testdouble_matcher
                 .
endclass.
class custom_matcher                   implementation.
  method if_abap_testdouble_matcher~matches.
    data         : act_currency_code
                                  type ref to data
                 , conf_currency_code
                                  type ref to data
                 .
    field-symbols: <act_currency> type string
                 , <conf_currency>
                                  type string
                 .
    result                        = abap_false.
    if method_name eq 'CONVERT'. " Must be specified as upper-case value
      act_currency_code           = actual_arguments->get_param_importing( 'source_currency' ).
      conf_currency_code          = configured_arguments->get_param_importing( 'source_currency' ).
      assign act_currency_code->*  to <act_currency>.
      assign conf_currency_code->* to <conf_currency>.
      if <act_currency>  is assigned and
         <conf_currency> is assigned.
        if <act_currency> cp <conf_currency>.
          result                  = abap_true.
        endif.
      endif.
    endif.
  endmethod.
endclass.

class custom_answer                    definition.
  public section.
    interfaces   : if_abap_testdouble_answer
                 .
endclass.
class custom_answer                     implementation.
  method if_abap_testdouble_answer~answer.
    data         : source_currency_code
                                  type ref to data
                 , target_currency_code
                                  type ref to data
                 , amount         type ref to data
                 .
    field-symbols: <source_currency_code>
                                  type string
                 , <target_currency_code>
                                  type string
                 , <amount>       type  i
                 .
    result->set_param_returning( 00 ).
    if method_name eq 'CONVERT'.  " Must be specified as upper-case value
      source_currency_code        = arguments->get_param_importing( 'source_currency' ).
      target_currency_code        = arguments->get_param_importing( 'target_currency' ).
      amount                      = arguments->get_param_importing( 'amount' ).
      assign source_currency_code->* to <source_currency_code>.
      assign target_currency_code->* to <target_currency_code>.
      assign amount->*               to <amount>.
      if <source_currency_code> is assigned and
         <target_currency_code> is assigned and
         <amount>               is assigned.
        if <source_currency_code> eq 'INR' and
           <target_currency_code> eq 'EUR'.
          result->set_param_returning( <amount> / 80 ).
        endif.
      endif.
    endif.
  endmethod.
endclass.

class abap_test_double_examples        definition
                                       final
                                       for testing
                                       duration short
                                       risk level harmless
                                       .
  private section.
    types        : currency       type string
                 .
    constants    : currency_converter_interface
                                  type seoclsname
                                                 value 'if_td_currency_converter'
                 , euro           type abap_test_double_examples=>currency
                                                 value 'EUR'
                 , indian_rupee
                                  type abap_test_double_examples=>currency
                                                 value 'INR'
                 , us_dollar      type abap_test_double_examples=>currency
                                                 value 'USD'
                 .
    data         : currency_converter_double
                                  type ref to if_td_currency_converter
                 , expense_manager
                                  type ref to cl_td_expense_manager
                 .
    methods      : setup
                 , teardown
                 , simple_configuration
                     for testing raising cx_static_check
                 , configuration_variation_01
                     for testing raising cx_static_check
                 , configuration_variation_02
                     for testing raising cx_static_check
                 , configuration_variation_03
                     for testing raising cx_static_check
                 , configuration_exception
                     for testing raising cx_static_check
                 , configuration_event
                     for testing raising cx_static_check
                 , configuration_times
                     for testing raising cx_static_check
                 , custom_answer
                     for testing raising cx_static_check
                 , custom_matcher
                     for testing raising cx_static_check
                 , verify_test_double_interaction
                     for testing raising cx_static_check
                 .
endclass.
class abap_test_double_examples        implementation.
  method setup.
    " Create both code under test (expense manager) as well as the
    " test double masquerading as the ccmponent on which it depends
    " (currency converter):
    " Create test double object:
    me->currency_converter_double ?= cl_abap_testdouble=>create( me->currency_converter_interface ).
    " Create the object to be tested, injecting the test double upon
    " which it shall depend for the purpose of testing:
    create object me->expense_manager
      exporting
        currency_converter        = me->currency_converter_double
        .
  endmethod.
  method teardown.
    " Destroy both code under test (expense manager) as well as the
    " test double masquerading as the ccmponent on which it depends
    " (currency converter):
    clear: me->currency_converter_double
         , me->expense_manager
         .
  endmethod.
  method simple_configuration.
    constants    : fixed_returning_expense
                                  type i         value 80
                 .
    data         : expected_expense
                                  type i
                 , actual_expense
                                  type i
                 .
    " In this test, the test double always will return the value
    " fixed_returning_expense when the code under test invokes its
    " convert method.

    " Configuring the call to method 'convert' of the test double
    " always to return a known value when invoked by the code under test:
    " Step 1: Set the desired returning value for the called method
    "         of the test double:
    " Step 2: Specify the method to be called and the parameters
    "         associated with the call that will return the value
    "         configured in the previous step:
    " Step 3: Invoke a method upon the code under test that will cause
    "         it to return a value affected by the call it makes
    "         to the test double:
    " Step 4: Assert that the actual value returned by the previous step
    "         is the same as the expected value it should return:

    " Step 1: Set the desired returning value for the called method
    "         of the test double:
    cl_abap_testdouble=>configure_call( me->currency_converter_double
      )->returning( fixed_returning_expense
      ).
    " Step 2: Specify the method to be called and the parameters
    "         associated with the call that will return the value
    "         configured in the previous step:
    me->currency_converter_double->convert(
      exporting
        amount                    = 100
        source_currency           = me->us_dollar
        target_currency           = me->euro
      ).
    " Step 3: Invoke a method upon the code under test that will cause
    "         it to return an actual value affected by the call it makes
    "         to the test double:
    " 3a) Call the expense manager to provide it with an entry having
    "     an amount and source currency code matching the parameters used
    "     in the previous step to configure the call to the convert method
    "     of the test double:
    me->expense_manager->add_expense_item(
      exporting
        description               = 'Line item 1'
        currency_code             = me->us_dollar
        amount                    = 100
      ).
    " 3b) Now that the expense manager has an entry to use, call its
    "     method calculate_total_expense, passing the target currency code
    "     matching the parameters used in step 2 to configure the call
    "     to the convert method of the test double; this will cause
    "     the expense manager to invoke the convert method of the test
    "     double using the same parameters as configured in step 2, and
    "     as a consequence will return the amount fixed_returning_expense:
    actual_expense                = me->expense_manager->calculate_total_expense( currency_code = me->euro ).
    " Step 4: Assert that the actual value returned by the previous step
    "         is the same as the expected value it should return:
    expected_expense              = fixed_returning_expense.
    cl_abap_unit_assert=>assert_equals(
      exp                         = expected_expense
      act                         = actual_expense + 1 " ABAP Unit failure
*      act                         = actual_expense     " ABAP Unit success
      ).
  endmethod.
  method configuration_variation_01.
    constants    : fixed_base_currency_amount
                                  type i         value 150
                 .
    data         : currency_exchanger
                                  type ref
                                    to currency_exchanger
                 , base_currency  type string
                 , base_curr_amount
                                  type i
                 .
    " This is a special case.  We are not going to use the code under
    " test provided by the setup method, but are going to instantiate
    " our own object, where the instance of the class to be tested is
    " not one of global class cl_td_expense_manager but is instead a
    " an instance of local class currency_exchanger (see comments in
    " local class currency_exchanger definition for more explanation
    " of the associated issues).  To do this:
    "   1) Destroy the expense manager established by the setup method.
    "   2) Create an instance of currency_exchanger, injecting it with
    "      the currency converter already created by the setup method.

    " 1) Destroy the expense manager established by the setup method:
    clear me->expense_manager.
    " 2) Create an instance of currency_exchanger, injecting it with
    "    the currency converter already created by the setup method:
    create object currency_exchanger
      exporting
        currency_converter        = me->currency_converter_double
        .
    " Configuration for exporting parameters
    cl_abap_testdouble=>configure_call( me->currency_converter_double
      )->set_parameter( name      = 'base_currency'
                        value     = me->euro
      )->set_parameter( name      = 'base_curr_amount'
                        value     = fixed_base_currency_amount
      ).
    me->currency_converter_double->convert_to_base_currency(
      exporting
        amount                    = 100
        source_currency           = me->us_dollar
      ).
    " Actual method call
    currency_exchanger->exchange_currency(
      exporting
        amount                    = 100
        source_currency           = me->us_dollar
      importing
        base_currency             = base_currency
        base_curr_amount          = base_curr_amount
      ).
    " Assertions
    cl_abap_unit_assert=>assert_equals(
      exp                         = fixed_base_currency_amount
      act                         = base_curr_amount + 1 " ABAP Unit failure
*      act                         = base_curr_amount     " ABAP Unit success
      ).
    cl_abap_unit_assert=>assert_equals(
      exp                         = me->euro
      act                         = me->us_dollar " ABAP Unit failure
*      act                         = base_currency " ABAP Unit success
      ).
  endmethod.
  method configuration_variation_02.
    constants    : fixed_returning_expense
                                  type i         value 55
                 .
    data         : expected_expense
                                  type i
                 , actual_expense
                                  type i
                 .
    " In this test, the test double always will return the value
    " fixed_returning_expense when the code under test invokes its
    " convert method using currency US dollar, regardless of the value
    " specified for the amount parameter.

    " Configuration ignoring one parameter. fixed_returning_expense gets returned if
    " source currency = US dollar, target currency = Euro and any value for amount.
    cl_abap_testdouble=>configure_call( me->currency_converter_double
      )->returning( fixed_returning_expense
      )->ignore_parameter( 'amount'
      ).
    me->currency_converter_double->convert(
      exporting
        amount                    = 0 "dummy value because amount is a non optional parameter
        source_currency           = me->us_dollar
        target_currency           = me->euro
      ).
    " Add expense item
    me->expense_manager->add_expense_item(
      exporting
        description               = 'Line item 1'
        currency_code             = me->us_dollar
        amount                    = 100
      ).
    expected_expense              = expected_expense + fixed_returning_expense.
    " Add expense item
    me->expense_manager->add_expense_item(
      exporting
        description               = 'Line item 2'
        currency_code             = me->us_dollar
        amount                    = 200
      ).
    expected_expense              = expected_expense + fixed_returning_expense.
    " Add expense item
    me->expense_manager->add_expense_item(
      exporting
        description               = 'Line item 3'
        currency_code             = me->us_dollar
        amount                    = 400
      ).
    expected_expense              = expected_expense + fixed_returning_expense.
    " Actual method call
    actual_expense                = me->expense_manager->calculate_total_expense( currency_code = me->euro ).
    " Assertion
    cl_abap_unit_assert=>assert_equals(
      exp                         = expected_expense
      act                         = actual_expense + 1 " ABAP Unit failure
*      act                         = actual_expense     " ABAP Unit success
      ).
  endmethod.
  method configuration_variation_03.
    constants    : fixed_returning_expense
                                  type i         value 55
                 .
    data         : expected_expense
                                  type i
                 , actual_expense
                                  type i
                 .
    " In this test, the test double always will return the value
    " fixed_returning_expense when the code under test invokes its
    " convert method regardless of any parameters values specified.

    " Configuration ignoring all parameters. fixed_returning_expense gets
    " returned for any input.
    cl_abap_testdouble=>configure_call( me->currency_converter_double
      )->returning( fixed_returning_expense
      )->ignore_all_parameters(
      ).
    me->currency_converter_double->convert(
      exporting
        amount                    = 0 "dummy value
        source_currency           = me->us_dollar "dummy value
        target_currency           = me->euro "dummy value
      ).
    " Add expense item
    me->expense_manager->add_expense_item(
      exporting
        description               = 'Line item 1'
        currency_code             = 'XYZ' " deliberate invalid currency code
        amount                    = 100
      ).
    expected_expense              = expected_expense + fixed_returning_expense.
    " Add expense item
    me->expense_manager->add_expense_item(
      exporting
        description               = 'Line item 2'
        currency_code             = 'ABC' " deliberate invalid currency code
        amount                    = 200
      ).
    expected_expense              = expected_expense + fixed_returning_expense.
    " Add expense item
    me->expense_manager->add_expense_item(
      exporting
        description               = 'Line item 3'
        currency_code             = 'HIJ' " deliberate invalid currency code
        amount                    = 400
      ).
    expected_expense              = expected_expense + fixed_returning_expense.
    " Actual method call
    actual_expense                = me->expense_manager->calculate_total_expense( currency_code = me->euro ).
    " Assertion
    cl_abap_unit_assert=>assert_equals(
      exp                         = expected_expense
      act                         = actual_expense + 1 " ABAP Unit failure
*      act                         = actual_expense     " ABAP Unit success
      ).
  endmethod.
  method configuration_exception.
    data         : actual_expense ##NEEDED
                                  type i
                 , exception      type ref to cx_td_currency_exception
                 .
    " In this test, the test double always will raise the
    " exception cx_td_currency_exception.

    " Instantiate the exception object.
    "   Note: This step would not be necessary in production code since
    "         the act of raising the exception would instantiate this
    "         instance.  With the ABAP Test Double Framework in control
    "         of raising the exception, the ABAP Unit test will fail with
    "         an exception CX_ATD_EXCEPTION accompanied by the text
    "         "[ ABAP Testdouble Framework ] Exception object not bound"
    "         unless there is an object instance already bound to this
    "         reference variable.
    create object exception.
    " Configuration for exception. the specified exception gets raised if
    " amount = -1, source_currency = US dollar and target_currency = Euro
    cl_abap_testdouble=>configure_call( me->currency_converter_double
      )->raise_exception( exception
      ).
    " The instantiated exception object only needs to remain instantiated
    " for the duration of the preceding configuration call:
    clear exception.
    " ABAP Unit failure due to the inactive ABAP statements from here ...
*    me->currency_converter_double->convert(
*      exporting
*        amount                    = -1
*        source_currency           = me->us_dollar
*        target_currency           = me->euro
*      ).
    " ... to here. ABAP Unit failure
    " Add expense item
    me->expense_manager->add_expense_item(
      exporting
        description               = 'Line item 1'
        currency_code             = me->us_dollar
        amount                    = -1
      ).
    try.
      " Actual method call
      actual_expense              = me->expense_manager->calculate_total_expense( currency_code = me->euro ).
      " We should never get this far; exception should have been raised
      cl_abap_unit_assert=>fail(
        msg                       = 'cx_td_currency_exception not raised'
        ).
    catch cx_td_currency_exception ##NO_HANDLER.
      " No action; this is the intended path
    endtry.
  endmethod.

 method configuration_event.
   data          : actual_expense ##NEEDED
                                  type i
                 , event_params
                                  type abap_parmbind_tab
                 , event_param    type abap_parmbind
                 , event_observer type ref to new_currency_code_observer
                 .
    field-symbols: <value>        type string
                 .
    " In this test, the test double always will raise the
    " event new_currency_code of if_td_currency_converter.

    " Create event observer:
    create object event_observer.
    " Configuration for event. 'new_currency_code' event gets raised if the
    " source_currency = Indian rupee
    event_param-name              = 'currency_code'.
    create data event_param-value type string.
    assign event_param-value->* to <value>.
    <value>                       = me->indian_rupee.
    insert event_param into table event_params.
    cl_abap_testdouble=>configure_call( me->currency_converter_double
      )->raise_event( name        = 'new_currency_code'
                      parameters  = event_params
      )->ignore_parameter( 'target_currency'
      )->ignore_parameter( 'amount'
      ).
    " ABAP Unit failure due to the inactive ABAP statements from here ...
*    me->currency_converter_double->convert(
*      exporting
*        amount                    = 00 " dummy value
*        source_currency           = me->indian_rupee
*        target_currency           = '' " dummy value
*      ).
    " ... to here. ABAP Unit failure
    " Add expense item
    me->expense_manager->add_expense_item(
      exporting
        description               = 'Line item 1'
        currency_code             = me->indian_rupee
        amount                    = 100
      ).
    " Actual method call
    actual_expense                = me->expense_manager->calculate_total_expense( currency_code = me->euro ).
    " At this point we do not care what value was returned into
    " field actual_expense.  Instead, we care whether the call caused
    " the test double to raise the event new_currency_code of
    " if_td_currency_converter.  Retrieving the value returned by
    " invoking method get_currency_code of the event observer will
    " tell us whether or not the event was raised and the event
    " observer responded to it:
    cl_abap_unit_assert=>assert_equals(
      exp                         = me->indian_rupee
      act                         = event_observer->get_currency_code( )
      ).
  endmethod.
  method configuration_times.
    data         : expected_expense
                                  type i
                 , actual_expense
                                  type i
                 .
    " In this test, the test double will return values depending
    " on the sequence of calls made to it.  In this case it will
    " be configured to return the value 80 for the first 2 calls
    " where the calling parameters match and to return the value
    " 40 for any subsequent calls where the calling parmaters match.

    " Configuration for returning 80 for 2 times
    cl_abap_testdouble=>configure_call( me->currency_converter_double
      )->returning( 80
      )->times( 2
      ).
    me->currency_converter_double->convert(
      exporting
        amount                    = 100
        source_currency           = me->us_dollar
        target_currency           = me->euro
      ).
    " Configuration for returning 40 the next time
    cl_abap_testdouble=>configure_call( me->currency_converter_double
      )->returning( 40
      ).
    me->currency_converter_double->convert(
      exporting
        amount                    = 100
        source_currency           = me->us_dollar
        target_currency           = me->euro
      ).
    " Add expense item
    me->expense_manager->add_expense_item(
      exporting
        description               = 'Line item 1'
        currency_code             = me->us_dollar
        amount                    = 100
      ).
    expected_expense              = expected_expense + 80.
    " Add expense item
    me->expense_manager->add_expense_item(
      exporting
        description               = 'Line item 2'
        currency_code             = me->us_dollar
        amount                    = 100
      ).
    expected_expense              = expected_expense + 80.
    " Add expense item
    me->expense_manager->add_expense_item(
      exporting
        description               = 'Line item 3'
        currency_code             = me->us_dollar
        amount                    = 100
      ).
    expected_expense              = expected_expense + 40.
    " Actual method call
    actual_expense                = me->expense_manager->calculate_total_expense( currency_code = me->euro ).
    " Assertion
    cl_abap_unit_assert=>assert_equals(
      exp                         = expected_expense
      act                         = actual_expense + 1 " ABAP Unit failure
*      act                         = actual_expense     " ABAP Unit success
     ).
  endmethod.
  method verify_test_double_interaction.
    data         : expected_expense
                                  type i
                 , actual_expense
                                  type i
                 .
    " In this test, the test double itself will be invoked to
    " determine whether it was called using matching calling
    " parameters as many times as we expected it should have
    " been called.

    " Add three expenses. Notice the currency_code
    " specified for each of these calls:
    me->expense_manager->add_expense_item(
      exporting
        description               = 'line item 1'
        currency_code             = me->indian_rupee
        amount                    = 100
      ).
    me->expense_manager->add_expense_item(
      exporting
        description               = 'line item 2'
        currency_code             = me->us_dollar
        amount                    = 100
      ).
    expected_expense              = expected_expense + 80.
    me->expense_manager->add_expense_item(
      exporting
        description               = 'line item 3'
        currency_code             = me->us_dollar
        amount                    = 100
      ).
    expected_expense              = expected_expense + 80.
    " ABAP Unit failure due to the active ABAP statements from here ...
    me->expense_manager->add_expense_item(
      exporting
        description               = 'line item 4'
        currency_code             = me->us_dollar
        amount                    = 100
      ).
    expected_expense              = expected_expense + 80.
    " ... to here. ABAP Unit failure
    " Configuration of expected interactions.  We expect there
    " will be 2 calls made where the test double will return the
    " value 80 when called with amount 100, source currency US dollar
    " and target currency Euro:
    cl_abap_testdouble=>configure_call( me->currency_converter_double
      )->returning( 80
      )->and_expect(
      )->is_called_times( 2
      ).
    me->currency_converter_double->convert(
      exporting
        amount                    = 100
        source_currency           = me->us_dollar
        target_currency           = me->euro
      ).
    " Actual method call
    actual_expense                = me->expense_manager->calculate_total_expense( currency_code = me->euro ).
    " Assertion
    cl_abap_unit_assert=>assert_equals(
      exp                         = expected_expense
      act                         = actual_expense + 1 " ABAP Unit failure
*      act                         = actual_expense     " ABAP Unit success
      ).
    " Verify interactions on testdouble
    cl_abap_testdouble=>verify_expectations( me->currency_converter_double ).
  endmethod.

  method custom_matcher.
    constants    : fixed_returning_expense
                                  type i         value 80
                 .
    data         : expected_expense
                                  type i
                 , actual_expense
                                  type i
                 , matcher        type ref to custom_matcher
                 .
    " Instantiate matcher object
    create object matcher.
    " Configure test double to use matcher object
    cl_abap_testdouble=>configure_call( me->currency_converter_double
      )->returning( fixed_returning_expense
      )->set_matcher( matcher
      ).
    me->currency_converter_double->convert(
      exporting
        amount                    = 100
        source_currency           = 'usd*' " currency pattern for US dollar
        target_currency           = me->euro
      ).
    " Add expenses with pattern
    expense_manager->add_expense_item(
      exporting
        description               = 'line item 1'
        currency_code             = 'usdollar' " will conform to expected currency pattern
        amount                    = 100
      ).
    expected_expense              = expected_expense + fixed_returning_expense.
    expense_manager->add_expense_item(
      exporting
        description               = 'line item 2'
        currency_code             = 'usdlr' " will conform to expected currency pattern
        amount                    = 100
      ).
    expected_expense              = expected_expense + fixed_returning_expense.
    expense_manager->add_expense_item(
      exporting
        description               = 'line item 3'
        currency_code             = 'dollar' " will not conform to expected currency pattern
        amount                    = 100
      ).
    expected_expense              = expected_expense + fixed_returning_expense. " ABAP Unit failure
*    expected_expense              = expected_expense + 00.                      " ABAP Unit success
    " Actual method call
    actual_expense                = me->expense_manager->calculate_total_expense( currency_code = me->euro ).
    " Assertion
    cl_abap_unit_assert=>assert_equals(
      exp                         = expected_expense
      act                         = actual_expense
      ).
  endmethod.

  method custom_answer.
    data         : expected_expense
                                  type i
                 , actual_expense
                                  type i
                 , answer            type ref to custom_answer
                 .

    " Instantiate answer object
    create object answer.
    " Configure test double to use answer object
    cl_abap_testdouble=>configure_call( me->currency_converter_double
      )->ignore_parameter( 'amount'
      )->set_answer( answer
      ).
    me->currency_converter_double->convert(
      exporting
        amount                    =  0
        source_currency           = me->indian_rupee
        target_currency           = me->euro
      ).
    " Add the expense line items
    expense_manager->add_expense_item(
      exporting
        description               = 'line item 1'
        currency_code             = me->indian_rupee
        amount                    = 80
      ).
    expense_manager->add_expense_item(
      exporting
        description               = 'line item 2'
        currency_code             = me->indian_rupee
        amount                    = 240
      ).
    expense_manager->add_expense_item(
      exporting
        description               = 'line item 3'
        currency_code             = me->indian_rupee
        amount                    = 800
      ).
    expense_manager->add_expense_item(
      exporting
        description               = 'line item 4'
        currency_code             = me->indian_rupee
        amount                    = 880
      ).
    " Actual method call
    actual_expense                = me->expense_manager->calculate_total_expense( currency_code = me->euro ).
    " Assertion
    expected_expense              = 25. " ( 80 + 240 + 800 + 880 ) / 80 = 25
    cl_abap_unit_assert=>assert_equals(
      exp                         = expected_expense
      act                         = actual_expense + 1 " ABAP Unit failure
*      act                         = actual_expense     " ABAP Unit success
      ).
  endmethod.
endclass.
