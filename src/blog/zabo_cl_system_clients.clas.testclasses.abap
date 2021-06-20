CLASS ltc_test DEFINITION FINAL
               FOR TESTING
               DURATION SHORT
               RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS get_client_000 FOR TESTING.
    METHODS get_client_001 FOR TESTING.
    METHODS get_client_999 FOR TESTING.

  PRIVATE SECTION.
    CLASS-DATA osql_test_environment TYPE REF TO if_osql_test_environment.
    CLASS-METHODS class_setup.
    CLASS-METHODS class_teardown.
ENDCLASS.


CLASS ltc_test IMPLEMENTATION.
  METHOD class_setup.
    osql_test_environment = cl_osql_test_environment=>create( VALUE #( ( 'T000' ) ) ).
    DATA(clients_test_data) = VALUE zabo_cl_system_clients=>clients( ( mandt = '000' mtext = 'Test double' )
                                                                 ( mandt = '001' mtext = 'Test double' )
                                                                 ( mandt = '999' mtext = 'Test double' ) ).
    osql_test_environment->insert_test_data( clients_test_data ).
  ENDMETHOD.

  METHOD class_teardown.
    osql_test_environment->destroy( ).
  ENDMETHOD.

  METHOD get_client_000.
    DATA(cut) = NEW zabo_cl_system_clients( ).
    DATA(result) = cut->get_client( '000' ).
    cl_aunit_assert=>assert_not_initial( result ).
  ENDMETHOD.

  METHOD get_client_001.
    DATA(cut) = NEW zabo_cl_system_clients( ).
    DATA(result) = cut->get_client( '001' ).
    cl_aunit_assert=>assert_not_initial( result ).
  ENDMETHOD.

  METHOD get_client_999.
    DATA(cut) = NEW zabo_cl_system_clients( ).
    DATA(result) = cut->get_client( '999' ).
    cl_aunit_assert=>assert_not_initial( result ).
  ENDMETHOD.
ENDCLASS.
