/*
Author: Andrea Borgia
Source: https://www.youtube.com/watch?v=0eXR-2mgEVs&t=4569s
*/
@AbapCatalog.sqlViewName: 'ZABO_SESSIONVAR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@ClientHandling.type: #INHERITED
@ClientHandling.algorithm: #SESSION_VARIABLE
@EndUserText.label: 'Demo on session variables'
define view ZABO_CDS_SESSIONVAR as select from sflight {
    key carrid as CarrierID,
    key connid as ConnectionID,
    key fldate as FlightDate,
    planetype as PlaneType,
    $session.client as CurrentClient,
    $session.system_date as SystemDate,
    $session.system_language as SystemLanguage,
    $session.user as LoggedInUser
}
