/*
Author: Andrea Borgia
Source: https://www.youtube.com/watch?v=0eXR-2mgEVs&t=3360s

Useful info:
* Example won't work in cloud because of direct table access, see https://answers.sap.com/answers/12916499/view.html
*/
@AbapCatalog.sqlViewName: 'ZABO_SYX_RULES'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Demo on syntax rules'
define view ZABO_CDS_SYX_RULES as select from sflight {
    key carrid as CarrierID,
    key connid as ConnectionID,
    key fldate as FlightDate,
    planetype as PlaneType
}
