/*
Author: Rishi
Source: https://www.youtube.com/watch?v=0eXR-2mgEVs&t=5274s
*/
@AbapCatalog.sqlViewName: 'ZABO_CASE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Demo on case and nested case'
define view ZABO_CDS_CASE
  as select from snwd_so
{
  key so_id,
      created_at,
      created_by,
      case billing_status
      when 'P' then 'Paid'
      when ' ' then
      case delivery_status
      when 'D' then 'Delivered'
      when ' ' then 'Open'
      else delivery_status
      end
      else billing_status
      end as BillingStatus
}
