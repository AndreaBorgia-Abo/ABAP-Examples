/*
Author: Rishi
Source: https://www.youtube.com/watch?v=0eXR-2mgEVs&t=9635s
*/ 
@AbapCatalog.sqlViewName: 'ZABO_PATHDEMO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Path expression demo in association'
define view ZABO_CDS_ITEM_PRD_DETAILS
  as select from snwd_so
  association [1..*] to snwd_so_i as _Items           on $projection.NodeKey = _Items.parent_key
  association [1..1] to snwd_bpa  as _BusinessPartner on $projection.BuyerGuid = _BusinessPartner.node_key
{
      client       as Client,
  key node_key     as NodeKey,
      so_id        as SoId,
      created_by   as CreatedBy,
      buyer_guid   as BuyerGuid,
      gross_amount as GrossAmount,
      net_amount   as NetAmount,
      _Items,
      _BusinessPartner
}
