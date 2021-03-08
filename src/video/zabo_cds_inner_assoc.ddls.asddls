/*
Author: Rishi
Source: https://youtu.be/0eXR-2mgEVs?t=8950
*/ 
@AbapCatalog.sqlViewName: 'ZABO_INNERASSO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Demo inner join for an association'
define view ZABO_CDS_INNER_ASSOC
  as select from snwd_so_i
  association[0..1] to snwd_pd as _Products on $projection.ProductGuid = _Products.node_key
{
  key node_key      as NodeKey,
      parent_key    as ParentKey,
      so_item_pos   as SoItemPos,
      product_guid  as ProductGuid,
      currency_code as CurrencyCode,
      gross_amount  as GrossAmount,
      net_amount    as NetAmount,
      _Products[inner].category,
      _Products[inner].price
}
