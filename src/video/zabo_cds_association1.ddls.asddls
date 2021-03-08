/*
Author: Rishi
Source: https://youtu.be/0eXR-2mgEVs?t=7390
*/ 
@AbapCatalog.sqlViewName: 'ZABO_ASSO1'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Demo on association'
define view ZABO_CDS_ASSOCIATION1
  as select from snwd_so_i
  association[0..1] to snwd_pd as _Products on $projection.ProductGuid = _Products.node_key
{
  key node_key      as NodeKey,
      parent_key    as ParentKey,
      so_item_pos   as SoItemPos,
      product_guid  as ProductGuid,
      note_guid     as NoteGuid,
      currency_code as CurrencyCode,
      gross_amount  as GrossAmount,
      net_amount    as NetAmount,
      tax_amount    as TaxAmount,
      _Products.category as ProductsCategory,
      _Products.price as ProductsPrice
}
