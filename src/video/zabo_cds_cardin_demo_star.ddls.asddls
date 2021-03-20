/*
Author: Rishi
Source: https://www.youtube.com/watch?v=0eXR-2mgEVs&t=11340s
Variant: cardinality 1..*
*/
@AbapCatalog.sqlViewName: 'ZABO_CARD_DEMOST'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Cardinality demo, impact on results'
define view ZABO_CDS_CARDIN_DEMO_STAR
  as select from snwd_so
  association[1..*] to snwd_so_i as _Items on $projection.NodeKey = _Items.parent_key
{
      client           as Client,
  key node_key         as NodeKey,
      so_id            as SoId,
      net_amount       as NetAmount,
      currency_code    as CurrencyCode,
      _Items.so_item_pos,
      _Items
}
