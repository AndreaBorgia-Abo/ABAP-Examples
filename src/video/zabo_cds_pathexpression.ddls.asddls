/*
Author: Rishi
Source: https://www.youtube.com/watch?v=0eXR-2mgEVs&t=9635s
*/
@AbapCatalog.sqlViewName: 'ZABO_PATHEXPR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Demo on path expression with filter'
define view ZABO_CDS_PATHEXPRESSION
  as select from snwd_pd
  association [0..*] to snwd_texts as _ProductText on $projection.NameGuid = _ProductText.parent_key
{

  key snwd_pd.node_key                  as NodeKey,
      snwd_pd.product_id                as ProductId,
      snwd_pd.type_code                 as TypeCode,
      snwd_pd.category                  as Category,
      snwd_pd.created_by                as CreatedBy,
      snwd_pd.name_guid                 as NameGuid,
      snwd_pd.desc_guid                 as DescGuid,
      snwd_pd.supplier_guid             as SupplierGuid,
      _ProductText[1:language = $session.system_language].text as ProductName,
// MEMO: we need to export the alias in order to use it in view ZABO_ASSO_DATASOURCE
      _ProductText
} 
// MEMO: "Value-set associations are not allowed here"
// where _ProductText.language = $session.system_language
