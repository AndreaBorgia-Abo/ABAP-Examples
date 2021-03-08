/*
Author: Rishi
Source: https://www.youtube.com/watch?v=0eXR-2mgEVs&t=11040s
*/
@AbapCatalog.sqlViewName: 'ZABO_ASSOC_DS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Association as a datasource'
define view ZABO_ASSO_DATASOURCE
  as select from ZABO_CDS_PATHEXPRESSION._ProductText
{
  node_key as NodeKey,
  language as Language,
  text     as Text
} where language = $session.system_language
