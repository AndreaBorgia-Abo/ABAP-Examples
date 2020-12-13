*&---------------------------------------------------------------------*
*& Report ZABO_DEEP_STRUCTURE_APPEND
*&---------------------------------------------------------------------*
*& Author: Elias Kekakos / Michael Biber
*& Source: https://answers.sap.com/questions/13206680/problem-with-a-very-deep-structure.html
*&---------------------------------------------------------------------*
REPORT ZABO_DEEP_STRUCTURE_APPEND.

TYPES: BEGIN OF ty_header,
         version TYPE string,
       END OF ty_header.
TYPES: BEGIN OF ty_company_info,
         comptin      TYPE string,
         fydateend    TYPE string,
         bookcateg    TYPE numc2,
         acccodemaskg TYPE string,
         acccodemaska TYPE string,
         acccodemaskt TYPE string,
         chartcode    TYPE string,
         chartdescr   TYPE string,
       END OF ty_company_info.

TYPES: BEGIN OF ty_relations,
         acc     TYPE char14,
         reltype TYPE numc2,
       END OF ty_relations,
       tt_relations TYPE TABLE OF ty_relations WITH EMPTY KEY.

TYPES: BEGIN OF ty_accounts,
         acc                 TYPE string,
         descr               TYPE string,
         cat                 TYPE i,
         acckind             TYPE string,
         relations           TYPE tt_relations,
         vatcat              TYPE numc2,
         vatprc              TYPE p,
         vatexemptreason     TYPE string,
         busacttype          TYPE numc2,
         tradetype           TYPE numc2,
         offactcode          TYPE string,
         isdeduction         TYPE char1,
         deductionoffcodedet TYPE string,
         deductionoffcode    TYPE string,
         stampdutyoffcode    TYPE string,
         taxoffcode          TYPE string,
         extrataxoffcode     TYPE string,
         deductvattype       TYPE numc2,
       END OF ty_accounts,
       tt_accounts TYPE TABLE OF ty_accounts WITH EMPTY KEY.

TYPES: BEGIN OF ty_adata,
         companyinfo TYPE ty_company_info,
         accounts    TYPE tt_accounts,
       END OF ty_adata,
       tt_adata TYPE TABLE OF ty_adata WITH EMPTY KEY.


TYPES: BEGIN OF ty_accledg,
         header TYPE ty_header,
         data   TYPE tt_adata,
       END OF ty_accledg.
DATA: str_accledg         TYPE   ty_accledg.



str_accledg = VALUE #(
  Data = VALUE #( (    "table with one line
    Accounts = VALUE #( (  "table with one line
      acc = '01'
      descr = 'Elias test'
      cat = 11
    ) )
  ) )
).
