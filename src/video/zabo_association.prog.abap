*&---------------------------------------------------------------------*
*& Report ZABO_ASSOCIATION
*&---------------------------------------------------------------------*
*& Author: Rishi
*& Source: https://www.youtube.com/watch?v=0eXR-2mgEVs&t=11340s
*&---------------------------------------------------------------------*
*& [1..1] ZABO_CDS_CARDIN_DEMO1     ->
*& [1..*] ZABO_CDS_CARDIN_DEMO_STAR ->
REPORT zabo_association.

PARAMETERS: p_one  RADIOBUTTON GROUP rad1 DEFAULT 'X',
            p_star RADIOBUTTON GROUP rad1.

IF p_one = abap_true.
  SELECT soid,
         SUM( netamount ) AS netamount
    FROM zabo_cds_cardin_demo1
    INTO TABLE @DATA(lt_sales1)
    GROUP BY soid.
  cl_demo_output=>display_data( lt_sales1 ).
ELSEIF p_star = abap_true.
  SELECT soid,
         SUM( netamount ) AS netamount
    FROM zabo_cds_cardin_demo_star
    INTO TABLE @DATA(lt_sales_star)
    GROUP BY soid.
  cl_demo_output=>display_data( lt_sales_star ).
ENDIF.
