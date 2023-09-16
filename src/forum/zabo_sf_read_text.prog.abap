*&---------------------------------------------------------------------*
*& Report ZABO_SF_READ_TEXT
*&---------------------------------------------------------------------*
*& Author: unknown
*& Source: https://answers.sap.com/comments/7911991/view.html
*&---------------------------------------------------------------------*
REPORT zabo_sf_read_text.


DATA: lr_form TYPE REF TO cl_ssf_fb_smart_form.
DATA: lr_node TYPE REF TO cl_ssf_fb_node.
DATA: lr_text TYPE REF TO cl_ssf_fb_text_item.

DATA: ls_varheader TYPE ssfvarhdr.
DATA: ls_text TYPE tline.


START-OF-SELECTION.

  CREATE OBJECT lr_form.

  TRY.
      CALL METHOD lr_form->load
        EXPORTING
          im_formname = 'SF_ADRS_FOOTER'.
    CATCH cx_ssf_fb .
  ENDTRY.

  IF lr_form IS BOUND.
    READ TABLE lr_form->varheader INTO ls_varheader INDEX 1.
    IF sy-subrc = 0.
      lr_node ?= ls_varheader-pagetree.
      IF lr_node IS BOUND.

        lr_text ?= lr_node->obj.

        IF lr_text IS NOT INITIAL.

          LOOP AT lr_text->text INTO ls_text.
            WRITE:/ ls_text-tdline.
          ENDLOOP.

        ENDIF.

      ENDIF.
    ENDIF.
  ENDIF.
