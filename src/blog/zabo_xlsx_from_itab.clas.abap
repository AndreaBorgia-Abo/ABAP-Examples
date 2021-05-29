*&---------------------------------------------------------------------*
*& Author: Łukasz Pęgiel
*& Source: https://abapblog.com/articles/tricks/120-create-xlsx-file-from-internal-table-in-background-v2
*&---------------------------------------------------------------------*
CLASS zabo_xlsx_from_itab DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS: create_xlsx_from_itab
      IMPORTING
                it_fieldcat      TYPE lvc_t_fcat OPTIONAL
                it_sort          TYPE lvc_t_sort OPTIONAL
                it_filt          TYPE lvc_t_filt OPTIONAL
                is_layout        TYPE lvc_s_layo OPTIONAL
                it_hyperlinks    TYPE lvc_t_hype OPTIONAL
                VALUE(it_data)    TYPE STANDARD TABLE
      RETURNING VALUE(r_xstring) TYPE xstring.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zabo_xlsx_from_itab IMPLEMENTATION.
  METHOD create_xlsx_from_itab.
    DATA(lt_data) = REF #( it_data ).

    IF it_fieldcat IS INITIAL.
      FIELD-SYMBOLS: <tab> TYPE STANDARD TABLE.
      ASSIGN lt_data->* TO <tab>.
      TRY.
          cl_salv_table=>factory(
          EXPORTING
            list_display = abap_false
          IMPORTING
            r_salv_table = DATA(salv_table)
          CHANGING
            t_table      = <tab> ).

          DATA(lt_fcat) = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                                   r_columns      = salv_table->get_columns( )
                                   r_aggregations = salv_table->get_aggregations( ) ).
        CATCH cx_salv_msg.
          RETURN.
      ENDTRY.

    ELSE.
      lt_fcat = it_fieldcat.
    ENDIF.

    cl_salv_bs_lex=>export_from_result_data_table(
      EXPORTING
        is_format            = if_salv_bs_lex_format=>mc_format_xlsx
        ir_result_data_table =  cl_salv_ex_util=>factory_result_data_table(
                                                r_data                      = lt_data
                                                s_layout                    = is_layout
                                                t_fieldcatalog              = lt_fcat
                                                t_sort                      = it_sort
                                                t_filter                    = it_filt
                                                t_hyperlinks                = it_hyperlinks )
      IMPORTING
        er_result_file       = r_xstring ).

  ENDMETHOD.

ENDCLASS.
