*&---------------------------------------------------------------------*
*& Include          LZLSP012_TABUPDP01
*&---------------------------------------------------------------------*

CLASS lcl_rtti_util IMPLEMENTATION.
  METHOD constructor.
    mv_tabname = iv_tabname.
  ENDMETHOD.

  METHOD create_tab_n_line.
*      EXPORTING et_ref TYPE REF TO data
*                es_ref TYPE REF TO data.


    DATA lo_line_type	TYPE REF TO cl_abap_structdescr.
    DATA lo_tab_type  TYPE REF TO cl_abap_tabledescr.

    DATA lt_tab_ref TYPE REF TO data.
    DATA ls_line_ref TYPE REF TO data.


    TRY.
        me->get_tab_n_line_type(
          IMPORTING
            eo_line_type =     lo_line_type             " Runtime Type Services
            eo_tab_type  =     lo_tab_type             " Runtime Type Services
        ).
        "      CATCH zcx_nm0002_templ. " Exceptions while template

        CREATE DATA lt_tab_ref TYPE HANDLE lo_tab_type.
        CREATE DATA ls_line_ref TYPE HANDLE lo_line_type.

        et_ref ?= lt_tab_ref.
        es_ref ?= ls_line_ref.

    ENDTRY.

  ENDMETHOD.

  METHOD get_tab_n_line_type.
*      EXPORTING eo_line_type TYPE REF TO cl_abap_structdescr
*                eo_tab_type  TYPE REF TO cl_abap_tabledescr.

    DATA lo_struct   TYPE REF TO cl_abap_structdescr.
    DATA lt_comp     TYPE cl_abap_structdescr=>component_table.

    DATA lv_msg TYPE string.
    DATA lo_typedescr TYPE REF TO cl_abap_typedescr.

    lo_struct ?= cl_abap_typedescr=>describe_by_name( mv_tabname ).
    lt_comp  = lo_struct->get_components( ).

    TRY .
        eo_line_type = cl_abap_structdescr=>create(
                             p_components = lt_comp ).
        eo_tab_type = cl_abap_tabledescr=>create(
                      p_line_type  = eo_line_type ).
      CATCH cx_sy_table_creation. " Exception when Creating a Table Type
        MESSAGE e011(zlsp012_msg) WITH mv_tabname INTO lv_msg.

        RAISE EXCEPTION TYPE zcx_lsp012_error.

    ENDTRY.
  ENDMETHOD.
ENDCLASS.
