*&---------------------------------------------------------------------*
*& Include          LZLSP012_TABUPDD01
*&---------------------------------------------------------------------*

CLASS lcl_rtti_util DEFINITION.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_tabname TYPE tabname.
    METHODS create_tab_n_line
      EXPORTING et_ref TYPE REF TO data
                es_ref TYPE REF TO data
      RAISING   zcx_lsp012_error.


  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mv_tabname TYPE tabname.

    METHODS get_tab_n_line_type
      EXPORTING eo_line_type TYPE REF TO cl_abap_structdescr
                eo_tab_type  TYPE REF TO cl_abap_tabledescr
      RAISING   zcx_lsp012_error.


ENDCLASS.
