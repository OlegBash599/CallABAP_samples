*&---------------------------------------------------------------------*
*& Include          LZLSP012_FUNCP01
*&---------------------------------------------------------------------*

CLASS lcl_number_oper IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.

  METHOD get_any_num0_1000.
*    METHODS get_any_num0_1000
*      RETURNING VALUE(rv_val) TYPE syindex.

    rv_val = cl_abap_random=>seed( ).
  ENDMETHOD.

ENDCLASS.
