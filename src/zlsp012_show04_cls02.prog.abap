*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW04_CLS02
*&---------------------------------------------------------------------*

CLASS lcl_base_vals DEFINITION ABSTRACT.

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_tab_id_val
              , id TYPE string
              , val TYPE string
           , END OF ts_tab_id_val
           , tt_tab_id_val TYPE STANDARD TABLE OF
            ts_tab_id_val WITH DEFAULT KEY
           .

    CLASS-METHODS get_obj_vals " factory
      IMPORTING iv_line       TYPE string
      RETURNING VALUE(ro_obj) TYPE REF TO lcl_base_vals.

    METHODS constructor
      IMPORTING iv_line TYPE string.
    METHODS set_id_val
      IMPORTING iv_id  TYPE c
                iv_val TYPE c.

    METHODS get_tab
      RETURNING VALUE(rt_tab) TYPE tt_tab_id_val.

  PROTECTED SECTION.

    DATA mv_line TYPE string.
    DATA mt_tab TYPE tt_tab_id_val.

  PRIVATE SECTION.


ENDCLASS.                    "lcl_base_vals DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_sales_key_vals DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_sales_key_vals DEFINITION INHERITING FROM lcl_base_vals.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_line TYPE string.
  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mv_vkorg TYPE vkorg.
    DATA mv_vtweg TYPE vtweg.
    DATA mv_spart TYPE spart.

    METHODS fill_tab_sd_ids.

ENDCLASS.                    "lcl_sales_key_vals DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_fi_key_vals DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_fi_key_vals DEFINITION INHERITING FROM lcl_base_vals.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_line TYPE string.
  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mv_bukrs TYPE bukrs.

    METHODS fill_tab_fi_ids.

ENDCLASS.                    "lcl_fi_key_vals DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_mvmnt_key_vals DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_mvmnt_key_vals DEFINITION INHERITING FROM lcl_base_vals.


  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_line TYPE string.
  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mv_plant TYPE werks_d.
    DATA mv_move_type TYPE bwart.

    METHODS fill_tab_mm_ids.

ENDCLASS.                    "lcl_mvmnt_key_vals DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_null_key_vals DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_null_key_vals DEFINITION INHERITING FROM lcl_base_vals.

ENDCLASS.                    "lcl_null_key_vals DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_base_vals IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_base_vals IMPLEMENTATION.
  METHOD constructor.
    mv_line = iv_line.
  ENDMETHOD.                    "constructor
  METHOD set_id_val.
    DATA ls_id_val TYPE ts_tab_id_val.

    ls_id_val-id = iv_id.
    ls_id_val-val = iv_val.
    APPEND ls_id_val TO mt_tab.
  ENDMETHOD.                    "set_id_val

  METHOD get_tab.
    rt_tab[] = mt_tab[].
  ENDMETHOD.                    "get_tab

  METHOD get_obj_vals.
    DATA lt_str TYPE STANDARD TABLE OF string.
    DATA lv_str_area TYPE string.
    DATA lv_str_key TYPE string.

    DATA lv_class_name TYPE seoclsname.

    SPLIT iv_line AT '$' INTO lv_str_area lv_str_key.

    CASE lv_str_area.
      WHEN 'ZSD'.
        lv_class_name = 'lcl_sales_key_vals'.
        "create OBJECT ro_obj TYPE lcl_sales_key_vals.
      WHEN 'ZFI'.
        lv_class_name = 'lcl_fi_key_vals'.
      WHEN 'ZMM'.
        lv_class_name = 'lcl_mvmnt_key_vals'.

      WHEN 'ZPP'.
        lv_class_name = 'lcl_pp_key_vals'.

      WHEN OTHERS.
        lv_class_name = 'lcl_null_key_vals'.
    ENDCASE.

    TRANSLATE lv_class_name TO UPPER CASE.
    TRY .
        CREATE OBJECT ro_obj TYPE (lv_class_name)
          EXPORTING
            iv_line = lv_str_key.
      CATCH cx_root.

    ENDTRY.


  ENDMETHOD.                    "get_obj_vals

ENDCLASS.                    "lcl_base_vals IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_sales_key_vals IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_sales_key_vals IMPLEMENTATION.
  METHOD constructor.

    super->constructor( iv_line = iv_line ).

    SPLIT iv_line AT '$' INTO mv_vkorg mv_vtweg mv_spart.

    fill_tab_sd_ids( ).

  ENDMETHOD.                    "constructor

  METHOD fill_tab_sd_ids.
    DATA ls_id_val TYPE ts_tab_id_val.
    DATA lt_id_val TYPE tt_tab_id_val.

    ls_id_val-id = 'KEY_VKORG'.
    ls_id_val-val = me->mv_vkorg.
    APPEND ls_id_val TO lt_id_val.

    ls_id_val-id = 'KEY_VTWEG'.
    ls_id_val-val = me->mv_vtweg.
    APPEND ls_id_val TO lt_id_val.

    ls_id_val-id = 'KEY_SPART'.
    ls_id_val-val = me->mv_spart.
    APPEND ls_id_val TO lt_id_val.


    ls_id_val-id = 'ZZ_VAL1'.
    ls_id_val-val = 'SD1'.
    APPEND ls_id_val TO lt_id_val.

    ls_id_val-id = 'ZZ_VAL2'.
    ls_id_val-val = 'SD2'.
    APPEND ls_id_val TO lt_id_val.

    mt_tab[] = lt_id_val[].

  ENDMETHOD.                    "fill_tab_sd_ids

ENDCLASS.                    "lcl_sales_key_vals IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_fi_key_vals IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_fi_key_vals IMPLEMENTATION.

  METHOD constructor.
    DATA lv_str TYPE string.

    super->constructor( iv_line = iv_line ).

    SPLIT iv_line AT '$' INTO mv_bukrs lv_str.

    fill_tab_fi_ids( ).

  ENDMETHOD.                    "constructor

  METHOD fill_tab_fi_ids.
    DATA ls_id_val TYPE ts_tab_id_val.

    ls_id_val-id = 'KEY_BUKRS'.
    ls_id_val-val = me->mv_bukrs.
    APPEND ls_id_val TO mt_tab.

    ls_id_val-id = 'ZZ_VAL1'.
    ls_id_val-val = 'FI1'.
    APPEND ls_id_val TO mt_tab.

    ls_id_val-id = 'ZZ_VAL2'.
    ls_id_val-val = 'FI2'.
    APPEND ls_id_val TO mt_tab.

  ENDMETHOD.                    "fill_tab_fi_ids

ENDCLASS.                    "lcl_fi_key_vals IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_mvmnt_key_vals IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_mvmnt_key_vals IMPLEMENTATION.

  METHOD constructor.
    super->constructor( iv_line = iv_line ).

    SPLIT iv_line AT '$' INTO mv_plant mv_move_type.

    fill_tab_mm_ids( ).

  ENDMETHOD.                    "constructor

  METHOD fill_tab_mm_ids.
    DATA ls_id_val TYPE ts_tab_id_val.

    ls_id_val-id = 'KEY_PLANT'.
    ls_id_val-val = me->mv_plant.
    APPEND ls_id_val TO mt_tab.

    ls_id_val-id = 'KEY_MOVEMENT_TYPE'.
    ls_id_val-val = me->mv_move_type.
    APPEND ls_id_val TO mt_tab.

    ls_id_val-id = 'ZZ_VAL1'.
    ls_id_val-val = 'MM_PLANT1'.
    APPEND ls_id_val TO mt_tab.

    ls_id_val-id = 'ZZ_VAL2'.
    ls_id_val-val = 'MM_PLANT2'.
    APPEND ls_id_val TO mt_tab.

  ENDMETHOD.                    "fill_tab_fi_ids

ENDCLASS.                    "lcl_mvmnt_key_vals IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_factory_meth DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_factory_meth DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS main.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS run_step1.
    METHODS run_step2.
    METHODS run_step3.
ENDCLASS.                    "lcl_factory_meth DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_factory_meth IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_factory_meth IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.                    "constructor

  METHOD main.

    me->run_step1( ).
    me->run_step2( ).
    me->run_step3( ).

  ENDMETHOD.                    "main

  METHOD run_step1.
    DATA lv_sdkey TYPE string VALUE 'ZSD$1000$10$10'. " key = $VKORG$VTWEG$VTWWW

    DATA lo_vals TYPE REF TO lcl_base_vals.
    lo_vals = lcl_base_vals=>get_obj_vals( iv_line = lv_sdkey ).

    lo_vals->set_id_val(
  EXPORTING
    iv_id  = 'ZID_STEP1_SD'
    iv_val = 'Value for STEP1_Sd'
).

    zcl_lsp012_html=>get_instance( )->add_tab_ch(
    it_tab = lo_vals->get_tab( ) )->show( ).


  ENDMETHOD.                    "run_step1

  METHOD run_step2.
    DATA lv_fikey TYPE string VALUE 'ZFI$1000'. " key = $BUKRS
    DATA lo_vals TYPE REF TO lcl_base_vals.
    lo_vals = lcl_base_vals=>get_obj_vals( iv_line = lv_fikey ).

    lo_vals->set_id_val(
      EXPORTING
        iv_id  = 'ZID_STEP2'
        iv_val = 'Value for STEP2'
    ).

    zcl_lsp010_html=>get_instance( )->add_tab_ch(
    it_tab = lo_vals->get_tab( ) )->show( ).
  ENDMETHOD.                    "run_step2

  METHOD run_step3.
    DATA lv_movkey TYPE string VALUE 'ZMM$1000$101'. " key = $PLANT$MOVEMENTTYPE
    DATA lo_vals TYPE REF TO lcl_base_vals.
    lo_vals = lcl_base_vals=>get_obj_vals( iv_line = lv_movkey ).

    lo_vals->set_id_val(
      EXPORTING
        iv_id  = 'ZID_STEP3_PLANT'
        iv_val = 'Value for STEP3_MM' ).

    zcl_lsp010_html=>get_instance( )->add_tab_ch(
    it_tab = lo_vals->get_tab( ) )->show( ).
  ENDMETHOD.                    "run_step3
ENDCLASS.
