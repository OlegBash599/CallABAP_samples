*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW04_CLS11
*&---------------------------------------------------------------------*


CLASS lcl_cache_matnr DEFINITION.
  " данные храняется сделсть

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING iv_matnr     TYPE matnr
                iv_matnr_tx  TYPE text40
                iv_mat_gr    TYPE char4
                iv_mat_gr_tx TYPE text10
                iv_meins     TYPE meins
      .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_matnr TYPE matnr.
    DATA mv_matnr_tx TYPE text40.
    DATA mv_mat_gr TYPE char4.
    DATA mv_mat_gr_tx TYPE text10.
    DATA mv_meins TYPE meins.


ENDCLASS.                    "lcl_cache_matnr DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_cache_mat_factory DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_cache_mat_factory DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ts_mat_cache
              , matnr TYPE matnr
              , lo_obj TYPE REF TO lcl_cache_matnr
          , END OF ts_mat_cache
          , tt_mat_cache TYPE SORTED TABLE OF ts_mat_cache
            WITH UNIQUE KEY primary_key COMPONENTS
              matnr
          .

    METHODS constructor.

    " проверяется создавать ли новый объект
    METHODS get_obj
      IMPORTING
                iv_matnr      TYPE matnr
                iv_matnr_tx   TYPE text40
                iv_mat_gr     TYPE char4
                iv_mat_gr_tx  TYPE text10
                iv_meins      TYPE meins
      RETURNING VALUE(ro_obj) TYPE REF TO lcl_cache_matnr
      .

  PROTECTED SECTION.
    DATA mt_mat_cache TYPE tt_mat_cache.
  PRIVATE SECTION.

ENDCLASS.                    "lcl_cache_mat_factory DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_sales_list DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_sales_list DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ts_sd_info_before
          , vbeln TYPE vbeln
          , posnr TYPE posnr
          , klmeng TYPE menge_d
          , matnr TYPE matnr
          , matnr_tx TYPE text40
          , mat_gr TYPE char4
          , mat_gr_tx TYPE text10
          , meins TYPE meins
        , END OF ts_sd_info_before
        , tt_sd_info_before TYPE STANDARD TABLE OF
          ts_sd_info_before WITH DEFAULT KEY
        .


    TYPES: BEGIN OF ts_sd_info_after
          , vbeln TYPE vbeln
          , posnr TYPE posnr
          , klmeng TYPE menge_d
          , matnr TYPE REF TO lcl_cache_matnr
        , END OF ts_sd_info_after
        , tt_sd_info_after TYPE STANDARD TABLE OF
          ts_sd_info_after WITH DEFAULT KEY
        .

    METHODS constructor.
    METHODS add_item
      IMPORTING
        iv_vbeln     TYPE vbeln
        iv_posnr     TYPE posnr
        iv_klmeng    TYPE menge_d
        iv_matnr     TYPE matnr
        iv_matnr_tx  TYPE text40
        iv_mat_gr    TYPE char4
        iv_mat_gr_tx TYPE text10
        iv_meins     TYPE meins

      .

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_sd_info TYPE tt_sd_info_after.
    DATA mt_sd_info_bef TYPE tt_sd_info_before.
    DATA mo_mat_fac TYPE REF TO lcl_cache_mat_factory.

ENDCLASS.                    "lcl_sales_list DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_cache_matnr IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_cache_matnr IMPLEMENTATION.
  METHOD constructor.
*    importing iv_matnr type matnr
*              iv_matnr_tx type text40
*              iv_mat_gr type char4
*              iv_mat_gr_tx type text10
*              iv_meins type meins
*              .

    mv_matnr = iv_matnr.
    mv_matnr_tx = iv_matnr_tx.
    mv_mat_gr = iv_mat_gr.
    mv_mat_gr_tx = iv_mat_gr_tx.
    mv_meins = iv_meins.
  ENDMETHOD.                    "constructor
ENDCLASS.                    "lcl_cache_matnr IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_cache_mat_factory IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_cache_mat_factory IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.                    "constructor

  METHOD get_obj.
    DATA lo_obj_mat TYPE REF TO lcl_cache_matnr.
    FIELD-SYMBOLS <fs_mat_cache> TYPE ts_mat_cache.
    DATA ls_mat_cache TYPE ts_mat_cache.

    READ TABLE mt_mat_cache ASSIGNING <fs_mat_cache>
      WITH KEY matnr = iv_matnr.
    IF sy-subrc EQ 0.
      ro_obj = <fs_mat_cache>-lo_obj.
    ELSE.
      CREATE OBJECT lo_obj_mat
        EXPORTING
          iv_matnr     = iv_matnr
          iv_matnr_tx  = iv_matnr_tx
          iv_mat_gr    = iv_mat_gr
          iv_mat_gr_tx = iv_mat_gr_tx
          iv_meins     = iv_meins.

      ls_mat_cache-matnr = iv_matnr.
      ls_mat_cache-lo_obj = lo_obj_mat.
      INSERT ls_mat_cache INTO TABLE mt_mat_cache.
      ro_obj = lo_obj_mat.
    ENDIF.


  ENDMETHOD.                    "get_obj

ENDCLASS.                    "lcl_cache_mat_factory IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_cache_matnr IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_sales_list IMPLEMENTATION.
  METHOD constructor.
    CREATE OBJECT mo_mat_fac.
    CLEAR mt_sd_info.
  ENDMETHOD.                    "constructor

  METHOD add_item.
*      IMPORTING
*                iv_vbeln TYPE vbeln
*                iv_posnr TYPE posnr
*                iv_klmeng TYPE klmeng
*                iv_matnr TYPE matnr
*                iv_matnr_tx TYPE text40
*                iv_mat_gr TYPE char4
*                iv_mat_gr_tx TYPE text10
*                iv_meins TYPE meins
*                .
*
    DATA ls_sd_info_after TYPE ts_sd_info_after.




    ls_sd_info_after-vbeln = iv_vbeln.
    ls_sd_info_after-posnr = iv_posnr.
    ls_sd_info_after-klmeng = iv_klmeng.
    ls_sd_info_after-matnr = me->mo_mat_fac->get_obj(
        iv_matnr     = iv_matnr
        iv_matnr_tx  = iv_matnr_tx
        iv_mat_gr    = iv_mat_gr
        iv_mat_gr_tx = iv_mat_gr_tx
        iv_meins     = iv_meins
    ).
    APPEND ls_sd_info_after TO mt_sd_info.



    " без flyweight

    DATA ls_sd_info_bef TYPE ts_sd_info_before.


    ls_sd_info_bef-vbeln = iv_vbeln.
    ls_sd_info_bef-posnr = iv_posnr.
    ls_sd_info_bef-klmeng = iv_klmeng.
    ls_sd_info_bef-matnr = iv_matnr.
    ls_sd_info_bef-matnr_tx =   iv_matnr_tx .
    ls_sd_info_bef-mat_gr =   iv_mat_gr.
    ls_sd_info_bef-mat_gr_tx =   iv_mat_gr_tx .
    ls_sd_info_bef-meins = iv_meins.

    APPEND ls_sd_info_bef TO mt_sd_info_bef.



  ENDMETHOD.                    "add_item
ENDCLASS.                    "lcl_cache_matnr IMPLEMENTATION

"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
*----------------------------------------------------------------------*
*       CLASS lcl_flyweight DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_flyweight DEFINITION.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS main.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_sales_items TYPE REF TO lcl_sales_list.
    METHODS run_step1.


ENDCLASS.                    "lcl_flyweight DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_flyweight IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_flyweight IMPLEMENTATION.

  METHOD constructor.
    CREATE OBJECT mo_sales_items.
  ENDMETHOD.                    "constructor

  METHOD main.
    " применим там где повторяются записи
    DATA lv_vbeln TYPE vbeln.
    DATA lv_posnr TYPE posnr.
    DATA lv_klmeng TYPE menge_d.
    DATA lv_matnr TYPE matnr.
    DATA lv_matnr_tx TYPE text40.
    DATA lv_mat_gr TYPE char4.
    DATA lv_mat_gr_tx TYPE text10.
    DATA lv_meins TYPE meins.

    DO 100 TIMES.

      lv_vbeln = '1230033333'.
      lv_posnr = ( sy-index * 10 ).

      lv_klmeng = sy-index * 2.

      IF ( sy-index MOD 2 ) = 0.
        lv_matnr = '34567831'.
        lv_matnr_tx = 'test 31'.
        lv_mat_gr = '1200'.
        lv_mat_gr_tx = 'MatGr for 1200 in KG'.
        lv_meins = 'KG'.
      ELSE.
        lv_matnr = '34567836'.
        lv_matnr_tx = 'test 36'.
        lv_mat_gr = '1500'.
        lv_mat_gr_tx = 'MatGr for 1500 in ST'.
        lv_meins = 'ST'.
      ENDIF.

      mo_sales_items->add_item(
    EXPORTING
      iv_vbeln     = lv_vbeln
      iv_posnr     = lv_posnr
      iv_klmeng    = lv_klmeng
      iv_matnr     = lv_matnr
      iv_matnr_tx  = lv_matnr_tx
      iv_mat_gr    = lv_mat_gr
      iv_mat_gr_tx = lv_mat_gr_tx
      iv_meins     = lv_meins
  ).

    ENDDO.


    BREAK-POINT.
    "look to memory analysis

  ENDMETHOD.                    "main

  METHOD run_step1.
  ENDMETHOD.                    "run_step1

ENDCLASS.
