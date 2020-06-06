*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW04_CLS07
*&---------------------------------------------------------------------*

INTERFACE lif_abstraction.

  DATA mv_area_id TYPE string.

  METHODS v
    IMPORTING iv_line TYPE string
    EXPORTING ev_val TYPE any.

  METHODS show.
  METHODS set_area
    IMPORTING iv_area TYPE string.

ENDINTERFACE.                    "lif_abstraction

*----------------------------------------------------------------------*
*       INTERFACE lif_area
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
INTERFACE lif_area.

  DATA mv_area_id TYPE string.

  METHODS v
    IMPORTING iv_line TYPE string
    EXPORTING ev_val TYPE any.

  METHODS show.


ENDINTERFACE.                    "lif_area

*----------------------------------------------------------------------*
*       CLASS lcl_br_abstraction DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_br_abstraction DEFINITION.
  PUBLIC SECTION.

    INTERFACES lif_abstraction.
    ALIASES set_area FOR lif_abstraction~set_area.
    ALIASES v FOR lif_abstraction~v.
    ALIASES show FOR lif_abstraction~show.


    METHODS constructor
      IMPORTING iv_area TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_area_deal TYPE REF TO lif_area.
ENDCLASS.                    "lcl_br_abstraction DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_simple_area DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_simple_area DEFINITION.

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_gen_area_line
             , id TYPE text20
             , id_pos TYPE numc5
             , value TYPE text
           , END OF ts_gen_area_line
           , tt_gen_area_line TYPE STANDARD TABLE OF ts_gen_area_line
               WITH DEFAULT KEY
           .
    INTERFACES lif_area.

    ALIASES v FOR lif_area~v.
    ALIASES show FOR lif_area~show.

    METHODS constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_gen_area TYPE tt_gen_area_line.

    METHODS set_area_id.


ENDCLASS.                    "lif_area DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_zsd1_area DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_zsd1_area DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ts_zsd1_area_line
             , vkorg TYPE vkorg
             , vtweg TYPE vtweg
             , id TYPE text20
             , id_pos TYPE numc5
             , value TYPE text
           , END OF ts_zsd1_area_line
           , tt_zsd1_area_line TYPE STANDARD TABLE OF ts_zsd1_area_line
               WITH DEFAULT KEY
           .
    INTERFACES lif_area.

    ALIASES v FOR lif_area~v.
    ALIASES show FOR lif_area~show.

    METHODS constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_zsd1_area TYPE tt_zsd1_area_line.

    DATA mv_key1_vkorg TYPE vkorg.
    DATA mv_key2_vtweg TYPE vtweg.
    DATA mv_key3_id TYPE text20.

    METHODS set_area_id.

ENDCLASS.                    "lif_area DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_br_abstraction IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_br_abstraction IMPLEMENTATION.

  METHOD constructor.
    me->set_area( iv_area = iv_area ).
  ENDMETHOD.                    "constructor

  METHOD set_area.
    DATA lv_area TYPE string.

    IF me->lif_abstraction~mv_area_id EQ iv_area.
    ELSE.
      FREE mo_area_deal.
    ENDIF.

    CASE iv_area.
      WHEN 'ZSD1'.
        "   lv_area = 'LCL_ZSD1_AREA'.
        CREATE OBJECT mo_area_deal TYPE lcl_zsd1_area.
      WHEN OTHERS.
        "lv_area = 'LCL_SIMPLE_AREA'.
        CREATE OBJECT mo_area_deal TYPE lcl_simple_area.
    ENDCASE.

    TRANSLATE lv_area TO UPPER CASE.

    "CREATE OBJECT mo_area_deal TYPE (lv_area).
    me->lif_abstraction~mv_area_id = iv_area.
  ENDMETHOD.                    "set_area

  METHOD lif_abstraction~v.
    IF mo_area_deal IS BOUND.
      mo_area_deal->v(
        EXPORTING
          iv_line = iv_line
        IMPORTING
          ev_val  = ev_val
      )..
    ENDIF.
  ENDMETHOD.                    "lif_abstraction~v

  METHOD lif_abstraction~show.
    IF mo_area_deal IS BOUND.
      mo_area_deal->show( ).
    ENDIF.
  ENDMETHOD.                    "lif_abstraction~show

ENDCLASS.                    "lcl_br_abstraction IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_simple_area IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_simple_area IMPLEMENTATION.
  METHOD constructor.
    DATA ls_gen_area_line TYPE ts_gen_area_line.

    ls_gen_area_line-id = 'ZSWITCH_NEW_FORMS'.
    ls_gen_area_line-id_pos = 1.
    ls_gen_area_line-value = abap_false.
    APPEND ls_gen_area_line TO  mt_gen_area.

    DO 5 TIMES.
      ls_gen_area_line-id = |'ZPAR_' { sy-index }|.
      ls_gen_area_line-id_pos = sy-index.
      ls_gen_area_line-value = |'VAL_' { ( sy-index ) * 25 }|.
      APPEND ls_gen_area_line TO  mt_gen_area.
    ENDDO.


    ls_gen_area_line-id = 'ZVAT20_SWITCH'.
    ls_gen_area_line-id_pos = 1.
    ls_gen_area_line-value = abap_true.
    APPEND ls_gen_area_line TO  mt_gen_area.

    DO 5 TIMES.
      ls_gen_area_line-id = |'ZPAR_' { sy-index }|.
      ls_gen_area_line-id_pos = sy-index.
      ls_gen_area_line-value = |'VAL_' { ( sy-index ) * 17 }|.
      APPEND ls_gen_area_line TO  mt_gen_area.
    ENDDO.


    set_area_id( ).
  ENDMETHOD.                    "constructor
  METHOD set_area_id.
    me->lif_area~mv_area_id = 'ZSIMPLE$'.
  ENDMETHOD.                    "set_area_id
  METHOD lif_area~show.
"    zcl_lsp010_html=>get_instance( )->add_tab_ch( it_tab = mt_gen_area )->show( ).
    zcl_lsp012_html=>get_instance( )->add_tab_ch( it_tab = mt_gen_area )->show( ).
  ENDMETHOD.                    "lif_area~show

  METHOD lif_area~v.
    DATA lv_line TYPE string.
    DATA lt_str TYPE STANDARD TABLE OF string.
    FIELD-SYMBOLS <fs_str> TYPE string.
    FIELD-SYMBOLS <fs_gen_area> TYPE ts_gen_area_line.

    lv_line = iv_line.

    REPLACE ALL OCCURRENCES OF me->lif_area~mv_area_id IN lv_line WITH ''.



    READ TABLE mt_gen_area ASSIGNING <fs_gen_area>
      WITH KEY id  = lv_line.
    IF sy-subrc EQ 0.
      ev_val = <fs_gen_area>-value.
    ELSE.
      CLEAR ev_val.
    ENDIF.
  ENDMETHOD.                    "lif_area~v

ENDCLASS.                    "lcl_simple_area IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_zsd1_area IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_zsd1_area IMPLEMENTATION.
  METHOD constructor.
    DATA ls_zsd1_area_line TYPE ts_zsd1_area_line.

    ls_zsd1_area_line-vkorg = '1000'.
    ls_zsd1_area_line-vtweg = '01'.
    ls_zsd1_area_line-id = 'ZX5RET_INTEGRATION'.
    ls_zsd1_area_line-id_pos = 1.
    ls_zsd1_area_line-value = abap_false.
    APPEND ls_zsd1_area_line TO  mt_zsd1_area.

    DO 5 TIMES.
      ls_zsd1_area_line-id = |'ZSDPAR_' { sy-index }|.
      ls_zsd1_area_line-id_pos = sy-index.
      ls_zsd1_area_line-value = |'VAL_' { ( sy-index ) * 25 }|.
      APPEND ls_zsd1_area_line TO  mt_zsd1_area.
    ENDDO.


    ls_zsd1_area_line-vkorg = '2000'.
    ls_zsd1_area_line-vtweg = '01'.
    ls_zsd1_area_line-id = 'ZX5RET_INTEGRATION'.
    ls_zsd1_area_line-id_pos = 1.
    ls_zsd1_area_line-value = abap_true.
    APPEND ls_zsd1_area_line TO  mt_zsd1_area.

    DO 5 TIMES.
      ls_zsd1_area_line-id = |'ZSDPAR_' { sy-index }|.
      ls_zsd1_area_line-id_pos = sy-index.
      ls_zsd1_area_line-value = |'VAL_' { ( sy-index ) * 17 }|.
      APPEND ls_zsd1_area_line TO  mt_zsd1_area.
    ENDDO.


    set_area_id( ).


  ENDMETHOD.                    "constructor

  METHOD set_area_id.
    me->lif_area~mv_area_id = 'ZSD1$'.
  ENDMETHOD.                    "set_area_id

  METHOD lif_area~show.
    DATA lo_alv TYPE REF TO cl_salv_table.
    DATA lx_salv_msg TYPE REF TO cx_salv_msg.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = mt_zsd1_area ).
      CATCH cx_salv_msg INTO lx_salv_msg.
    ENDTRY.

    lo_alv->display( ).
  ENDMETHOD.                    "lif_area~show

  METHOD lif_area~v.
    DATA lv_line TYPE string.
    DATA lt_str TYPE STANDARD TABLE OF string.
    FIELD-SYMBOLS <fs_str> TYPE string.
    FIELD-SYMBOLS <fs_zsd1_area> TYPE ts_zsd1_area_line.

    lv_line = iv_line.

    REPLACE ALL OCCURRENCES OF me->lif_area~mv_area_id IN lv_line WITH ''.

    SPLIT lv_line AT '$' INTO TABLE lt_str.

    CLEAR mv_key1_vkorg.
    CLEAR mv_key2_vtweg.
    CLEAR mv_key3_id.

    LOOP AT lt_str ASSIGNING <fs_str>.
      CASE sy-tabix.
        WHEN 1.
          mv_key1_vkorg = <fs_str>.
        WHEN 2.
          mv_key2_vtweg = <fs_str>.
        WHEN 3.
          mv_key3_id = <fs_str>.
        WHEN OTHERS.
          EXIT.
      ENDCASE.
    ENDLOOP.

    READ TABLE mt_zsd1_area ASSIGNING <fs_zsd1_area>
      WITH KEY vkorg = mv_key1_vkorg
               vtweg = mv_key2_vtweg
               id  = mv_key3_id.
    IF sy-subrc EQ 0.
      ev_val = <fs_zsd1_area>-value.
    ELSE.
      CLEAR ev_val.
    ENDIF.

  ENDMETHOD.                    "lif_area~v
ENDCLASS.                    "lcl_zsd1_area IMPLEMENTATION

"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
*----------------------------------------------------------------------*
*       CLASS lcl_bridge DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_bridge DEFINITION.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS main.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_area_string TYPE string.
    METHODS run_step1.
    METHODS run_step2.


ENDCLASS.                    "lcl_bridge DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_bridge IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_bridge IMPLEMENTATION.

  METHOD constructor.

  ENDMETHOD.                    "constructor

  METHOD main.
    mv_area_string = 'ZSD1'.
    run_step1( ).

    CLEAR mv_area_string.
    run_step2( ).
  ENDMETHOD.                    "main

  METHOD run_step1.
    DATA lo_c TYPE REF TO lcl_br_abstraction.
    DATA lv_vkorg TYPE vkorg VALUE '2000'.
    DATA lv_vtweg TYPE vtweg VALUE '01'.
    "data lv_id TYPE text20 value 'ZLEN'.

    DATA lv_is_active TYPE char1.

    CREATE OBJECT lo_c
      EXPORTING
        iv_area = mv_area_string.

    lo_c->v( EXPORTING iv_line = |{ lv_vkorg }${ lv_vtweg }$ZX5RET_INTEGRATION |
             IMPORTING ev_val = lv_is_active ).

    IF lv_is_active EQ abap_true.
      MESSAGE i004 WITH 'ZX5RET_INTEGRATION' ' Интеграция запущена'.
    ELSE.
      MESSAGE i004 WITH 'ZX5RET_INTEGRATION' ' Интеграция отключена'.
    ENDIF.

    lo_c->show( ).

  ENDMETHOD.                    "run_step1


  METHOD run_step2.
    DATA lo_c TYPE REF TO lcl_br_abstraction.
    DATA lv_is_active TYPE char1.

    CREATE OBJECT lo_c
      EXPORTING
        iv_area = mv_area_string.

    lo_c->v( EXPORTING iv_line = |ZSWITCH_NEW_FORMS|
             IMPORTING ev_val = lv_is_active ).

    IF lv_is_active EQ abap_true.
      MESSAGE i004 WITH 'ZSWITCH_NEW_FORMS' 'Новые формы запущены'.
    ELSE.
      MESSAGE i004 WITH 'ZSWITCH_NEW_FORMS' 'Новые формы отключены'.
    ENDIF.

    lo_c->show( ).
  ENDMETHOD.                    "run_step2
ENDCLASS.
