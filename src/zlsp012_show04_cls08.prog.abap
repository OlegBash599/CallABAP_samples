*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW04_CLS08
*&---------------------------------------------------------------------*


INTERFACE lif_component.
  TYPES: BEGIN OF ts_id_val
          , area TYPE string
          , id TYPE text20
          , val TYPE text255
         , END OF ts_id_val
         , tt_id_val TYPE STANDARD TABLE OF ts_id_val
          WITH DEFAULT KEY
         .

  TYPES: tt_comps TYPE STANDARD TABLE OF REF TO lif_component
           WITH DEFAULT KEY
        .


  DATA ms_id_val TYPE ts_id_val.

  METHODS add
    IMPORTING io_comp TYPE REF TO lif_component.

  METHODS remove
    IMPORTING io_comp TYPE REF TO lif_component.

  METHODS get_child
    IMPORTING iv_num TYPE syindex
    EXPORTING eo_comp TYPE REF TO lif_component.

  METHODS get_id_val
    EXPORTING et_id_val TYPE tt_id_val.

ENDINTERFACE.                    "lif_component


*----------------------------------------------------------------------*
*       CLASS lcl_id_val_leaf DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_id_val_leaf DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_component.
    METHODS constructor
      IMPORTING is_id_val TYPE lif_component~ts_id_val.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.                    "lif_component DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_id_val_comp DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_id_val_comp DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_component.
    METHODS constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_comps TYPE lif_component~tt_comps.
ENDCLASS.                    "lif_component DEFINITION

""""""""""""""""""""""""""""
*----------------------------------------------------------------------*
*       CLASS lcl_id_val_leaf IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_id_val_leaf IMPLEMENTATION.
  METHOD constructor.
    lif_component~ms_id_val = is_id_val.
  ENDMETHOD.                    "constructor
  METHOD lif_component~get_id_val.
    APPEND lif_component~ms_id_val TO et_id_val.
  ENDMETHOD.                    "lif_component~get_id_val

  METHOD lif_component~add.
  ENDMETHOD.                    "lif_component~add

  METHOD lif_component~remove.
  ENDMETHOD.                    "lif_component~remove

  METHOD lif_component~get_child.
  ENDMETHOD.                    "lif_component~get_child

ENDCLASS.                    "lcl_id_val_leaf IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_id_val_comp IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_id_val_comp IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.                    "constructor

  METHOD lif_component~get_id_val.
    DATA lo_comp TYPE REF TO lif_component.
    DATA lt_id_val TYPE lif_component~tt_id_val.

    LOOP AT me->mt_comps INTO lo_comp.
      lo_comp->get_id_val(
        IMPORTING
          et_id_val = lt_id_val
      ).
      APPEND LINES OF lt_id_val  TO et_id_val.
      CLEAR lt_id_val.
    ENDLOOP.

  ENDMETHOD.                    "lif_component~get_id_val

  METHOD lif_component~add.
    APPEND io_comp TO me->mt_comps.
  ENDMETHOD.                    "lif_component~add

  METHOD lif_component~remove.
    DELETE me->mt_comps WHERE table_line EQ io_comp.
  ENDMETHOD.                    "lif_component~remove

  METHOD lif_component~get_child.

    READ TABLE mt_comps INTO eo_comp INDEX iv_num.

  ENDMETHOD.                    "lif_component~get_child

ENDCLASS.                    "lcl_id_val_comp IMPLEMENTATION

"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
*----------------------------------------------------------------------*
*       CLASS lcl_composite DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_composite DEFINITION.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS main.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS run_step1.

    DATA mo_root TYPE REF TO lcl_id_val_comp.

ENDCLASS.                    "lcl_composite DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_composite IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_composite IMPLEMENTATION.

  METHOD constructor.
    CREATE OBJECT mo_root.
  ENDMETHOD.                    "constructor

  METHOD main.

    DATA  lt_id_val TYPE lif_component=>tt_id_val.

    me->run_step1( ).


    mo_root->lif_component~get_id_val(
      IMPORTING
        et_id_val = lt_id_val
    ).

    zcl_lsp010_html=>get_instance( )->add_tab_ch( it_tab = lt_id_val )->show( ).

  ENDMETHOD.                    "main

  METHOD run_step1.

    DATA lo_comp TYPE REF TO lcl_id_val_comp.
    DATA lo_comp_sub TYPE REF TO lcl_id_val_comp.
    DATA lo_leaf TYPE REF TO lcl_id_val_leaf.
    DATA ls_id_val TYPE lif_component=>ts_id_val.

    CREATE OBJECT lo_comp.

    DO 12 TIMES.
      IF ( sy-index MOD 4 ) EQ 0.

        ls_id_val-area = 'ZCOMPO_SUB'.
        ls_id_val-id = |ZCOMPO_SUB_{ sy-index } |.
        ls_id_val-val = |ZCOMPS_VAL_{ sy-index * 10 } |.

        CREATE OBJECT lo_leaf
          EXPORTING
            is_id_val = ls_id_val.

        CREATE OBJECT lo_comp_sub.
        DO 2 TIMES.
          lo_comp_sub->lif_component~add( io_comp = lo_leaf ).
          lo_comp->lif_component~add( io_comp = lo_comp_sub ).
        ENDDO.

      ELSE.

        ls_id_val-area = 'ZCOMPOSITE'.
        ls_id_val-id = |ZCOMPOSITE_{ sy-index } |.
        ls_id_val-val = |ZCOMP_VAL_{ sy-index * 10 } |.

        CREATE OBJECT lo_leaf
          EXPORTING
            is_id_val = ls_id_val.


        lo_comp->lif_component~add( io_comp = lo_leaf ).

      ENDIF.
    ENDDO.

    mo_root = lo_comp.

  ENDMETHOD.                    "run_step1

ENDCLASS.
