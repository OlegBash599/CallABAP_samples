*&---------------------------------------------------------------------*
*& Include          LZLSP012_MEDIATORD03
*&---------------------------------------------------------------------*

CLASS lcl_colleague_total_stock DEFINITION
    INHERITING FROM lcl_colleague_alv.
  PUBLIC SECTION.
    TYPES: BEGIN OF ts_stock
          , matnr TYPE char18
          , matnr_tx TYPE text40
          , menge_st01 TYPE menge_d
          , meins_st01 TYPE meins
          , menge_stdl TYPE menge_d
          , meins_stdl TYPE meins
        , END OF ts_stock
        , tt_stock TYPE STANDARD TABLE OF ts_stock
        .

    METHODS constructor
      IMPORTING io_mediator TYPE REF TO lif_mediator.



    METHODS get_sel_line
      RETURNING VALUE(rs_sel) TYPE ts_stock.

    METHODS process_stock
      IMPORTING iv_action     TYPE char1 DEFAULT '-'
                is_stock TYPE ts_stock.

  PROTECTED SECTION.
    METHODS create_alv REDEFINITION.

    METHODS on_user_command REDEFINITION.

    METHODS handle_double_click REDEFINITION.

    METHODS on_link_click REDEFINITION.

  PRIVATE SECTION.


    DATA mt_stock TYPE tt_stock.
    DATA ms_stock TYPE ts_stock.


ENDCLASS.
