*&---------------------------------------------------------------------*
*& Include          LZLSP012_MEDIATORD02
*&---------------------------------------------------------------------*

CLASS lcl_colleague_consignment DEFINITION
    INHERITING FROM lcl_colleague_alv.
  PUBLIC SECTION.
    TYPES: BEGIN OF ts_cons_stock
            , matnr TYPE char18
            , matnr_tx TYPE text40
            , menge TYPE menge_d
            , meins TYPE meins
            , END OF ts_cons_stock
            , tt_cons_stock TYPE STANDARD TABLE OF ts_cons_stock
            .

    METHODS constructor
      IMPORTING io_mediator TYPE REF TO lif_mediator.



    METHODS get_sel_line
      RETURNING VALUE(rs_sel) TYPE ts_cons_stock.

    METHODS process_stock
      IMPORTING iv_action     TYPE char1 DEFAULT '-'
                is_cons_stock TYPE ts_cons_stock.

  PROTECTED SECTION.
    METHODS create_alv REDEFINITION.

    METHODS on_user_command REDEFINITION.

    METHODS handle_double_click REDEFINITION.

    METHODS on_link_click REDEFINITION.

  PRIVATE SECTION.


    DATA mt_cons_stock TYPE tt_cons_stock.
    DATA ms_cons_stock TYPE ts_cons_stock.



ENDCLASS.
