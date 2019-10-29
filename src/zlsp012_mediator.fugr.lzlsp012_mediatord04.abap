*&---------------------------------------------------------------------*
*& Include          LZLSP012_MEDIATORD04
*&---------------------------------------------------------------------*

CLASS lcl_colleague_sales DEFINITION
    INHERITING FROM lcl_colleague_alv.
  PUBLIC SECTION.

    TYPES: BEGIN OF ts_stock
              , customer TYPE kunnr
              , customer_tx TYPE text40
              , delivery_date TYPE sydatum
              , matnr TYPE char18
              , matnr_tx TYPE text40
              , menge TYPE menge_d
              , meins TYPE meins
            , END OF ts_stock
            , tt_stock TYPE STANDARD TABLE OF ts_stock
            .


    METHODS constructor
      IMPORTING io_mediator TYPE REF TO lif_mediator.



    METHODS get_sel_line
      RETURNING VALUE(rs_sel) TYPE ts_stock.

    METHODS process_stock
      IMPORTING iv_action TYPE char1 DEFAULT '-'
                is_stock  TYPE ts_stock.

  PROTECTED SECTION.
    METHODS create_alv REDEFINITION.

    METHODS on_user_command REDEFINITION.

    METHODS handle_double_click REDEFINITION.

    METHODS on_link_click REDEFINITION.

  PRIVATE SECTION.


    DATA mt_stock TYPE tt_stock.
    DATA ms_stock TYPE ts_stock.



ENDCLASS.
