*&---------------------------------------------------------------------*
*& Include          LZLSP012_MEDIATORD06
*&---------------------------------------------------------------------*

CLASS lcl_mediator_director DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_mediator.
    METHODS constructor.

    METHODS fill_blocks.



  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA lo_colleague_consignment TYPE REF TO lcl_colleague_consignment.
    DATA lo_colleague_sales TYPE REF TO lcl_colleague_sales.
    DATA lo_colleague_total_stock TYPE REF TO lcl_colleague_total_stock.

    DATA lo_colleague_log TYPE REF TO lcl_colleague_log.

    METHODS move2sales.
    METHODS moveoutsales.

    METHODS write2log
      IMPORTING iv_src  TYPE text20
                iv_line TYPE string.

    METHODS refresh
      IMPORTING iv_cons  TYPE char1
                iv_sales TYPE char1
                iv_total TYPE char1
                iv_log   TYPE char1.

ENDCLASS.
