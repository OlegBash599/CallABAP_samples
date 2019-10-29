*&---------------------------------------------------------------------*
*& Include          LZLSP012_MEDIATORD05
*&---------------------------------------------------------------------*

CLASS lcl_colleague_log DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_mediator TYPE REF TO lif_mediator.

    METHODS fill_block.

    METHODS add_line2log
      IMPORTING iv_src  TYPE text20
                iv_line TYPE string.

    METHODS do_refresh.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_container TYPE REF TO cl_gui_custom_container.

    METHODS show_cont.

ENDCLASS.
