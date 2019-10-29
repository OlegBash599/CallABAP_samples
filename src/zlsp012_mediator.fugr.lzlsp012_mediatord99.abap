*&---------------------------------------------------------------------*
*& Include          LZLSP012_MEDIATORD99
*&---------------------------------------------------------------------*

CLASS lcl_app DEFINITION.

  PUBLIC SECTION.
    METHODS constructor.
    METHODS syucomm
      IMPORTING iv_syucomm TYPE syucomm.

    METHODS free.
    METHODS prepare.
    METHODS screen_view.

  PROTECTED SECTION.

  PRIVATE SECTION.
    data mo_mediator_director TYPE REF TO lcl_mediator_director.
ENDCLASS.
