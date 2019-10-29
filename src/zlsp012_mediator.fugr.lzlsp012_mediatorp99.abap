*&---------------------------------------------------------------------*
*& Include          LZLSP012_MEDIATORP99
*&---------------------------------------------------------------------*

CLASS lcl_app IMPLEMENTATION.

  METHOD constructor.
    mo_mediator_director = NEW #( ).
  ENDMETHOD.

  METHOD syucomm.

  ENDMETHOD.

  METHOD free.

  ENDMETHOD.

  METHOD prepare.
    mo_mediator_director->fill_blocks( ).
  ENDMETHOD.

  METHOD screen_view.

  ENDMETHOD.

ENDCLASS.
