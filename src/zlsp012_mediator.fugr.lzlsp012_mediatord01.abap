*&---------------------------------------------------------------------*
*& Include          LZLSP012_MEDIATORD01
*&---------------------------------------------------------------------*

" mediator - посредник
INTERFACE lif_mediator.
  METHODS notify
    IMPORTING io_obj TYPE REF TO object.
ENDINTERFACE.
