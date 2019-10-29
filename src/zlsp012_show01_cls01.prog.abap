*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW01_CLS01
*&---------------------------------------------------------------------*

CLASS lcl_id DEFINITION.

  PUBLIC SECTION.
    METHODS constructor.

    METHODS set_id_val
      IMPORTING iv_id  TYPE string
                iv_val TYPE string.

    METHODS show_id_val.

  PROTECTED SECTION.

    DATA mv_id TYPE string.
    DATA mv_val TYPE string.

  PRIVATE SECTION.

*    DATA mv_id TYPE string.
*    DATA mv_val TYPE string.

ENDCLASS.                    "lcl_id DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_id IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_id IMPLEMENTATION.
  METHOD constructor.
    BREAK-POINT.
  ENDMETHOD.                    "constructor

  METHOD set_id_val.
    mv_id = iv_val.
    mv_val = iv_val.
  ENDMETHOD.                    "set_id_val

  METHOD show_id_val.
    MESSAGE i001 WITH mv_id mv_val.
  ENDMETHOD.                    "show_id_val

ENDCLASS.
