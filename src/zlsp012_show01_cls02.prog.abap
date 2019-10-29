*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW01_CLS02
*&---------------------------------------------------------------------*

CLASS lcl_id_s2db DEFINITION INHERITING FROM lcl_id.
  PUBLIC SECTION.
    CLASS-METHODS: class_constructor.

    METHODS save2db
      IMPORTING iv_commit TYPE char1 DEFAULT abap_false.

    METHODS set_id_val REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS bapi_commit.
ENDCLASS.                    "lcl_id_s2db DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_id_s2db IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_id_s2db IMPLEMENTATION.

  METHOD class_constructor.
    BREAK-POINT.
  ENDMETHOD.

  METHOD save2db.
    " save to table

    MESSAGE i002 WITH me->mv_id me->mv_val.

    IF iv_commit EQ abap_true.
      me->bapi_commit( ).
    ENDIF.
  ENDMETHOD.                    "save2db

  METHOD bapi_commit.

  ENDMETHOD.                    "bapi_commit

  METHOD set_id_val.
    BREAK-POINT.
  ENDMETHOD.

ENDCLASS.
