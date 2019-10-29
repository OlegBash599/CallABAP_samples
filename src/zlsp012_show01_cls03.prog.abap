*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW01_CLS03
*&---------------------------------------------------------------------*

CLASS lcl_id_s2db_2 DEFINITION INHERITING FROM lcl_id_s2db.
  PUBLIC SECTION.
    METHODS save2db REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.


ENDCLASS.                    "lcl_id_s2db_2 DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_id_s2db_2 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_id_s2db_2 IMPLEMENTATION.
  METHOD save2db.
    MESSAGE i003 WITH me->mv_id me->mv_val.
  ENDMETHOD.                    "save2db
ENDCLASS.
