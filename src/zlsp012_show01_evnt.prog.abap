*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW01_EVNT
*&---------------------------------------------------------------------*

START-OF-SELECTION.

  " BREAK-POINT.

  DATA lo_id TYPE REF TO lcl_id.
  DATA lo_id2 TYPE REF TO lcl_id.

*  CREATE OBJECT lo_id.
*
*  lo_id->set_id_val(
*    EXPORTING
*      iv_id  = 'ZZ1'
*      iv_val =  'Значение для ID ZZ1'
*  ).
**
*  lo_id->show_id_val( ).
**
*  CREATE OBJECT lo_id2.
*
*  BREAK-POINT.
*  lo_id2->set_id_val(
*    EXPORTING
*      iv_id  = 'ZZ2'
*      iv_val =  'Значение для ID ZZ2'
*  ).
*
*  lo_id2->show_id_val( ).

*
  DATA lo_id_s2db TYPE REF TO lcl_id_s2db.
  BREAK-POINT.
  CREATE OBJECT lo_id_s2db.
  lo_id_s2db->set_id_val(
    EXPORTING
      iv_id  = 'ZZ3'
      iv_val =  'inhere ID ZZ3'
  ).


  lo_id_s2db->save2db( ).
