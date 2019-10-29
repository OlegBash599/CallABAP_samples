*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW02_DATA
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ts_screen
       , p_text TYPE text200
       , p_id TYPE text20
       , p_idval TYPE text200
       , END OF ts_screen
       .


DATA gs_scr TYPE ts_screen.

CLASS lcl_app DEFINITION DEFERRED.
DATA go_app TYPE REF TO lcl_app.
