*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW04_DATA
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ts_screen
*       , p_text TYPE text200
*       , p_id TYPE text20
*       , p_idval TYPE text200

       , r_absfac TYPE char1
       , r_single TYPE char1
       , r_facmet TYPE char1
       , r_build TYPE char1
       , r_proto TYPE char1

       , r_adapt TYPE char1
       , r_bridg TYPE char1
       , r_compo TYPE char1
       , r_decor TYPE char1
       , r_facad TYPE char1
       , r_flywei TYPE char1
       , r_proxy TYPE char1


       , r_chofr TYPE char1
       , r_cmd TYPE char1
       , r_itera TYPE char1
       , r_mediat TYPE char1
       , r_mement TYPE char1
       , r_observ TYPE char1
       , r_state TYPE char1
       , strat TYPE char1
       , r_tmplme TYPE char1
       , r_visito TYPE char1

       , END OF ts_screen
       .


DATA gs_scr TYPE ts_screen.

CLASS lcl_app DEFINITION DEFERRED.
DATA go_app TYPE REF TO lcl_app.
