*&---------------------------------------------------------------------*
*& Include          ZREP_C8A014_DATETIME_EVNTS
*&---------------------------------------------------------------------*

INITIALIZATION.
  CREATE OBJECT lo_rep_app.
  lo_rep_app->init( ).

at SELECTION-SCREEN on VALUE-REQUEST FOR p_mode.
  lo_rep_app->on_val_req_p_mode( CHANGING cv = p_mode ).

AT SELECTION-SCREEN.
  lo_rep_app->at_sel_screen( ).

AT SELECTION-SCREEN OUTPUT.
  lo_rep_app->at_sel_screen_output( ).

START-OF-SELECTION.
  lo_rep_app->start_of_sel( ).

end-of-SELECTION.
  lo_rep_app->end_of_sel( ).
