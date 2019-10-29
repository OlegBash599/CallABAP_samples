*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW02_EVNT
*&---------------------------------------------------------------------*

INITIALIZATION.
  CREATE OBJECT go_app.

AT SELECTION-SCREEN.

  gs_scr-p_text = p_text.
  gs_scr-p_id = p_id.
  gs_scr-p_idval = p_idval.

  go_app->lif_app~set_scr( is_scr = gs_scr iv_syucomm = sy-ucomm ).


START-OF-SELECTION.
  go_app->lif_app~process( ).

END-OF-SELECTION.
  go_app->lif_app~output( ).
