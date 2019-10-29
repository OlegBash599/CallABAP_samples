*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW04_EVNT
*&---------------------------------------------------------------------*

INITIALIZATION.
  CREATE OBJECT go_app.

AT SELECTION-SCREEN OUTPUT.
  go_app->at_sel_scr_out( ).

AT SELECTION-SCREEN.

*  gs_scr-p_text = p_text.
*  gs_scr-p_id = p_id.
*  gs_scr-p_idval = p_idval.

  gs_scr-r_single = r_single.
  gs_scr-r_facmet = r_facmet.
  gs_scr-r_absfac = r_absfac.
  gs_scr-r_build = r_build.
  gs_scr-r_proto = r_proto.

  gs_scr-r_adapt = r_adapt.
  gs_scr-r_bridg = r_bridg.
  gs_scr-r_compo = r_compo.
  gs_scr-r_decor = r_decor.
  gs_scr-r_facad = r_facad.
  gs_scr-r_flywei = r_flywei.
  gs_scr-r_proxy = r_proxy.

  gs_scr-r_chofr = r_chofr.
  gs_scr-r_cmd = r_cmd.
  gs_scr-r_itera = r_itera.
  gs_scr-r_mediat = r_mediat.
  gs_scr-r_mement = r_mement.
  gs_scr-r_observ = r_observ.
  gs_scr-r_state = r_state.
  gs_scr-strat = strat.
  gs_scr-r_tmplme = r_tmplme.
  gs_scr-r_visito = r_visito.


  go_app->zif_lsp012_app~set_scr( is_scr = gs_scr iv_syucomm = sy-ucomm ).


START-OF-SELECTION.
  go_app->zif_lsp012_app~process( ).

END-OF-SELECTION.
  go_app->zif_lsp012_app~output( ).
