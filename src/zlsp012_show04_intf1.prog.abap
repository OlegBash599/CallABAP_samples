*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW04_INTF1
*&---------------------------------------------------------------------*

INTERFACE lif_app.
  INTERFACES zif_lsp012_app.
**
**  METHODS process.
**  METHODS output.
**  METHODS  set_scr
**    IMPORTING is_scr TYPE ts_screen
**              iv_syucomm TYPE syucomm.
**  METHODS do_check_before.
**
**
**ENDINTERFACE.                    "lif_app
**
**
**INTERFACE lif_app_const.
**
**  CONSTANTS:
**    c_onli TYPE syucomm VALUE 'ONLI'.
**
ENDINTERFACE.
