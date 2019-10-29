INTERFACE zif_lsp012_app
  PUBLIC .


  CONSTANTS c_onli TYPE syucomm VALUE 'ONLI'.               "#EC NOTEXT

  METHODS process .
  METHODS output .
  METHODS set_scr
    IMPORTING
      !is_scr     TYPE any
      !iv_syucomm TYPE syucomm .
  METHODS do_check_before .
  METHODS commit
    IMPORTING
      !iv_wait TYPE sap_bool .

ENDINTERFACE.
