INTERFACE zif_lsp012_command
  PUBLIC .

  METHODS execute
    RETURNING VALUE(rv_val) TYPE boolean.

ENDINTERFACE.
