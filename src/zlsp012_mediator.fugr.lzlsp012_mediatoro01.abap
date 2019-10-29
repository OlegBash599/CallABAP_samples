*&---------------------------------------------------------------------*
*& Include          LZLSP012_MEDIATORO01
*&---------------------------------------------------------------------*

MODULE status_7500 OUTPUT.
  SET TITLEBAR 'ZLSP012_T1' WITH sy-datum sy-uzeit.
  SET PF-STATUS 'ZLSP012_ST2'.

  IF go_app IS BOUND.
    go_app->screen_view( ).
  ENDIF.

ENDMODULE.
