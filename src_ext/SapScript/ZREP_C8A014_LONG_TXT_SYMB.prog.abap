*&---------------------------------------------------------------------*
*& Report ZREP_C8A014_LONG_TXT_SYMB
*&---------------------------------------------------------------------*
*& Использование длинных текстов в качестве шаблона с переменными
*&---------------------------------------------------------------------*
REPORT zrep_c8a014_long_txt_symb.

TYPES: BEGIN OF ts_model
            , v1 TYPE string
            , v2 TYPE string
            , v3 TYPE string
            , v4 TYPE string
        , END OF ts_model
        .

PARAMETERS:   p_tdname TYPE thead-tdname DEFAULT 'ZSD_MAIL_ORDER_CREATED_ALL'
            , p_tdobj TYPE thead-tdid DEFAULT 'TEXT'
            , p_tdid TYPE thead-tdid DEFAULT 'ST'
            , p_langu TYPE sylangu DEFAULT 'RU'
            .

START-OF-SELECTION.
  DATA lv_txt TYPE string.
  DATA ls_txt_h TYPE thead.

  data s TYPE ts_model.

  ls_txt_h-tdid     = p_tdid.
  ls_txt_h-tdspras  = p_langu.
  ls_txt_h-tdobject = p_tdobj.
  ls_txt_h-tdname   = p_tdname.

  s-v1 = 'Переменная V1'.
  s-v2 = 'Вторая переменная'.
  s-v3 = 'СОЛОВЕЙ'.

  lv_txt = NEW zcl_c8a014_long_text( sy-cprog )->r( ls_txt_h ).

  DATA lv_as_html_out TYPE string.
  lv_as_html_out =
          | <html dir="ltr" lang="ru"> |
      && |<head> <meta charset="UTF-8">|
      && |<meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1">|
      && |<title>ABAP String Show </title>|
      && |</head> <body>|
      && |<div> { lv_txt } </div>|
      && |</body> </html>|
      .

  cl_abap_browser=>show_html(
    EXPORTING
      html_string  = lv_as_html_out                           " HTML String
).
