*&---------------------------------------------------------------------*
*& Include          ZREP_C8A014_DATETIME_CLS10
*&---------------------------------------------------------------------*

CLASS lcl_sys_vars DEFINITION.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING is_scr TYPE lif_share=>ts_scr.

    METHODS fn
      RETURNING VALUE(rc) TYPE sysubrc.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA msr_scr TYPE REF TO lif_share=>ts_scr.

    METHODS _conv_date_def
      IMPORTING iv        TYPE sydatum DEFAULT sy-datum
      RETURNING VALUE(rv) TYPE string.

    METHODS _conv_time_def
      IMPORTING iv        TYPE syuzeit DEFAULT sy-uzeit
      RETURNING VALUE(rv) TYPE string.

    METHODS _show_sys_date_n_time.

    METHODS _show_client_date_n_time.

ENDCLASS.


CLASS lcl_sys_vars IMPLEMENTATION.
  METHOD constructor.
    msr_scr = REF #( is_scr ).
  ENDMETHOD.

  METHOD fn.

    " https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abentime_system_fields.htm

    GET TIME. " обновляет системные переменные даты и времени

    zcl_c8a014_show_html=>get_instance(
        iv_title = 'Системные переменные дат'
    ).

    _show_sys_date_n_time( ).

    _show_client_date_n_time( ).


    zcl_c8a014_show_html=>get_instance( )->add_para_val_ch(
      EXPORTING
        iv_id    = 'ABAP_help latest'
        iv_value = 'https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abentime_system_fields.htm'
    ).

    zcl_c8a014_show_html=>get_instance( )->show( ).

  ENDMETHOD.

  METHOD _show_sys_date_n_time.

    DATA lv_sys_timezone TYPE ttzz-tzone.

    CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
      IMPORTING
        timezone            = lv_sys_timezone                 " Time Zone
      EXCEPTIONS
        customizing_missing = 1
        OTHERS              = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
    ENDIF.

    zcl_c8a014_show_html=>get_instance(
  )->add_para_val_ch( EXPORTING
    iv_id    = '----Дата/Время сервера----' iv_value = ''
  )->add_para_val_ch( EXPORTING
    iv_id    = 'SY-DATUM - дата сервера'
    iv_value = _conv_date_def( sy-datum )
)->add_para_val_ch( EXPORTING
    iv_id    = 'SY-UZEIT - время сервера'
    iv_value = _conv_time_def( sy-uzeit )
)->add_para_val_ch( EXPORTING
    iv_id    = 'Времен.зона сервера / FUNC GET_SYSTEM_TIMEZONE'
    iv_value = lv_sys_timezone
)->add_para_val_ch( EXPORTING
    iv_id    = 'Разница в секундах: временем сервера и UTC'
    iv_value = sy-tzone
    )
.

  ENDMETHOD.

  METHOD _show_client_date_n_time.
    zcl_c8a014_show_html=>get_instance(
 )->add_para_val_ch( EXPORTING
    iv_id    = '----Дата/Время клиента----' iv_value = ''
)->add_para_val_ch( EXPORTING
    iv_id    = 'SY-DATLO - дата клиента (Vladivostok)'
    iv_value = _conv_date_def( sy-datlo )
)->add_para_val_ch( EXPORTING
    iv_id    = 'SY-TIMLO - время клиента (Vladivostok)'
    iv_value = _conv_time_def( sy-timlo )
)->add_para_val_ch( EXPORTING
    iv_id    = 'SY-ZONLO - Времен.зона клиента'
    iv_value = sy-zonlo
)
.
  ENDMETHOD.

  METHOD _conv_date_def.
    "  IMPORTING iv        TYPE sydatum DEFAULT sy-datum
    "  RETURNING VALUE(rv) TYPE string.
    DATA lv_date_char10 TYPE char10.

    WRITE iv TO lv_date_char10.

    rv = lv_date_char10.
  ENDMETHOD.

  METHOD _conv_time_def.
    "    IMPORTING iv        TYPE syuzeit DEFAULT sy-uzeit
    "    RETURNING VALUE(rv) TYPE string.

    DATA lv_time_char10 TYPE char10.

    WRITE iv TO lv_time_char10.

    rv = lv_time_char10.

  ENDMETHOD.

ENDCLASS.
