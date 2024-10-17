*&---------------------------------------------------------------------*
*& Include          ZREP_C8A014_DATETIME_CLS11
*&---------------------------------------------------------------------*

CLASS lcl_date_time_conv DEFINITION.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING is_scr TYPE lif_share=>ts_scr.

    METHODS fn
      RETURNING VALUE(rc) TYPE sysubrc.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES: BEGIN OF ts_conv
            , conv_type TYPE string
            , conv_res TYPE string
        , END OF ts_conv
        , tt_conv TYPE STANDARD TABLE OF ts_conv WITH DEFAULT KEY
        .
    DATA msr_scr TYPE REF TO lif_share=>ts_scr.

    DATA mt_date_conv TYPE tt_conv.
    DATA mt_time_conv TYPE tt_conv.


    METHODS _date_by_edit_mask.
    METHODS _date_by_format.
    METHODS _date_conv_via_function.
    METHODS _date_conv_via_class.

    METHODS _date_conv_via_set_country.

    METHODS _time_conv_edit_mask.
    METHODS _time_conv_set_country.
    METHODS _time_conv_custom.
ENDCLASS.

CLASS lcl_date_time_conv IMPLEMENTATION.
  METHOD constructor.
    msr_scr = REF #( is_scr ).
  ENDMETHOD.

  METHOD fn.

    CLEAR mt_date_conv.

    _date_by_edit_mask( ).

    _date_by_format( ).

    _date_conv_via_function( ).

    _date_conv_via_class( ).

    _date_conv_via_set_country( ).


    zcl_c8a014_show_html=>get_instance( )->add_tab_ch( it_tab = mt_date_conv ).

    _time_conv_edit_mask( ).

    _time_conv_set_country( ).

    _time_conv_custom( ).

    zcl_c8a014_show_html=>get_instance( )->add_tab_ch( it_tab = mt_time_conv ).

    zcl_c8a014_show_html=>get_instance( )->show( ).

  ENDMETHOD.

  METHOD _date_by_edit_mask.
    DATA ls_date_conv TYPE ts_conv.

    DATA lv_date_some TYPE sydatum .
    DATA lv_date_some_str TYPE char10.

    lv_date_some = sy-datum.

    WRITE lv_date_some TO lv_date_some_str USING EDIT MASK '__-__-____'.
    ls_date_conv-conv_type = 'USING EDIT MASK ''__-__-____'''.
    ls_date_conv-conv_res = lv_date_some_str.
    APPEND ls_date_conv TO mt_date_conv.

  ENDMETHOD.

  METHOD _date_by_format.
    DATA ls_date_conv TYPE ts_conv.

    DATA lv_date_some TYPE sydatum .
    DATA lv_date_some_str TYPE char10.

    lv_date_some = sy-datum.

    WRITE lv_date_some TO lv_date_some_str MM/DD/YYYY.
    ls_date_conv-conv_type = 'write to MM/DD/YYYY'.
    ls_date_conv-conv_res = lv_date_some_str.
    APPEND ls_date_conv TO mt_date_conv.

    " возможные форматы - элемент данных XUDATFM
*1  DD.MM.YYYY (Gregorian Date)
*2  MM/DD/YYYY (Gregorian Date)
*3  MM-DD-YYYY (Gregorian Date)
*4  YYYY.MM.DD (Gregorian Date)
*5  YYYY/MM/DD (Gregorian Date)
*6  YYYY-MM-DD (Gregorian Date, ISO 8601)
*7  GYY.MM.DD (Japanese Date)
*8  GYY/MM/DD (Japanese Date)
*9  GYY-MM-DD (Japanese Date)
*A  YYYY/MM/DD (Islamic Date 1)
*B  YYYY/MM/DD (Islamic Date 2)
*C  YYYY/MM/DD (Iranian Date)

    " https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abendate_formats.htm


  ENDMETHOD.

  METHOD _date_conv_via_function.
    DATA ls_date_conv TYPE ts_conv.
    DATA lv_date_some TYPE sydatum .
    DATA lv_date_some_str TYPE char10.

    lv_date_some = sy-datum - 3.
    " function group SCA1

    CALL FUNCTION 'CONVERSION_EXIT_LDATE_OUTPUT'
      EXPORTING
        input  = lv_date_some                 " internal date format
      IMPORTING
        output = ls_date_conv-conv_res.                 " external date format
    ls_date_conv-conv_type = 'Func CONVERSION_EXIT_LDATE_OUTPUT'.
    APPEND ls_date_conv TO mt_date_conv.


    CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
      EXPORTING
        input  = lv_date_some                 " internal date format
      IMPORTING
        output = ls_date_conv-conv_res.                 " external date format
    ls_date_conv-conv_type = 'Func CONVERSION_EXIT_SDATE_OUTPUT'.
    APPEND ls_date_conv TO mt_date_conv.

  ENDMETHOD.

  METHOD _date_conv_via_class.
    DATA ls_date_conv TYPE ts_conv.
    DATA lv_date_some TYPE sydatum .
    DATA lv_date_some_str TYPE char20.

    DATA lv_user_target_format TYPE xudatfm.
    DATA lv_used_format TYPE xudatfm.

    lv_date_some = sy-datum - 3.

    DO 2 TIMES.

      CASE sy-index.
        WHEN 1.
          lv_user_target_format = cl_abap_datfm=>get_datfm( ).
        WHEN 2.
          " можем взять формат страны согласно настройкам в таблице - T005X
          lv_user_target_format = cl_abap_datfm=>get_country_datfm( country = 'KR' ).
        WHEN OTHERS.
      ENDCASE.

      TRY .

          cl_abap_datfm=>conv_date_int_to_ext(
            EXPORTING
              im_datint    = lv_date_some                 " internal representation of date
              im_datfmdes  = lv_user_target_format                 " date format wanted for conversion
        IMPORTING
          ex_datext    = lv_date_some_str                 " external representation of date
          ex_datfmused = lv_used_format                 " date format used for conversion
          ).

          ls_date_conv-conv_type = |cl_abap_datfm=> { lv_user_target_format }|.
          ls_date_conv-conv_res = |{ lv_date_some_str }|.


        CATCH cx_abap_datfm_format_unknown. " Exception in Class CL_ABAP_DATFM - Format unknown
          ls_date_conv-conv_type = |cl_abap_datfm=> { lv_user_target_format }|.
          ls_date_conv-conv_res = |Exception|.

      ENDTRY.

      APPEND ls_date_conv TO mt_date_conv.
    ENDDO.


  ENDMETHOD.

  METHOD _date_conv_via_set_country.
    " DEMO_STRING_TEMPLATE_ENV_SETT - расширенный пример
    DATA ls_date_conv TYPE ts_conv.
    DATA lv_date_some TYPE sydatum .
    DATA lv_date_some_str TYPE char10.

    DATA lv_default_country TYPE t005x-land.
    DATA lv_format_country TYPE t005x-land VALUE 'KR'.

    CALL FUNCTION 'ADDR_DEFAULT_SENDER_COUNTRY'
      IMPORTING
        ev_sender_country = lv_default_country.

    lv_date_some = sy-datum - 3.
    SET COUNTRY lv_format_country.
    WRITE lv_date_some TO lv_date_some_str.
    ls_date_conv-conv_type = |via Country { lv_format_country }|.
    ls_date_conv-conv_res = lv_date_some_str.
    APPEND ls_date_conv TO mt_date_conv.

    SET COUNTRY lv_default_country.
    WRITE lv_date_some TO lv_date_some_str.
    ls_date_conv-conv_type = |via Country { lv_default_country }|.
    ls_date_conv-conv_res = lv_date_some_str.
    APPEND ls_date_conv TO mt_date_conv.

  ENDMETHOD.

  METHOD _time_conv_edit_mask.
    DATA lv_time_some TYPE syuzeit .
    DATA lv_time_some_str TYPE text40.
    DATA ls_time_conv TYPE ts_conv.

    lv_time_some = sy-uzeit.
    WRITE lv_time_some TO lv_time_some_str USING EDIT MASK 'Сейчас = __:__:__'.
    ls_time_conv-conv_type = 'Time via edit mask: '.
    ls_time_conv-conv_res = lv_time_some_str.

    APPEND ls_time_conv TO mt_time_conv.

  ENDMETHOD.

  METHOD _time_conv_set_country.
    CONSTANTS lc_10mins TYPE int4 VALUE '600'.

    DATA lv_time_some TYPE syuzeit .
    DATA lv_time_some_str TYPE text40.
    DATA ls_time_conv TYPE ts_conv.

    DATA lv_default_country TYPE t005x-land.
    DATA lv_format_country TYPE t005x-land VALUE 'KR'.

    lv_time_some = sy-uzeit - lc_10mins.

    CALL FUNCTION 'ADDR_DEFAULT_SENDER_COUNTRY'
      IMPORTING
        ev_sender_country = lv_default_country.

    SET COUNTRY lv_format_country.
    WRITE lv_time_some TO lv_time_some_str.
    ls_time_conv-conv_type = |time via Country { lv_format_country }|.
    ls_time_conv-conv_res = lv_time_some_str.
    APPEND ls_time_conv TO mt_time_conv.

    SET COUNTRY lv_default_country.
    WRITE lv_time_some TO lv_time_some_str.
    ls_time_conv-conv_type = |time via Country { lv_default_country }|.
    ls_time_conv-conv_res = lv_time_some_str.
    APPEND ls_time_conv TO mt_time_conv.

  ENDMETHOD.

  METHOD _time_conv_custom.
    " DEMO_STRING_TEMPLATE_ENV_SETT - расширенный пример
    DATA lv_time_some TYPE syuzeit .
    DATA lv_time_some_str TYPE text40.
    DATA ls_time_conv TYPE ts_conv.

    DATA lv_launch_time_beg TYPE syuzeit VALUE '130000'.
    DATA lv_launch_time_end TYPE syuzeit VALUE '140000'.


    lv_time_some = '131214'.
    IF lv_time_some GE lv_launch_time_beg
        AND lv_launch_time_end LE lv_launch_time_end.
      lv_time_some_str = 'Время обеда'.
      ls_time_conv-conv_type = 'Любой каприз -)'.
      ls_time_conv-conv_res = lv_time_some_str.
    ENDIF.

    APPEND ls_time_conv TO mt_time_conv.

  ENDMETHOD.


ENDCLASS.
