*&---------------------------------------------------------------------*
*& Include          ZREP_C8A014_DATETIME_CLS12
*&---------------------------------------------------------------------*

CLASS lcl_date_time_calc DEFINITION.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING is_scr TYPE lif_share=>ts_scr.

    METHODS fn
      RETURNING VALUE(rc) TYPE sysubrc.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES: BEGIN OF ts_res_out
        , show_txt TYPE string
        , res TYPE string
    , END OF ts_res_out
    , tt_res_out TYPE STANDARD TABLE OF ts_res_out WITH DEFAULT KEY
    .

    DATA msr_scr TYPE REF TO lif_share=>ts_scr.
    DATA mt_res_out TYPE tt_res_out.

    METHODS _first_day_of_cur_month.
    METHODS _first_day_of_prev_month.
    METHODS _first_day_of_next_month.

    METHODS _last_day_of_cur_month.
    METHODS _last_day_of_prev_month.
    METHODS _last_day_of_next_month.

    METHODS _conv_date_def
      IMPORTING iv        TYPE sydatum DEFAULT sy-datum
      RETURNING VALUE(rv) TYPE string.

    METHODS _conv_time_def
      IMPORTING iv        TYPE syuzeit DEFAULT sy-uzeit
      RETURNING VALUE(rv) TYPE string.

ENDCLASS.

CLASS lcl_date_time_calc IMPLEMENTATION.
  METHOD constructor.
    "IMPORTING is_scr TYPE lif_share=>ts_scr.
    msr_scr = REF #( is_scr ).
  ENDMETHOD.

  METHOD fn.
    "RETURNING VALUE(rc) TYPE sysubrc.
    _first_day_of_cur_month( ).
    _first_day_of_prev_month( ).
    _first_day_of_next_month( ).

    _last_day_of_cur_month( ).
    _last_day_of_prev_month( ).
    _last_day_of_next_month( ).

    zcl_c8a014_show_html=>get_instance( )->add_tab_ch( it_tab = mt_res_out ).

    zcl_c8a014_show_html=>get_instance( )->show( ).

  ENDMETHOD.

  METHOD _first_day_of_cur_month.

    DATA lv_target_day TYPE sydatum.
    DATA ls_res_out TYPE ts_res_out.

    lv_target_day = sy-datum.
    lv_target_day+6(2) = '01'.

    ls_res_out-show_txt = '1st Day of Month'.
    ls_res_out-res = _conv_date_def( lv_target_day ).

    APPEND ls_res_out TO mt_res_out.

  ENDMETHOD.
  METHOD _first_day_of_prev_month.
    DATA lv_target_day TYPE sydatum.
    DATA ls_res_out TYPE ts_res_out.

    lv_target_day = sy-datum.
    lv_target_day+6(2) = '01'.
    lv_target_day -= 1.
    lv_target_day+6(2) = '01'.

    ls_res_out-show_txt = 'first_day_of_prev_month'.
    ls_res_out-res = _conv_date_def( lv_target_day ).

    APPEND ls_res_out TO mt_res_out.
  ENDMETHOD.

  METHOD _first_day_of_next_month.
    DATA lv_target_day TYPE sydatum.
    DATA ls_res_out TYPE ts_res_out.

    lv_target_day = sy-datum.
    lv_target_day+6(2) = '01'.
    lv_target_day += 45.
    lv_target_day+6(2) = '01'.

    ls_res_out-show_txt = 'first_day_of_next_month'.
    ls_res_out-res = _conv_date_def( lv_target_day ).

    APPEND ls_res_out TO mt_res_out.
  ENDMETHOD.

  METHOD _last_day_of_cur_month.
    DATA lv_target_day TYPE sydatum.
    DATA ls_res_out TYPE ts_res_out.

    lv_target_day = sy-datum.
    lv_target_day+6(2) = '01'.
    lv_target_day += 45.
    lv_target_day+6(2) = '01'.
    lv_target_day -= 1.

    ls_res_out-show_txt = '_last_day_of_cur_month'.
    ls_res_out-res = _conv_date_def( lv_target_day ).

    APPEND ls_res_out TO mt_res_out.
  ENDMETHOD.

  METHOD _last_day_of_prev_month.
    DATA lv_target_day TYPE sydatum.
    DATA ls_res_out TYPE ts_res_out.

    lv_target_day = sy-datum.
    lv_target_day+6(2) = '01'.
    lv_target_day -= 1.

    ls_res_out-show_txt = '_last_day_of_prev_month'.
    ls_res_out-res = _conv_date_def( lv_target_day ).

    APPEND ls_res_out TO mt_res_out.
  ENDMETHOD.

  METHOD _last_day_of_next_month.
    DATA lv_target_day TYPE sydatum.
    DATA ls_res_out TYPE ts_res_out.

    lv_target_day = sy-datum.
    lv_target_day+6(2) = '01'.
    lv_target_day += 70.
    lv_target_day+6(2) = '01'.
    lv_target_day -= 1.

    ls_res_out-show_txt = '_last_day_of_next_month'.
    ls_res_out-res = _conv_date_def( lv_target_day ).

    APPEND ls_res_out TO mt_res_out.
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
