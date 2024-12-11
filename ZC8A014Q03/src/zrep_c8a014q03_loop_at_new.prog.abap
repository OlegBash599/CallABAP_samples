*&---------------------------------------------------------------------*
*& Report ZREP_C8A014Q03_LOOP_AT_NEW
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrep_c8a014q03_loop_at_new.

PARAMETERS: p_mode TYPE char1 DEFAULT 'W'.

CLASS lcl_app DEFINITION.

  PUBLIC SECTION.
    METHODS main.
  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES: BEGIN OF ts_forecast
            , budat TYPE sydatum
            , timestamp TYPE timestamp
            , vtstamp TYPE timestamp
            , racct TYPE char10
            , matnr TYPE matnr18
            , werks TYPE werks_d
            , lbkum TYPE umlme
            , msl TYPE umlme
            , poper TYPE char2
            , gjahr TYPE char4
          , END OF ts_forecast
          , tt_forecast TYPE STANDARD TABLE OF ts_forecast WITH EMPTY KEY
          .

    DATA mt_some_forecast TYPE tt_forecast.

    METHODS _do_load_prefill.


    METHODS _read_max_by_at_new.
    METHODS _read_max_by_srt.
    METHODS _read_max_group_by.
    METHODS _read_max_group_by_no_membr.

ENDCLASS.


CLASS lcl_app IMPLEMENTATION.
  METHOD main.



    _do_load_prefill( ).


    _read_max_by_at_new( ).
    _read_max_by_srt( ).
    _read_max_group_by( ).
    _read_max_group_by_no_membr( ).


  ENDMETHOD.

  METHOD _read_max_by_at_new.

    DATA lt_some_forecast_init TYPE tt_forecast.
    DATA lt_tab_with_max_date TYPE tt_forecast.

    DATA lv_combination_is_new TYPE abap_bool.

    FIELD-SYMBOLS <fs_some_forecast> TYPE ts_forecast.
    lt_some_forecast_init = mt_some_forecast. " just to to change initial tab for other approaches

    SORT lt_some_forecast_init BY matnr werks  " you need to do sorting
                               budat DESCENDING
                               .


    "" this WILL NOT GROUP CORRECTLY because budat is differ and it is on the LEFR from your component MATNR
    "" NO GROUP ~~~~~ NO GROUP ~~~~~ NO GROUP ~~~~~ NO GROUP ~~~~~ NO GROUP ~~~~~
    LOOP AT lt_some_forecast_init ASSIGNING <fs_some_forecast>.
      lv_combination_is_new = abap_false.
      AT NEW matnr .
        lv_combination_is_new = abap_true.
      ENDAT.
      AT NEW werks .
        lv_combination_is_new = abap_true.
      ENDAT.

      IF lv_combination_is_new EQ abap_true.
        APPEND <fs_some_forecast> TO lt_tab_with_max_date.
      ENDIF.
    ENDLOOP.
    "" NO GROUP ~~~~~ NO GROUP ~~~~~ NO GROUP ~~~~~ NO GROUP ~~~~~ NO GROUP ~~~~~
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""'
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""'
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""'

    """ but here
    TYPES: BEGIN OF ts_key_at_begin_of_line
         , matnr TYPE matnr18 " what is in the AT_ENDAT - should be at the BEGGING
        , werks TYPE werks_d  " what is in the AT_ENDAT - should be at the BEGGING
        , budat TYPE sydatum
        , timestamp TYPE timestamp
        , vtstamp TYPE timestamp
        , racct TYPE char10
        , lbkum TYPE umlme
        , msl TYPE umlme
        , poper TYPE char2
        , gjahr TYPE char4
      , END OF ts_key_at_begin_of_line
      , tt_key_at_begin_of_line TYPE STANDARD TABLE OF ts_key_at_begin_of_line WITH EMPTY KEY
      .

    DATA lt_key_at_begin_of_line  TYPE tt_key_at_begin_of_line .
    FIELD-SYMBOLS <fs_key_at_begin> TYPE ts_key_at_begin_of_line .
    DATA lt_tab_with_max_date2 TYPE tt_forecast.

    MOVE-CORRESPONDING lt_some_forecast_init TO lt_key_at_begin_of_line.
    CLEAR lt_tab_with_max_date2.

    LOOP AT lt_key_at_begin_of_line ASSIGNING <fs_key_at_begin>.
      lv_combination_is_new = abap_false.
      AT NEW matnr .
        lv_combination_is_new = abap_true.
      ENDAT.
      AT NEW werks .
        lv_combination_is_new = abap_true.
      ENDAT.

      IF lv_combination_is_new EQ abap_true.
        APPEND INITIAL LINE TO lt_tab_with_max_date2 ASSIGNING <fs_some_forecast>.
        MOVE-CORRESPONDING <fs_key_at_begin> TO <fs_some_forecast>.
      ENDIF.
    ENDLOOP.
    "

    BREAK-POINT.
    " please compare lt_tab_with_max_date2 and lt_tab_with_max_date

  ENDMETHOD.

  METHOD _read_max_by_srt.
    DATA lt_tab_with_max_date TYPE SORTED TABLE OF ts_forecast WITH UNIQUE KEY matnr werks.

    FIELD-SYMBOLS <fs_some_forecast> TYPE ts_forecast.

    SORT mt_some_forecast BY matnr werks
                             budat DESCENDING.

    LOOP AT mt_some_forecast ASSIGNING <fs_some_forecast>.
      INSERT <fs_some_forecast> INTO TABLE lt_tab_with_max_date.
    ENDLOOP.
  ENDMETHOD.

  METHOD _read_max_group_by.
    DATA lt_tab_with_max_date TYPE tt_forecast.

    FIELD-SYMBOLS <fs_some_forecast> TYPE ts_forecast.
    FIELD-SYMBOLS <fs_with_max_date> TYPE ts_forecast.
    FIELD-SYMBOLS <fs_some_grp> TYPE ts_forecast.

    SORT mt_some_forecast BY matnr werks
                             budat DESCENDING.

    " this is a quite overhead approach in your case

    LOOP AT mt_some_forecast ASSIGNING <fs_some_grp> " it is group- pseudo-oboject - not LINE, but GROUP
          GROUP BY ( key1 = <fs_some_grp>-matnr  key2 = <fs_some_grp>-werks ). " you are providing keys

     " LOOP AT GROUP <fs_some_grp> ASSIGNING <fs_some_forecast>. " here we get LINE from GROUP
        READ TABLE mt_some_forecast ASSIGNING <fs_some_forecast>
            WITH KEY matnr = <fs_some_grp>-matnr
                     werks = <fs_some_grp>-werks.
        IF sy-subrc EQ 0.
          APPEND <fs_some_forecast> TO lt_tab_with_max_date.
        ENDIF.
      "ENDLOOP.

    ENDLOOP.


  ENDMETHOD.

  METHOD _read_max_group_by_no_membr.
    DATA lt_tab_with_max_date TYPE tt_forecast.

    TYPES: BEGIN OF ts_key_line
           , matnr TYPE matnr18
            , werks TYPE werks_d
        , END OF ts_key_line
        .
    FIELD-SYMBOLS <fs_some_forecast> TYPE ts_forecast.
    FIELD-SYMBOLS <fs_with_max_date> TYPE ts_forecast.
    FIELD-SYMBOLS <fs_some_grp> TYPE ts_forecast.

    FIELD-SYMBOLS <fs_key_line> TYPE ts_key_line.

    CLEAR lt_tab_with_max_date.

    SORT mt_some_forecast BY matnr werks
                             budat DESCENDING.

    """ IF need oly keys - you can use WITHOUT MEMBERS

    LOOP AT mt_some_forecast ASSIGNING <fs_some_grp> " it is group- pseudo-oboject - not LINE, but GROUP
          GROUP BY ( matnr = <fs_some_grp>-matnr werks = <fs_some_grp>-werks )  " you are providing keys
          WITHOUT MEMBERS " and here we get only keys without addition fields
      ASSIGNING <fs_key_line>. " in case without membes we could not LOOP at GROUP

      READ TABLE mt_some_forecast ASSIGNING <fs_with_max_date>
            WITH KEY matnr = <fs_key_line>-matnr
                     werks = <fs_key_line>-werks.
      IF sy-subrc EQ 0.
        APPEND INITIAL LINE TO lt_tab_with_max_date ASSIGNING <fs_some_forecast> .
        MOVE-CORRESPONDING <fs_with_max_date> TO <fs_some_forecast>.
      ENDIF.


    ENDLOOP.
  ENDMETHOD.

  METHOD _do_load_prefill.
    DATA lv_ts TYPE timestamp.
    GET TIME STAMP FIELD lv_ts.

    mt_some_forecast = VALUE #(
     ( budat = sy-datum - 20 timestamp = lv_ts vtstamp = lv_ts
      racct = '22000' matnr = '2400011' werks = 'D001' lbkum = 10 msl = 3 poper = 4 gjahr = 2025 )

     ( budat = sy-datum - 20 timestamp = lv_ts vtstamp = lv_ts
      racct = '22000' matnr = '2400011' werks = 'DK02' lbkum = 10 msl = 3 poper = 4 gjahr = 2025 )

     ( budat = sy-datum - 20 timestamp = lv_ts vtstamp = lv_ts
      racct = '22000' matnr = '2400011' werks = 'D001' lbkum = 10 msl = 3 poper = 4 gjahr = 2025 )

     ( budat = sy-datum - 30 timestamp = lv_ts vtstamp = lv_ts
      racct = '22000' matnr = '2400011' werks = 'DK02' lbkum = 10 msl = 3 poper = 4 gjahr = 2025 )

     ( budat = sy-datum - 30 timestamp = lv_ts vtstamp = lv_ts
      racct = '22000' matnr = '2400011' werks = 'D001' lbkum = 10 msl = 3 poper = 4 gjahr = 2025 )

     ( budat = sy-datum - 30 timestamp = lv_ts vtstamp = lv_ts
      racct = '22000' matnr = '2400011' werks = 'D001' lbkum = 10 msl = 3 poper = 4 gjahr = 2025 )
    ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW lcl_app( )->main( ).
