*&---------------------------------------------------------------------*
*& Report ZREP_C8A014Q03_CDS_SUM
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrep_c8a014q03_cds_sum.

DATA gs_scr_zmard TYPE zmard.
PARAMETERS: p_mode TYPE char1 DEFAULT 'W'.

SELECT-OPTIONS s_lgort FOR gs_scr_zmard-lgort.

CLASS lcl_app DEFINITION.

  PUBLIC SECTION.
    METHODS fn.
  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES: BEGIN OF ts_matnr_werks
            , matnr TYPE matnr18
            , werks TYPE werks_d
          , END OF ts_matnr_werks
          , tt_matnr_werks TYPE STANDARD TABLE OF ts_matnr_werks WITH DEFAULT KEY
          .

    DATA mt_tab_matnr TYPE tt_matnr_werks.

    METHODS _do_load_prefill.

    METHODS _sel_one_no_sum_by_lgort.
    METHODS _sel_one_no_sum_fld_lgort.
    METHODS _sel_one_no_sum_join_lgort.
    METHODS _sel_one_no_sum_lgort.
    METHODS _sel_one_cds4_all.
    METHODS _sel_one_cds4_all_matnr.
    METHODS _sel_one_cds4_gtt_join.

ENDCLASS.


CLASS lcl_app IMPLEMENTATION.
  METHOD fn.



    IF p_mode EQ 'L'.
      _do_load_prefill( ).
    ENDIF.

    mt_tab_matnr = VALUE #(
    ( matnr = 'M1001' werks = 'W001' )
    ( matnr = 'M1002' werks = 'W001' )
    ).

    BREAK-POINT.
    """""""""""""""""""""""""""""""""""""
    _sel_one_no_sum_by_lgort( ).
    _sel_one_no_sum_fld_lgort( ).
    _sel_one_no_sum_join_lgort( ).
    _sel_one_no_sum_lgort( ).
    _sel_one_cds4_all_matnr( ).
    _sel_one_cds4_gtt_join( ).
    """""""""""""""""""""""""""""""""""""


  ENDMETHOD.

  METHOD _sel_one_no_sum_by_lgort.
*    define view ZMARD_CDS2_W as select from ZMARD_CDS1_M
*{
*    key matnr,
*    sum(labst) as stock
*} group by matnr

    SELECT zcds1~matnr,
              zcds2~stock
    FROM zmard_cds1_m AS zcds1
    INNER JOIN zmard_cds2_w AS zcds2 ON zcds2~matnr = zcds1~matnr
    FOR ALL ENTRIES IN @mt_tab_matnr
         WHERE zcds1~matnr = @mt_tab_matnr-matnr
           AND zcds1~werks = @mt_tab_matnr-werks
           AND zcds1~lgort IN @s_lgort
    INTO TABLE @DATA(lt_stock).
  ENDMETHOD.

  METHOD _sel_one_no_sum_fld_lgort.
    SELECT zcds1~matnr,
            zcds1~werks,
            zcds1~lgort,
            zcds2~stock
  FROM zmard_cds1_m AS zcds1
  INNER JOIN zmard_cds2_w AS zcds2 ON zcds2~matnr = zcds1~matnr
  FOR ALL ENTRIES IN @mt_tab_matnr
       WHERE zcds1~matnr = @mt_tab_matnr-matnr
         AND zcds1~werks = @mt_tab_matnr-werks
         AND zcds1~lgort IN @s_lgort
  INTO TABLE @DATA(lt_stock).
  ENDMETHOD.

  METHOD _sel_one_no_sum_join_lgort.

  ENDMETHOD.

  METHOD _sel_one_no_sum_lgort.

*  define view ZMARD_CDS3_W as select from ZMARD_CDS1_M
*{
*    key matnr,
*    key lgort,
*    sum(labst) as stock
*} group by matnr, lgort


    SELECT zcds1~matnr,
              zcds2~stock
    FROM zmard_cds1_m AS zcds1
    INNER JOIN zmard_cds3_w AS zcds2 ON zcds2~matnr = zcds1~matnr
    FOR ALL ENTRIES IN @mt_tab_matnr
         WHERE zcds1~matnr = @mt_tab_matnr-matnr
           AND zcds1~werks = @mt_tab_matnr-werks
           AND zcds1~lgort IN @s_lgort
    INTO TABLE @DATA(lt_stock).
  ENDMETHOD.

  METHOD _sel_one_cds4_all.
*define view ZMARD_CDS4_W as select from ZMARD_CDS1_M
*{
*    key matnr,
*    key werks,
*    key lgort,
*    sum(labst) as stock
*} group by matnr, werks, lgort

    SELECT matnr, stock
    FROM zmard_cds4_w
    FOR ALL ENTRIES IN @mt_tab_matnr
         WHERE matnr = @mt_tab_matnr-matnr
           AND werks = @mt_tab_matnr-werks
           AND lgort IN @s_lgort
    INTO TABLE @DATA(lt_stock).

  ENDMETHOD.

  METHOD _sel_one_cds4_all_matnr.

    TYPES: BEGIN OF ts_matnr_werks
        , matnr TYPE matnr18
        , werks TYPE werks_d
      , END OF ts_matnr_werks
      , tt_matnr_werks_srt TYPE SORTED TABLE OF ts_matnr_werks WITH UNIQUE KEY matnr werks
      .

    DATA lt_tab_matnr_srt TYPE tt_matnr_werks_srt.
    FIELD-SYMBOLS <fs_tab_matnr> TYPE ts_matnr_werks.

    " just to prepare sorted table
    LOOP AT mt_tab_matnr ASSIGNING <fs_tab_matnr>.
      INSERT <fs_tab_matnr> INTO TABLE lt_tab_matnr_srt.
    ENDLOOP.

    SELECT z1~matnr, SUM( labst ) AS stock
        FROM zmard AS z1
          JOIN @lt_tab_matnr_srt AS z2
        ON z1~matnr EQ z2~matnr
        AND z1~werks EQ z2~werks
      WHERE z1~lgort IN @s_lgort
      GROUP BY z1~matnr
      INTO TABLE @DATA(lt_stock)
      .

  ENDMETHOD.

  METHOD _sel_one_cds4_gtt_join.
    " via GTT_like technique (if no such object)
    DATA lt_db_table4join TYPE SORTED TABLE OF zmatnr_werks_gtt WITH UNIQUE KEY matnr werks.
    DATA ls_line_like_fae TYPE zmatnr_werks_gtt.
    FIELD-SYMBOLS <fs_tab_matnr> TYPE ts_matnr_werks.

    LOOP AT mt_tab_matnr ASSIGNING <fs_tab_matnr>.
      ls_line_like_fae-matnr = <fs_tab_matnr>-matnr.
      ls_line_like_fae-werks = <fs_tab_matnr>-werks.
      INSERT ls_line_like_fae INTO TABLE lt_db_table4join.
    ENDLOOP.

    " clear defore join - or add guid
    DELETE FROM zmatnr_werks_gtt.
    INSERT zmatnr_werks_gtt FROM TABLE lt_db_table4join.
    " no commit in one session


    SELECT z1~matnr, SUM( labst ) AS stock
    FROM zmard AS z1
      JOIN zmatnr_werks_gtt AS z2
    ON z1~matnr EQ z2~matnr
    AND z1~werks EQ z2~werks
  WHERE z1~lgort IN @s_lgort
  GROUP BY z1~matnr
  INTO TABLE @DATA(lt_stock)
  .

    " clear after using
    DELETE FROM zmatnr_werks_gtt.

  ENDMETHOD.

  METHOD _do_load_prefill.
    DATA lt_zmard_sample TYPE STANDARD TABLE OF zmard WITH DEFAULT KEY.

    lt_zmard_sample = VALUE #(
    ( matnr = 'M1001' werks = 'W001' lgort = 'L101' labst = 11 meins = 'ST' )
    ( matnr = 'M1001' werks = 'W001' lgort = 'L102' labst = 11 meins = 'ST' )
    ( matnr = 'M1001' werks = 'W001' lgort = 'L103' labst = 11 meins = 'ST' )
    ( matnr = 'M1001' werks = 'W002' lgort = 'L201' labst = 12 meins = 'ST' )
    ( matnr = 'M1001' werks = 'W003' lgort = 'L301' labst = 13 meins = 'ST' )

    ( matnr = 'M1002' werks = 'W001' lgort = 'L101' labst = 21 meins = 'ST' )
    ( matnr = 'M1002' werks = 'W001' lgort = 'L102' labst = 21 meins = 'ST' )
    ( matnr = 'M1002' werks = 'W001' lgort = 'L103' labst = 21 meins = 'ST' )
    ( matnr = 'M1002' werks = 'W002' lgort = 'L201' labst = 22 meins = 'ST' )
    ( matnr = 'M1002' werks = 'W003' lgort = 'L301' labst = 23 meins = 'ST' )

    ( matnr = 'M1003' werks = 'W001' lgort = 'L101' labst = 31 meins = 'ST' )
    ( matnr = 'M1003' werks = 'W001' lgort = 'L102' labst = 31 meins = 'ST' )
    ( matnr = 'M1003' werks = 'W001' lgort = 'L103' labst = 31 meins = 'ST' )
    ( matnr = 'M1003' werks = 'W002' lgort = 'L201' labst = 32 meins = 'ST' )
    ( matnr = 'M1003' werks = 'W003' lgort = 'L301' labst = 33 meins = 'ST' )

    ( matnr = 'M1004' werks = 'W001' lgort = 'L101' labst = 31 meins = 'ST' )
    ( matnr = 'M1004' werks = 'W001' lgort = 'L102' labst = 31 meins = 'ST' )
    ( matnr = 'M1004' werks = 'W001' lgort = 'L103' labst = 31 meins = 'ST' )
    ( matnr = 'M1004' werks = 'W002' lgort = 'L201' labst = 32 meins = 'ST' )
    ( matnr = 'M1004' werks = 'W003' lgort = 'L301' labst = 33 meins = 'ST' )

    ).

    MODIFY zmard FROM TABLE lt_zmard_sample.
    COMMIT WORK AND WAIT.

  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.
  NEW lcl_app( )->fn( ).
