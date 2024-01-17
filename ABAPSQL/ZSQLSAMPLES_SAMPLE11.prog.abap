*&---------------------------------------------------------------------*
*& Report ZSQLSAMPLES_SAMPLE11
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsqlsamples_sample11.

PARAMETERS: p_break AS CHECKBOX DEFAULT ' '.

CLASS lcl_app DEFINITION.

  PUBLIC SECTION.
    METHODS sh.

  PRIVATE SECTION.
    TYPES: BEGIN OF ts_res
        , id_num TYPE zsls_ord_h-id_num
        , item_num TYPE zsls_ord_i-item_num
        , partner_name TYPE zpartner_h-partner_name
        , total_max TYPE zsls_ord_i-total
        , total_min TYPE zsls_ord_i-total
      , END OF ts_res
      , tt_res TYPE STANDARD TABLE OF ts_res WITH DEFAULT KEY
      .

    METHODS _prepare_tabs.

    METHODS _show_slice_with_max_total_v1.
    METHODS _show_slice_with_max_total_v2.
    METHODS _show.

ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD sh.
    _prepare_tabs( ).

    _show_slice_with_max_total_v1( ).

    _show_slice_with_max_total_v2( ).

    _show( ).
  ENDMETHOD.

  METHOD _prepare_tabs.

    DATA lt_ord_h TYPE STANDARD TABLE OF zsls_ord_h.
    DATA lt_ord_i TYPE STANDARD TABLE OF zsls_ord_i.
    DATA lt_partner_h TYPE STANDARD TABLE OF zpartner_h.

    lt_ord_h = VALUE #(
    ( id_num = '1001' partner = '2001' num_ext = 'EXT_NUM_FOR_1001' )
    ( id_num = '1002' partner = '2002' num_ext = 'EXT_NUM_FOR_1002' )
    ( id_num = '1003' partner = '2003' num_ext = 'EXT_NUM_FOR_1003' ) ).

    lt_ord_i = VALUE #(
    ( id_num = '1001' item_num = '1' prod_id = 'PRODUCT_91' price = '20'
        quan = 5  uom = 'PC' total = 100 waers = 'RUB' )
    ( id_num = '1001' item_num = '2' prod_id = 'PRODUCT_92' price = '25'
        quan = 6  uom = 'PC' total = 150 waers = 'RUB' )
    ( id_num = '1001' item_num = '3' prod_id = 'PRODUCT_93' price = '15'
        quan = 7  uom = 'PC' total = 105 waers = 'RUB' )

    ( id_num = '1002' item_num = '1' prod_id = 'PRODUCT_81' price = '20'
        quan = 3  uom = 'PC' total = 60 waers = 'RUB' )
    ( id_num = '1002' item_num = '2' prod_id = 'PRODUCT_82' price = '30'
        quan = 8  uom = 'PC' total = 240 waers = 'RUB' )
    ( id_num = '1002' item_num = '3' prod_id = 'PRODUCT_83' price = '40'
        quan = 4  uom = 'PC' total = 120 waers = 'RUB' )

    ( id_num = '1003' item_num = '1' prod_id = 'PRODUCT_71' price = '33'
        quan = 7  uom = 'PC' total = 231 waers = 'RUB' )
    ( id_num = '1003' item_num = '2' prod_id = 'PRODUCT_72' price = '23'
        quan = 15  uom = 'PC' total = 345 waers = 'RUB' )
    ).

    lt_partner_h = VALUE #(
     ( partner = '2001' partner_name = 'ООО Полезное оборудование' )
     ( partner = '2002' partner_name = 'ЗАО Тюльпанчик' )
     ( partner = '2003' partner_name = 'ПАО Простоцарский банк' )
    ).

    MODIFY zsls_ord_h FROM TABLE lt_ord_h.
    MODIFY zsls_ord_i FROM TABLE lt_ord_i.
    MODIFY zpartner_h FROM TABLE lt_partner_h.

    COMMIT WORK.

  ENDMETHOD.

  METHOD _show_slice_with_max_total_v1.

    DATA lt_res TYPE tt_res.

    " доступно с 4.7
    " https://help.sap.com/saphelp_470/helpdata/EN/fc/eb39c4358411d1829f0000e829fbfe/frameset.htm
    " в подзапросе возвращается скалярное выражение и
    " и его используем как значение для общего JOIN
    SELECT h~id_num  imax~item_num k~partner_name imax~total as total_max imin~total as total_min
      FROM zsls_ord_i AS imax JOIN zsls_ord_h AS h ON imax~id_num EQ h~id_num
       join zsls_ord_i AS imin ON imin~id_num EQ imax~id_num
        JOIN zpartner_h AS k ON h~partner EQ k~partner
      INTO TABLE lt_res
      WHERE
       imax~total EQ (
        SELECT MAX( total ) FROM zsls_ord_i WHERE id_num EQ h~id_num
      )
      and
      imin~total EQ (
        SELECT MIN( total ) FROM zsls_ord_i WHERE id_num EQ h~id_num
      )
      ORDER BY h~id_num  imax~item_num
      .

    zcl_c8a014_show_html=>get_instance( )->add_para_val_ch(
        iv_id    = 'Vers' iv_value = 'Up to 752 (скалярное выражение через подзапрос)'
    )->add_tab_ch( it_tab = lt_res  ).

  ENDMETHOD.

  METHOD _show_slice_with_max_total_v2.
    DATA lt_res TYPE tt_res.

    " Используем CTE для формирования подзапросов
    " https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/index.htm?file=abapwith_subquery.htm
    WITH +ord_item_subtot AS (
      SELECT h~id_num AS ord_num, MAX( i~total ) AS maxtotal_item, MIN( i~total ) AS mintotal_item
        FROM zsls_ord_h AS h JOIN zsls_ord_i AS i ON h~id_num EQ i~id_num
        GROUP BY h~id_num
    )

    SELECT h~id_num, i~item_num, k~partner_name, subtot~maxtotal_item, subtot~mintotal_item
      FROM zsls_ord_i AS i JOIN zsls_ord_h AS h
        ON i~id_num EQ h~id_num
        JOIN +ord_item_subtot AS subtot ON i~id_num EQ subtot~ord_num and i~total = subtot~maxtotal_item
        JOIN zpartner_h AS k ON h~partner EQ k~partner
      ORDER BY h~id_num
      INTO TABLE @lt_res
      .

        zcl_c8a014_show_html=>get_instance( )->add_para_val_ch(
        iv_id    = 'Vers' iv_value = 'From 752 (подзапрос через CTE)'
    )->add_tab_ch( it_tab = lt_res  ).

  ENDMETHOD.

  METHOD _show.
    zcl_c8a014_show_html=>get_instance( )->show( ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  IF p_break EQ abap_true.
    BREAK-POINT.
  ENDIF.
  NEW lcl_app( )->sh( ).
