*&---------------------------------------------------------------------*
*& Include          LZLSP012_MEDIATORP06
*&---------------------------------------------------------------------*

CLASS lcl_mediator_director IMPLEMENTATION.
  METHOD constructor.
    lo_colleague_consignment = NEW #( me ).
    lo_colleague_sales = NEW #( me ).
    lo_colleague_total_stock = NEW #( me ).

    lo_colleague_log = NEW #( me ).

  ENDMETHOD.

  METHOD fill_blocks.
    lo_colleague_consignment->fill_block( ).
    lo_colleague_sales->fill_block( ).
    lo_colleague_total_stock->fill_block( ).

    lo_colleague_log->fill_block( ).
  ENDMETHOD.

  METHOD lif_mediator~notify.
    CASE TYPE OF io_obj.
      WHEN TYPE lcl_colleague_consignment.
        move2sales( ).
        write2log( iv_src = 'consignment'
                   iv_line = 'Движение из резерва в продажи' ).
        write2log( iv_src = 'consignment2Total'
                   iv_line = 'Обноление общих запасов' ).
        refresh( iv_cons = abap_true
                 iv_sales = abap_true
                 iv_total = abap_true
                 iv_log = abap_true ).
      WHEN TYPE lcl_colleague_sales.
        moveoutsales( ).
        write2log( iv_src = 'sales'
                   iv_line = 'Доставка клиенту' ).
        write2log( iv_src = 'sales2Total'
                   iv_line = 'Обноление общих запасов' ).
        refresh( iv_cons = abap_false
                 iv_sales = abap_true
                 iv_total = abap_true
                 iv_log = abap_true ).


      WHEN TYPE lcl_colleague_total_stock.

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD write2log.
    lo_colleague_log->add_line2log(
          EXPORTING
            iv_src  = iv_src
            iv_line = iv_line
        ).
  ENDMETHOD.

  METHOD move2sales.
    "      IMPORTING io_obj TYPE REF TO object.
    DATA ls_stock_from TYPE lcl_colleague_consignment=>ts_cons_stock.
    DATA ls_stock_to TYPE lcl_colleague_sales=>ts_stock.
    DATA ls_stock_total TYPE lcl_colleague_total_stock=>ts_stock.

    ls_stock_from = lo_colleague_consignment->get_sel_line( ).
    MOVE-CORRESPONDING ls_stock_from TO ls_stock_to.

    lo_colleague_consignment->process_stock(
      EXPORTING
        iv_action = '-'
        is_cons_stock  = ls_stock_from
    ).

    lo_colleague_sales->process_stock(
      EXPORTING
        iv_action = '+'
        is_stock  = ls_stock_to
    ).

    ls_stock_total-matnr = ls_stock_from-matnr.
    ls_stock_total-menge_st01 = ls_stock_from-menge.
    ls_stock_total-menge_stdl = ls_stock_to-menge.

    lo_colleague_total_stock->process_stock(
      EXPORTING
        iv_action = '*'
        is_stock  = ls_stock_total
    ).

  ENDMETHOD.

  METHOD moveoutsales.

    DATA ls_stock_from TYPE lcl_colleague_sales=>ts_stock.
    DATA ls_stock_total TYPE lcl_colleague_total_stock=>ts_stock.

    ls_stock_from = lo_colleague_sales->get_sel_line( ).
    MOVE-CORRESPONDING ls_stock_from TO ls_stock_total.
    ls_stock_total-menge_stdl = ls_stock_from-menge.
    CLEAR ls_stock_total-menge_st01.

    lo_colleague_sales->process_stock( EXPORTING
        iv_action = '-'
        is_stock  = ls_stock_from ).


    lo_colleague_total_stock->process_stock(
      EXPORTING
        iv_action = '-'
        is_stock  = ls_stock_total
    ).

  ENDMETHOD.

  METHOD refresh.
*      IMPORTING iv_cons  TYPE char1
*                iv_sales TYPE char1
*                iv_total TYPE char1
*                iv_log   TYPE char1.
*
    IF iv_cons EQ abap_true.
      lo_colleague_consignment->do_refresh( ).
    ENDIF.

    IF iv_sales EQ abap_true.
      lo_colleague_sales->do_refresh( ).
    ENDIF.

    IF iv_total EQ abap_true.
      lo_colleague_total_stock->do_refresh( ).
    ENDIF.

    IF iv_log EQ abap_true.
      lo_colleague_log->do_refresh( ).
    ENDIF.


  ENDMETHOD.
ENDCLASS.
