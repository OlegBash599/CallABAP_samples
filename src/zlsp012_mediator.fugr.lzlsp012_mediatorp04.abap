*&---------------------------------------------------------------------*
*& Include          LZLSP012_MEDIATORP04
*&---------------------------------------------------------------------*

CLASS lcl_colleague_sales IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mo_mediator ?= io_mediator.


    me->set_meta(
      EXPORTING
        iv_cont_name = 'ZUR'
        iv_title     = 'Запасы на складе STSL/продажи'
        iv_func_name = 'Отгрузить со склада'
    ).

    mt_stock = VALUE #(
    ( customer = '1001' customer_tx = 'Клиент важный' delivery_date = ( sy-datum + 10 )
         matnr = '789100' matnr_tx = 'Молоток' menge = '10' meins = 'ST'  )
    ( customer = '1002' customer_tx = 'Клиент очень важный' delivery_date = ( sy-datum + 15 )
         matnr = '789120' matnr_tx = 'Гвозди' menge = '40' meins = 'ST'  )
    ( customer = '1002' customer_tx = 'Клиент вообще важный' delivery_date = ( sy-datum + 20 )
         matnr = '789140' matnr_tx = 'Шурупы' menge = '31' meins = 'ST'  )
    ).


  ENDMETHOD.

  METHOD create_alv.
*... §2 create an alv table
    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container    = mo_container
            container_name = me->mv_cont_name
          IMPORTING
            r_salv_table   = mo_alv_table
          CHANGING
            t_table        = mt_stock ).
      CATCH cx_salv_msg.                                "#EC NO_HANDLER
    ENDTRY.
  ENDMETHOD.

  METHOD get_sel_line.
    rs_sel = ms_stock.
    CLEAR ms_stock.
  ENDMETHOD.


  METHOD process_stock.
*      IMPORTING iv_action     TYPE char1 DEFAULT '-'
*                is_cons_stock TYPE ts_cons_stock.
    FIELD-SYMBOLS <fs_stock> TYPE ts_stock.

    READ TABLE mt_stock ASSIGNING <fs_stock>
      WITH KEY matnr = is_stock-matnr.
    IF sy-subrc EQ 0.
      CASE iv_action.
        WHEN '-'.
          <fs_stock>-menge =
          <fs_stock>-menge - is_stock-menge.
        WHEN '+'.
          <fs_stock>-menge =
           <fs_stock>-menge + is_stock-menge.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.
  ENDMETHOD.


  METHOD on_user_command.
    " move all out
  ENDMETHOD.                    "on_user_command


  METHOD handle_double_click.

  ENDMETHOD.

  METHOD  on_link_click.
    FIELD-SYMBOLS <fs_stock> TYPE ts_stock.
    CLEAR ms_stock.
    READ TABLE mt_stock ASSIGNING <fs_stock> INDEX row.
    IF sy-subrc EQ 0.
      ms_stock = <fs_stock>.

      me->mo_mediator->notify( io_obj = me ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
