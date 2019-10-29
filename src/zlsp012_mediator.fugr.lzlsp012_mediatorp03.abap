*&---------------------------------------------------------------------*
*& Include          LZLSP012_MEDIATORP03
*&---------------------------------------------------------------------*

CLASS lcl_colleague_total_stock IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mo_mediator ?= io_mediator.


    me->set_meta(
      EXPORTING
        iv_cont_name = 'ZBL'
        iv_title     = 'Общие запасы: РЦ(резерв) и продажи'
        iv_func_name = 'Списать все для инвентаризации'
    ).

    mt_stock = VALUE #(
    ( matnr = '789100' matnr_tx = 'Молоток'
        menge_st01 = '20' meins_st01 = 'ST' menge_stdl = '10' meins_stdl = 'ST' )
    ( matnr = '789120' matnr_tx = 'Гвозди'
        menge_st01 = '21' meins_st01 = 'ST' menge_stdl = '40' meins_stdl = 'ST' )
    ( matnr = '789140' matnr_tx = 'Шурупы'
        menge_st01 = '22' meins_st01 = 'ST' menge_stdl = '31' meins_stdl = 'ST' )
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
    FIELD-SYMBOLS <fs_stock> TYPE ts_stock.

    READ TABLE mt_stock ASSIGNING <fs_stock>
      WITH KEY matnr = is_stock-matnr.
    IF sy-subrc EQ 0.
      CASE iv_action.
        WHEN '-'.
          <fs_stock>-menge_st01 =
          <fs_stock>-menge_st01 - is_stock-menge_st01.
          <fs_stock>-menge_stdl =
          <fs_stock>-menge_stdl - is_stock-menge_stdl.
        WHEN '+'.
          <fs_stock>-menge_stdl =
           <fs_stock>-menge_stdl + is_stock-menge_stdl.
        WHEN '*'.
          <fs_stock>-menge_st01 =
          <fs_stock>-menge_st01 - is_stock-menge_st01.
          <fs_stock>-menge_stdl =
          <fs_stock>-menge_stdl + is_stock-menge_stdl.
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
