*&---------------------------------------------------------------------*
*& Include          LZLSP012_MEDIATORP02
*&---------------------------------------------------------------------*

CLASS lcl_colleague_consignment IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mo_mediator ?= io_mediator.


    me->set_meta(
      EXPORTING
        iv_cont_name = 'ZUL'
        iv_title     = 'Запаса склада ST01/резерв РЦ'
        iv_func_name = 'Списать все со склада ST01'
    ).

    mt_cons_stock = VALUE #(
    ( matnr = '789100' matnr_tx = 'Молоток' menge = '20' meins = 'ST' )
    ( matnr = '789120' matnr_tx = 'Гвозди' menge = '21' meins = 'ST' )
    ( matnr = '789140' matnr_tx = 'Шурупы' menge = '22' meins = 'ST' )
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
            t_table        = mt_cons_stock ).
      CATCH cx_salv_msg.                                "#EC NO_HANDLER
    ENDTRY.
  ENDMETHOD.

  METHOD get_sel_line.
    "      RETURNING VALUE(rs_sel) TYPE ts_cons_stock.
    rs_sel = ms_cons_stock.
    CLEAR ms_cons_stock.
  ENDMETHOD.


  METHOD process_stock.
*      IMPORTING iv_action     TYPE char1 DEFAULT '-'
*                is_cons_stock TYPE ts_cons_stock.
    FIELD-SYMBOLS <fs_cons_stock> TYPE ts_cons_stock.

    READ TABLE mt_cons_stock ASSIGNING <fs_cons_stock>
      WITH KEY matnr = is_cons_stock-matnr.
    IF sy-subrc EQ 0.
      CASE iv_action.
        WHEN '-'.
          <fs_cons_stock>-menge =
          <fs_cons_stock>-menge - is_cons_stock-menge.
        WHEN '+'.
          <fs_cons_stock>-menge =
           <fs_cons_stock>-menge + is_cons_stock-menge.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.



  ENDMETHOD.


  METHOD on_user_command.
    DATA lr_sels TYPE REF TO cl_salv_selections.
    lr_sels = mo_alv_table->get_selections( ).
    BREAK-POINT.
  ENDMETHOD.                    "on_user_command


  METHOD handle_double_click.

  ENDMETHOD.

  METHOD  on_link_click.
    FIELD-SYMBOLS <fs_cons_stock> TYPE ts_cons_stock.
    CLEAR ms_cons_stock.
    READ TABLE mt_cons_stock ASSIGNING <fs_cons_stock> INDEX row.
    IF sy-subrc EQ 0.
      ms_cons_stock = <fs_cons_stock>.
      me->mo_mediator->notify( io_obj = me ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
