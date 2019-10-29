*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW04_CLS19
*&---------------------------------------------------------------------*

CLASS lcl_state_context DEFINITION DEFERRED.

INTERFACE lif_state.

  METHODS set_new_order_data
    IMPORTING it_order_data   TYPE mestringmap
              ir_context_data TYPE REF TO mestringmap .

  METHODS set_next_state.
  METHODS set_context
    IMPORTING io_context TYPE REF TO lcl_state_context.

ENDINTERFACE.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

CLASS lcl_state_init DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_state.
    METHODS constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_context TYPE REF TO lcl_state_context.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_state_context DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_status TYPE char4 DEFAULT 'INIT'.

    " INIT / EDIT / SIGN

    METHODS set_state
      IMPORTING iv_status TYPE char4 DEFAULT 'INIT'.

    METHODS set_new_order_data
      IMPORTING it_order_data TYPE mestringmap.

    METHODS get_order_data
      RETURNING VALUE(rt_val) TYPE mestringmap.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_state TYPE REF TO lif_state.
    DATA mr_order_data TYPE REF TO mestringmap.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


CLASS lcl_state_init IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.

  METHOD lif_state~set_new_order_data.
    ir_context_data->* = it_order_data.
  ENDMETHOD.

  METHOD lif_state~set_next_state.
    mo_context->set_state( iv_status = 'EDIT' ).
  ENDMETHOD.

  METHOD lif_state~set_context.
    mo_context ?= io_context.
  ENDMETHOD.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

CLASS lcl_state_edit DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_state.
    METHODS constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_context TYPE REF TO lcl_state_context.

ENDCLASS.

CLASS lcl_state_edit IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.

  METHOD lif_state~set_new_order_data.

    DATA lr_order_data_in TYPE REF TO menamevalue.
    DATA lr_order_data_target TYPE REF TO menamevalue.

    LOOP AT it_order_data REFERENCE INTO lr_order_data_in.

      CASE lr_order_data_in->name.
        WHEN 'CUSTOMER' OR 'PURCH_ORDER' OR 'WAERK'.
          zcl_lsp012_html=>get_instance(
*        iv_title =
                    )->add_para_val(
                      EXPORTING
                        iv_id    = 'Message'
                        iv_value = |No change for field { lr_order_data_in->name } in status EDIT|
            ).
          CONTINUE.
        WHEN OTHERS.
          READ TABLE ir_context_data->* REFERENCE INTO lr_order_data_target
            WITH KEY name = lr_order_data_in->name.
          IF sy-subrc EQ 0.
            lr_order_data_target->value = lr_order_data_in->value.
          ELSE.
            INSERT lr_order_data_in->* INTO TABLE ir_context_data->*.
          ENDIF.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD lif_state~set_next_state.
    mo_context->set_state( iv_status = 'SIGN' ).
  ENDMETHOD.

  METHOD lif_state~set_context.
    mo_context ?= io_context.
  ENDMETHOD.


ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

CLASS lcl_state_sign DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_state.
    METHODS constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_context TYPE REF TO lcl_state_context.

ENDCLASS.

CLASS lcl_state_sign IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.

  METHOD lif_state~set_new_order_data.

  ENDMETHOD.

  METHOD lif_state~set_next_state.
    " no next state
    zcl_lsp012_html=>get_instance(
*        iv_title =
    )->add_para_val(
      EXPORTING
        iv_id    = 'Message'
        iv_value = 'SIGN is final state'
    ).
  ENDMETHOD.

  METHOD lif_state~set_context.
    mo_context ?= io_context.
  ENDMETHOD.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


CLASS lcl_state_context IMPLEMENTATION.
  METHOD constructor.
    CREATE DATA mr_order_data.
    set_state( iv_status ).
  ENDMETHOD.

  METHOD set_state.
    DATA(lv_cls_name) = CONV seoclsname( `LCL_STATE_` && iv_status ).
    CREATE OBJECT mo_state TYPE (lv_cls_name).
    mo_state->set_context( io_context = me ).
  ENDMETHOD.

  METHOD set_new_order_data.
    DATA lr_order_data TYPE REF TO menamevalue.
    mo_state->set_new_order_data( it_order_data = it_order_data
                                  ir_context_data = mr_order_data ).

    LOOP AT it_order_data REFERENCE INTO lr_order_data WHERE name EQ 'SET_NEXT'.
      IF lr_order_data->value EQ abap_true.
        mo_state->set_next_state( ).
      ENDIF.
      EXIT.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_order_data.
    "        RETURNING VALUE(rt_val) TYPE mestringmap.
    rt_val = mr_order_data->*.
  ENDMETHOD.

ENDCLASS.


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

CLASS lcl_state DEFINITION.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS main.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: tt_stand_menamevalue TYPE STANDARD TABLE OF menamevalue WITH DEFAULT KEY.
    DATA mo_context TYPE REF TO lcl_state_context.

    METHODS run_step1.
    METHODS run_step2.
    METHODS run_step3.


ENDCLASS.

CLASS lcl_state IMPLEMENTATION.

  METHOD constructor.

    mo_context = NEW lcl_state_context( ).

  ENDMETHOD.                    "constructor

  METHOD main.
    run_step1( ).
    run_step2( ).
    run_step3( ).
  ENDMETHOD.                    "main

  METHOD run_step1.

    mo_context->set_new_order_data( it_order_data = VALUE mestringmap(
     ( name = 'CUSTOMER' value = '105020') ( name = 'PURCH_ORDER' value = '741258')
     ( name = 'DELIVERY_DATE' value = '20201010') ( name = 'TOTAL_AMOUNT' value = '10000') ( name = 'WAERK' value = 'RUB')
     ( name = 'ITEM1' value = '147') ( name = 'ITEM2' value = '150') ( name = 'ITEM3' value = '160')
     ) ).


    mo_context->set_new_order_data( it_order_data = VALUE mestringmap(
     ( name = 'CUSTOMER' value = '105020') ( name = 'PURCH_ORDER' value = '741258_55')
     ( name = 'DELIVERY_DATE' value = '20191201') ( name = 'TOTAL_AMOUNT' value = '30000') ( name = 'WAERK' value = 'RUB')
     ( name = 'ITEM1' value = '147') ( name = 'ITEM2' value = '150') ( name = 'ITEM3' value = '160')
     ( name = 'SET_NEXT' value = abap_true )
     ) ).

    zcl_lsp012_html=>get_instance( )->add_para_val_ch( iv_id = 'STEP1' iv_value = 'STEP1_data'
        )->add_tab( it_tab = CONV tt_stand_menamevalue( mo_context->get_order_data( ) ) ).

  ENDMETHOD.                    "run_step1


  METHOD run_step2.

    mo_context->set_new_order_data( it_order_data = VALUE mestringmap(
     ( name = 'CUSTOMER' value = '105999') ( name = 'PURCH_ORDER' value = '741258_99')
     ( name = 'DELIVERY_DATE' value = '20191201') ( name = 'TOTAL_AMOUNT' value = '30000') ( name = 'WAERK' value = 'RUB')
     ( name = 'ITEM1' value = '147') ( name = 'ITEM2' value = '150') ( name = 'ITEM3' value = '160')
     ) ).

    mo_context->set_new_order_data( it_order_data = VALUE mestringmap(
     ( name = 'CUSTOMER' value = '105999') ( name = 'PURCH_ORDER' value = '741258_99')
     ( name = 'DELIVERY_DATE' value = '20191201') ( name = 'TOTAL_AMOUNT' value = '50000') ( name = 'WAERK' value = 'RUB')
     ( name = 'ITEM1' value = '210') ( name = 'ITEM2' value = '150') ( name = 'ITEM3' value = '160')
     ( name = 'NOTE' value = 'Отправить морским транспортом' )
     ( name = 'SET_NEXT' value = abap_true )
     ) ).

    zcl_lsp012_html=>get_instance( )->add_para_val_ch( iv_id = 'STEP2' iv_value = 'STEP2_data'
        )->add_tab( it_tab = CONV tt_stand_menamevalue( mo_context->get_order_data( ) ) ).

    mo_context->set_new_order_data( it_order_data = VALUE mestringmap(
    ( name = 'CUSTOMER' value = '105999') ( name = 'PURCH_ORDER' value = '741258_99')
    ( name = 'DELIVERY_DATE' value = '20191201') ( name = 'TOTAL_AMOUNT' value = '50000') ( name = 'WAERK' value = 'RUB')
    ( name = 'ITEM1' value = '210') ( name = 'ITEM2' value = '150') ( name = 'ITEM3' value = '160')
    ( name = 'NOTE' value = 'Отправить морским транспортом' )
    ( name = 'SET_NEXT' value = abap_true )
    ) ).

  ENDMETHOD.

  METHOD run_step3.

    mo_context->set_state( iv_status = 'INIT' ).

    mo_context->set_new_order_data( it_order_data = VALUE mestringmap(
     ( name = 'CUSTOMER' value = '105020') ( name = 'PURCH_ORDER' value = '741258_55')
     ( name = 'DELIVERY_DATE' value = '20191220') ( name = 'TOTAL_AMOUNT' value = '50000') ( name = 'WAERK' value = 'RUB')
     ( name = 'ITEM1' value = '147') ( name = 'ITEM2' value = '150') ( name = 'ITEM3' value = '160')
     ( name = 'SET_NEXT' value = abap_true )
     ) ).

    zcl_lsp012_html=>get_instance( 'State results' )->add_para_val_ch( iv_id = 'STEP4' iv_value = 'STEP4_init_after_sign'
        )->add_tab_ch( it_tab = CONV tt_stand_menamevalue( mo_context->get_order_data( ) ) )->show( ).


  ENDMETHOD.

ENDCLASS.
