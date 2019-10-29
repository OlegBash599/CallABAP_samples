

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_order_data DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_order_info
                   , customer_type TYPE char4
                   , customer TYPE char10
                   , order_type TYPE char4
                   , order TYPE char10
                   , sales_org TYPE char4
                   , distr_channel TYPE char2
               , END OF ts_order_info
               , tt_order_info TYPE STANDARD TABLE OF ts_order_info WITH DEFAULT KEY
               .

    TYPES: BEGIN OF ts_email_list
                , email TYPE ad_smtpadr
            , END OF ts_email_list
            , tt_email_list TYPE STANDARD TABLE OF ts_email_list WITH DEFAULT KEY
            .

    METHODS constructor
      IMPORTING is_order_info TYPE ts_order_info.

    METHODS if_advertising_product
      RETURNING VALUE(rv_val) TYPE char1.

    METHODS get_order_info
      RETURNING VALUE(rs_val) TYPE ts_order_info.

    METHODS set_emails
      IMPORTING it_email_list TYPE tt_email_list OPTIONAL
                is_email_list TYPE ts_email_list OPTIONAL
      .

    METHODS get_emails
      RETURNING VALUE(rt_val) TYPE tt_email_list.

  PROTECTED SECTION.
    DATA ms_order_info TYPE ts_order_info.
  PRIVATE SECTION.

    DATA mt_email_list TYPE tt_email_list.


ENDCLASS.

CLASS lcl_order_data IMPLEMENTATION.
  METHOD constructor.
    ms_order_info = is_order_info.
  ENDMETHOD.

  METHOD if_advertising_product.
    rv_val = abap_true.
  ENDMETHOD.

  METHOD get_order_info.
    rs_val = ms_order_info.
  ENDMETHOD.

  METHOD set_emails.
*      IMPORTING it_email_list TYPE tt_email_list OPTIONAL
*                is_email_list TYPE ts_email_list OPTIONAL
*                .
    IF is_email_list IS SUPPLIED.
      APPEND is_email_list TO mt_email_list.
    ENDIF.

    IF it_email_list IS SUPPLIED.
      APPEND LINES OF it_email_list TO mt_email_list.
    ENDIF.

  ENDMETHOD.

  METHOD get_emails.
    rt_val = mt_email_list.
  ENDMETHOD.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

INTERFACE lif_get_mails.
  METHODS handle
    IMPORTING io_request TYPE REF TO lcl_order_data.

  METHODS set_next
    IMPORTING io_handler TYPE REF TO lif_get_mails.

ENDINTERFACE.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_get_mails_a DEFINITION .

  PUBLIC SECTION.
    INTERFACES lif_get_mails.

  PROTECTED SECTION.
    DATA mo_next_handler TYPE REF TO lif_get_mails.
    DATA mo_request TYPE REF TO lcl_order_data.

    METHODS if_do_proceed
      RETURNING VALUE(rv_val) TYPE char1.

    METHODS fill_emails.

  PRIVATE SECTION.


ENDCLASS.

CLASS lcl_get_mails_a IMPLEMENTATION.
  METHOD lif_get_mails~handle.

    " передаем следующему обработчику, проверив его наличие

    IF mo_next_handler IS BOUND.
      mo_next_handler->handle( io_request = io_request ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_get_mails~set_next.
    mo_next_handler ?= io_handler.
  ENDMETHOD.

  METHOD if_do_proceed.
    MESSAGE x000(cl). " should be redefined
  ENDMETHOD.

  METHOD fill_emails.
    MESSAGE x000(cl). " should be redefined
  ENDMETHOD.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_get_mails_by_cust DEFINITION INHERITING FROM lcl_get_mails_a.
  PUBLIC SECTION.

    METHODS constructor.
    METHODS lif_get_mails~handle REDEFINITION.

  PROTECTED SECTION.
    METHODS if_do_proceed REDEFINITION.
    METHODS fill_emails REDEFINITION.


  PRIVATE SECTION.


ENDCLASS.

CLASS lcl_get_mails_by_cust IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.

  METHOD lif_get_mails~handle.
    mo_request ?= io_request.
    IF if_do_proceed(  ).
      fill_emails(  ).
    ELSE.
      super->lif_get_mails~handle( io_request = io_request ).
    ENDIF.
  ENDMETHOD.

  METHOD if_do_proceed.
    DATA ls_order_info TYPE lcl_order_data=>ts_order_info.

    ls_order_info = mo_request->get_order_info( ).

    IF ls_order_info-customer EQ '1210'.
      rv_val = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD fill_emails.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "" алгоритм сбора адресотова: из контактных данных клиента, и связанных с клиентом лиц
    "" возможно по аффилированным лицам

    mo_request->set_emails(
      EXPORTING
        it_email_list =  VALUE lcl_order_data=>tt_email_list(
        ( email = 'commercial_dir@email.com' )
        ( email = 'accountant_sales@email.com' )
        ( email = 'sales_manager1@email.com' )
        ( email = 'sales_manager2@email.com' )
        ( email = 'mister_one@email.com' )
        ( email = 'brother_of_mister_one@email.com' )
        ( email = 'son_of_mister_one@email.com' )
        ( email = 'mister_two@email.com' )
        ( email = 'wife_of_mister_two@email.com' )
        )
    ).
  ENDMETHOD.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""

CLASS lcl_mails_by_cust_type_oao DEFINITION INHERITING FROM lcl_get_mails_a.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS lif_get_mails~handle REDEFINITION.

  PROTECTED SECTION.
    METHODS if_do_proceed REDEFINITION.
    METHODS fill_emails REDEFINITION.


  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_mails_by_cust_type_oao IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.

  METHOD lif_get_mails~handle.
    mo_request ?= io_request.
    IF if_do_proceed(  ).
      fill_emails(  ).
    ELSE.
      super->lif_get_mails~handle( io_request = io_request ).
    ENDIF.
  ENDMETHOD.

  METHOD if_do_proceed.
    DATA ls_order_info TYPE lcl_order_data=>ts_order_info.

    ls_order_info = mo_request->get_order_info( ).

    CLEAR rv_val.

    IF ls_order_info-customer_type EQ 'ZOAO'.
      rv_val = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD fill_emails.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "" алгоритм сбора адресотова: из контактных данных клиента, и связанных с клиентом лиц
    "" возможно по аффилированным лицам

    mo_request->set_emails(
      EXPORTING
        it_email_list =  VALUE lcl_order_data=>tt_email_list(
        ( email = 'commercial_dir@email.com' )
        ( email = 'accountant_sales@email.com' )
        ( email = 'sales_manager1@email.com' )
        ( email = 'sales_manager2@email.com' )
        )
    ).
  ENDMETHOD.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""

CLASS lcl_mails_by_disr_02 DEFINITION INHERITING FROM lcl_get_mails_a.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS lif_get_mails~handle REDEFINITION.

  PROTECTED SECTION.
    METHODS if_do_proceed REDEFINITION.
    METHODS fill_emails REDEFINITION.


  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_mails_by_disr_02 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.

  METHOD lif_get_mails~handle.
    mo_request ?= io_request.
    IF if_do_proceed(  ).
      fill_emails(  ).
    ELSE.
      super->lif_get_mails~handle( io_request = io_request ).
    ENDIF.
  ENDMETHOD.

  METHOD if_do_proceed.
    DATA ls_order_info TYPE lcl_order_data=>ts_order_info.

    ls_order_info = mo_request->get_order_info( ).

    CLEAR rv_val.

    IF ls_order_info-distr_channel EQ '02'.
      rv_val = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD fill_emails.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "" алгоритм сбора адресотова: из контактных данных клиента, и связанных с клиентом лиц
    "" возможно по аффилированным лицам

    mo_request->set_emails(
      EXPORTING
        it_email_list =  VALUE lcl_order_data=>tt_email_list(
        ( email = 'manager_special@email.com' )
        ( email = 'manager_special2@email.com' )
        ( email = 'manager_special3@email.com' )
        )
    ).
  ENDMETHOD.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""

CLASS lcl_mails_common DEFINITION INHERITING FROM lcl_get_mails_a.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS lif_get_mails~handle REDEFINITION.

  PROTECTED SECTION.
    METHODS if_do_proceed REDEFINITION.
    METHODS fill_emails REDEFINITION.


  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_mails_common IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.

  METHOD lif_get_mails~handle.
    mo_request ?= io_request.
    IF if_do_proceed(  ).
      fill_emails(  ).
    ELSE.
      super->lif_get_mails~handle( io_request = io_request ).
    ENDIF.
  ENDMETHOD.

  METHOD if_do_proceed.
    DATA ls_order_info TYPE lcl_order_data=>ts_order_info.

    ls_order_info = mo_request->get_order_info( ).

    CLEAR rv_val.

    " always true
    rv_val = abap_true.


  ENDMETHOD.

  METHOD fill_emails.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "" алгоритм сбора адресов - простой и общий

    mo_request->set_emails(
      EXPORTING
        it_email_list =  VALUE lcl_order_data=>tt_email_list(
        ( email = 'mail1@email.com' )
        ( email = 'mail2@email.com' )
        ( email = 'mail3@email.com' )
        )
    ).
  ENDMETHOD.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_chain_of_responsibility DEFINITION.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS main.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ts_chain_obj
            , cls TYPE seoclsname
            , obj TYPE REF TO lif_get_mails
            , sort_order_spec2common TYPE syindex
            , sort_order_common2spec TYPE syindex
        , END OF ts_chain_obj
        , tt_chain_obj TYPE STANDARD TABLE OF ts_chain_obj WITH DEFAULT KEY
        .


    DATA mt_order_info TYPE lcl_order_data=>tt_order_info.




    METHODS run_step1.
    METHODS run_step2.
    METHODS run_step3.

    METHODS get_chain
      IMPORTING iv_sort_type  TYPE string DEFAULT 'spec2common'
      RETURNING VALUE(rt_val) TYPE tt_chain_obj.

    METHODS order2request
      IMPORTING ir_order_info TYPE REF TO lcl_order_data=>ts_order_info
      RETURNING VALUE(ro_obj) TYPE REF TO lcl_order_data.

    METHODS fill_chain
      CHANGING ct_chain TYPE tt_chain_obj.

ENDCLASS.

CLASS lcl_chain_of_responsibility IMPLEMENTATION.

  METHOD constructor.
    CLEAR mt_order_info.

    mt_order_info = VALUE #(
    (  customer_type = 'ZOAO' customer = '1050' order_type = 'ZOR1' order = '41010' distr_channel = '01' )
    (  customer_type = 'ZOAO' customer = '1060' order_type = 'ZOR1' order = '41020' distr_channel = '02' )
    (  customer_type = 'ZOAO' customer = '1050' order_type = 'ZOR2' order = '42110' distr_channel = '03' )
    (  customer_type = 'ZOOO' customer = '3030' order_type = 'ZOR2' order = '42130' distr_channel = '01' )
    (  customer_type = 'ZOOO' customer = '3033' order_type = 'ZOR2' order = '42140' distr_channel = '02' )
    (  customer_type = 'ZIP'  customer = '1210' order_type = 'ZOR1' order = '41050' distr_channel = '03' )
    (  customer_type = 'ZIP'  customer = '1220' order_type = 'ZOR1' order = '41060' distr_channel = '01' )
    ).

  ENDMETHOD.                    "constructor

  METHOD main.
    run_step1( ).
    run_step2( ).
    run_step3( ).
  ENDMETHOD.                    "main

  METHOD run_step1.
    DATA lr_chain TYPE REF TO ts_chain_obj.
    DATA(lt_chain) = get_chain(  ).
    DATA lr_order_info TYPE REF TO lcl_order_data=>ts_order_info.

    fill_chain( CHANGING ct_chain = lt_chain ).

    IF lines( lt_chain ) GE 1.
      LOOP AT mt_order_info REFERENCE INTO lr_order_info.
        DATA(obj_sales) =  order2request( lr_order_info ).
        lt_chain[ 1 ]-obj->handle( obj_sales ).

        zcl_lsp012_html=>get_instance( )->add_para_val_ch( iv_id    = |Email по 1ому правилу { lr_order_info->order } |
                                                           iv_value = |Заказ: { lr_order_info->order } |
                                         )->add_tab_ch( it_tab = obj_sales->get_emails(  ) ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.                    "run_step1

  METHOD run_step2.


    DATA(lt_chain) = get_chain( 'common2spec' ).
    DATA lr_order_info TYPE REF TO lcl_order_data=>ts_order_info.

    fill_chain( CHANGING ct_chain = lt_chain ).

    IF lines( lt_chain ) GE 1.
      LOOP AT mt_order_info REFERENCE INTO lr_order_info.
        DATA(obj_sales) =  order2request( lr_order_info ).
        lt_chain[ 1 ]-obj->handle( obj_sales ).

        zcl_lsp012_html=>get_instance( )->add_para_val_ch( iv_id    = |Email по 2ому правилу { lr_order_info->order } |
                                                           iv_value = |Заказ: { lr_order_info->order } |
                                         )->add_tab_ch( it_tab = obj_sales->get_emails(  ) ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD run_step3.
    zcl_lsp012_html=>get_instance( iv_title = |Emails | )->show( ).
  ENDMETHOD.

  METHOD get_chain.
*        IMPORTING iv_sort_type TYPE string DEFAULT 'spec2common'
*        RETURNING VALUE(rt_val) TYPE tt_chain_obj.

    rt_val = VALUE #(
        ( cls = 'LCL_GET_MAILS_BY_CUST'      sort_order_spec2common = 10 sort_order_common2spec = 90 )
        ( cls = 'LCL_MAILS_BY_CUST_TYPE_OAO' sort_order_spec2common = 20 sort_order_common2spec = 80 )
        ( cls = 'LCL_MAILS_BY_DISR_02'       sort_order_spec2common = 30 sort_order_common2spec = 70 )
        ).

    IF iv_sort_type EQ 'spec2common'.
      SORT rt_val BY sort_order_spec2common ASCENDING.
    ENDIF.

    IF iv_sort_type EQ 'common2spec'.
      SORT rt_val BY sort_order_spec2common DESCENDING.
    ENDIF.

    APPEND
     VALUE #( cls = 'LCL_MAILS_COMMON'      sort_order_spec2common = 10 sort_order_common2spec = 90 )
     TO rt_val.

  ENDMETHOD.

  METHOD order2request.
    ro_obj = NEW lcl_order_data( ir_order_info->* ).
  ENDMETHOD.

  METHOD fill_chain.
    DATA lr_chain TYPE REF TO ts_chain_obj.
    DATA lo_prev_chain TYPE REF TO lif_get_mails.

    LOOP AT ct_chain REFERENCE INTO lr_chain.
      CREATE OBJECT lr_chain->obj TYPE (lr_chain->cls).
      IF lo_prev_chain IS BOUND.
        lo_prev_chain->set_next( io_handler = lr_chain->obj ).
      ENDIF.

      lo_prev_chain ?= lr_chain->obj.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
