*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW04_CLS20
*&---------------------------------------------------------------------*



INTERFACE lif_strategy_data2customer DEFERRED.
"INTERFACE lif_strategy_data2customer LOAD.


CLASS lcl_cntx_data2customer DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ts_order_info
            , customer_type TYPE char4
            , customer TYPE char10
            , order_type TYPE char4
            , order TYPE char10
          , END OF ts_order_info
          , tt_order_info TYPE STANDARD TABLE OF ts_order_info WITH DEFAULT KEY
          .


    METHODS constructor.
    METHODS provide_data
      EXPORTING et_ret2 TYPE bapirettab.

    METHODS set_strategy
      IMPORTING io_strategy TYPE REF TO lif_strategy_data2customer.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_strategy_d2c TYPE REF TO lif_strategy_data2customer.

ENDCLASS.



INTERFACE lif_strategy_data2customer.


  METHODS set_customer_order_type
    IMPORTING is_order_info TYPE lcl_cntx_data2customer=>ts_order_info.

  METHODS provide_data
    EXPORTING et_ret2 TYPE bapirettab.

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  "" вне паттерна, но может помочь
  METHODS set
    IMPORTING iv_id TYPE clike
              iv_p1 TYPE any.

ENDINTERFACE.


CLASS lcl_cntx_data2customer IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.

  METHOD provide_data.
    mo_strategy_d2c->provide_data( IMPORTING et_ret2 = et_ret2 ).
  ENDMETHOD.

  METHOD set_strategy.
    mo_strategy_d2c ?= io_strategy.
  ENDMETHOD.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_dt2cust_def DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_strategy_data2customer.

    METHODS constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES: BEGIN OF ts_data_model
              , order_num TYPE char10
              , order_posnr TYPE char6
              , order_date TYPE char10
              , order_price TYPE bsprice
              , waers TYPE waers
            , END OF ts_data_model
            , tt_data_model TYPE STANDARD TABLE OF ts_data_model
            .
    DATA ms_order_info TYPE lcl_cntx_data2customer=>ts_order_info.

    DATA mt_data_model TYPE tt_data_model.

ENDCLASS.

CLASS lcl_dt2cust_def IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.

  METHOD lif_strategy_data2customer~set.

  ENDMETHOD.

  METHOD lif_strategy_data2customer~set_customer_order_type.
    ms_order_info = is_order_info.
  ENDMETHOD.

  METHOD lif_strategy_data2customer~provide_data.

    DATA lv_data_line TYPE string.
    DATA lr_data_model TYPE REF TO ts_data_model.
    DATA lv_sep TYPE char1 VALUE '|'.
    DATA file_name TYPE string.

    mt_data_model = VALUE #(
    ( order_num = '1478' order_posnr = '000010' order_date = '20191020' order_price = '100' waers = 'USD' )
    ( order_num = '1478' order_posnr = '000020' order_date = '20191020' order_price = '200' waers = 'USD' )
    ( order_num = '1478' order_posnr = '000030' order_date = '20191020' order_price = '300' waers = 'USD' )
    ).

    CLEAR lv_data_line.
    LOOP AT mt_data_model REFERENCE INTO lr_data_model.
      lv_data_line = lv_data_line &&
           lv_sep && lr_data_model->order_num &&
           lv_sep && lr_data_model->order_posnr &&
           lv_sep && lr_data_model->order_price &&
           lv_sep && lr_data_model->waers &&
           cl_abap_char_utilities=>newline.
    ENDLOOP.

    file_name = 'OUTPUT_' && 'CUST_' && ms_order_info-customer && '_ORDER_' && ms_order_info-order && '.txt'.

    OPEN DATASET file_name FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
    IF sy-subrc EQ 0.
      TRANSFER lv_data_line TO file_name.
      CLOSE DATASET file_name.
    ENDIF.

    zcl_lsp012_html=>get_instance(
*        iv_title =
    )->add_para_val_ch(
      EXPORTING
        iv_id    = 'FILE - default'
        iv_value = CONV text200( file_name )
    )->add_tab( it_tab = mt_data_model ).

  ENDMETHOD.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_dt2cust_zoao_zor1 DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_strategy_data2customer.

    METHODS constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES: BEGIN OF ts_data_model
              , order_num TYPE char10
              , order_posnr TYPE char6
              , order_date TYPE char10
              , matnr TYPE matnr
              , order_price TYPE bsprice
              , waers TYPE waers
            , END OF ts_data_model
            , tt_data_model TYPE STANDARD TABLE OF ts_data_model
            .
    DATA ms_order_info TYPE lcl_cntx_data2customer=>ts_order_info.

    DATA mt_data_model TYPE tt_data_model.

ENDCLASS.

CLASS lcl_dt2cust_zoao_zor1 IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.

  METHOD lif_strategy_data2customer~set.

  ENDMETHOD.

  METHOD lif_strategy_data2customer~set_customer_order_type.
    ms_order_info = is_order_info.
  ENDMETHOD.

  METHOD lif_strategy_data2customer~provide_data.

    DATA lv_data_line TYPE string.
    DATA lr_data_model TYPE REF TO ts_data_model.
    DATA lv_sep TYPE char1 VALUE '|'.
    DATA file_name TYPE string.
    DATA lv_xml TYPE string.

    mt_data_model = VALUE #(
    ( order_num = '1478' order_posnr = '000010' order_date = '20191020' matnr = '789500' order_price = '100' waers = 'USD' )
    ( order_num = '1478' order_posnr = '000020' order_date = '20191020' matnr = '789600' order_price = '200' waers = 'USD' )
    ( order_num = '1478' order_posnr = '000030' order_date = '20191020' matnr = '789700' order_price = '300' waers = 'USD' )
    ).


    file_name = 'OUTPUT_' && 'CUST_' && ms_order_info-customer && '_ORDER_' && ms_order_info-order && '.xml'.

    CALL TRANSFORMATION id
           SOURCE root = mt_data_model
           RESULT XML lv_xml.


    OPEN DATASET file_name FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
    IF sy-subrc EQ 0.
      TRANSFER lv_xml TO file_name.
      CLOSE DATASET file_name.
    ENDIF.


    zcl_lsp012_html=>get_instance(
*        iv_title =
    )->add_para_val_ch(
      EXPORTING
        iv_id    = 'FILE - ZOAO ZOR1'
        iv_value = CONV text200( file_name )
    )->add_tab( it_tab = mt_data_model ).


  ENDMETHOD.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

CLASS lcl_dt2cust_xl DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES lif_strategy_data2customer.
    METHODS constructor.

  PROTECTED SECTION.

    METHODS put2excel
      IMPORTING it_tab       TYPE ANY TABLE
                iv_file_name TYPE string.

  PRIVATE SECTION.


ENDCLASS.

CLASS lcl_dt2cust_xl IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.

  METHOD lif_strategy_data2customer~set.
    MESSAGE x000(cl) WITH 'to be redefined'.
  ENDMETHOD.

  METHOD lif_strategy_data2customer~set_customer_order_type.
    MESSAGE x000(cl) WITH 'to be redefined'.
  ENDMETHOD.

  METHOD lif_strategy_data2customer~provide_data.
    MESSAGE x000(cl) WITH 'to be redefined'.
  ENDMETHOD.

  METHOD put2excel.
    DATA file_name TYPE string.

    DATA: xdata     TYPE xstring,             " Will be used for sending as email
          t_rawdata TYPE solix_tab,           " Will be used for downloading or open directly
          bytecount TYPE i.


    DATA lo_excel                TYPE REF TO zcl_excel.
    DATA lo_worksheet            TYPE REF TO zcl_excel_worksheet.
    DATA: ls_table_settings       TYPE zexcel_s_table_settings.

    DATA: bytes_remain TYPE i.

    DATA cl_writer TYPE REF TO zif_excel_writer.
    FIELD-SYMBOLS: <rawdata> LIKE LINE OF t_rawdata.

    "file_name = 'OUTPUT_' && 'CUST_' && ms_order_info-customer && '_ORDER_' && ms_order_info-order && '.xlsx'.
    file_name = iv_file_name.



    CREATE OBJECT cl_writer TYPE zcl_excel_writer_2007.

    TRY.
        CREATE OBJECT lo_excel.

        " Get active sheet
        lo_worksheet = lo_excel->get_active_worksheet( ).
        lo_worksheet->set_title( ip_title = 'DAta2CUST_ZOOO' ).

        "        lo_worksheet->bind_table( ip_table          = mt_data_model
        lo_worksheet->bind_table( ip_table          = it_tab
                                is_table_settings = ls_table_settings ).


        xdata = cl_writer->write_file( lo_excel ).
        t_rawdata = cl_bcs_convert=>xstring_to_solix( iv_xstring  = xdata ).
        bytecount = xstrlen( xdata ).

        OPEN DATASET file_name FOR OUTPUT IN BINARY MODE.
        IF sy-subrc EQ 0.

          bytes_remain = bytecount.

          LOOP AT t_rawdata ASSIGNING <rawdata>.

            AT LAST.
              CHECK bytes_remain >= 0.
              TRANSFER <rawdata> TO file_name LENGTH bytes_remain.
              EXIT.
            ENDAT.

            TRANSFER <rawdata> TO file_name.
            SUBTRACT 255 FROM bytes_remain.  " Solix has length 255

          ENDLOOP.

          CLOSE DATASET file_name.

        ENDIF.


      CATCH zcx_excel.

    ENDTRY.

  ENDMETHOD.

ENDCLASS.

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_dt2cust_zooo_zor1 DEFINITION
    INHERITING FROM lcl_dt2cust_xl.
  PUBLIC SECTION.
    "    INTERFACES lif_strategy_data2customer.

    METHODS constructor.
    METHODS lif_strategy_data2customer~set_customer_order_type REDEFINITION.
    METHODS lif_strategy_data2customer~provide_data REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES: BEGIN OF ts_data_model
              , order_num TYPE char10
              , order_posnr TYPE char6
              , order_date TYPE char10
              , matnr TYPE matnr
              , matnr_subst TYPE matnr
              , order_price TYPE bsprice
              , waers TYPE waers
            , END OF ts_data_model
            , tt_data_model TYPE STANDARD TABLE OF ts_data_model
            .
    DATA ms_order_info TYPE lcl_cntx_data2customer=>ts_order_info.

    DATA mt_data_model TYPE tt_data_model.



ENDCLASS.

CLASS lcl_dt2cust_zooo_zor1 IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.



  METHOD lif_strategy_data2customer~set_customer_order_type.
    ms_order_info = is_order_info.
  ENDMETHOD.

  METHOD lif_strategy_data2customer~provide_data.

    DATA lv_data_line TYPE string.
    DATA lr_data_model TYPE REF TO ts_data_model.
    DATA lv_sep TYPE char1 VALUE '|'.
    DATA lv_xml TYPE string.
    DATA file_name TYPE string.

    file_name = 'OUTPUT_' && 'CUST_' && ms_order_info-customer && '_ORDER_' && ms_order_info-order && '.xlsx'.

    mt_data_model = VALUE #(
    ( order_num = '1478' order_posnr = '000010' order_date = '20191020' matnr = '789500' matnr_subst = 'sss' order_price = '100' waers = 'USD' )
    ( order_num = '1478' order_posnr = '000020' order_date = '20191020' matnr = '789600' matnr_subst = 'ss2s' order_price = '200' waers = 'USD' )
    ( order_num = '1478' order_posnr = '000030' order_date = '20191020' matnr = '789700' matnr_subst = 'ss3s' order_price = '300' waers = 'USD' )
    ).


    put2excel(
      EXPORTING
        it_tab       = mt_data_model
        iv_file_name = file_name
    ).

    zcl_lsp012_html=>get_instance(
*        iv_title =
)->add_para_val_ch(
  EXPORTING
    iv_id    = 'FILE - ZOOO ZOR1'
    iv_value = CONV text200( file_name )
)->add_tab( it_tab = mt_data_model ).


  ENDMETHOD.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_dt2cust_ztoo_zor3 DEFINITION
    INHERITING FROM lcl_dt2cust_xl.
  PUBLIC SECTION.
    "    INTERFACES lif_strategy_data2customer.

    METHODS constructor.
    METHODS lif_strategy_data2customer~set_customer_order_type REDEFINITION.
    METHODS lif_strategy_data2customer~provide_data REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES: BEGIN OF ts_data_model
              , order_num TYPE char10
              , order_posnr TYPE char6
              , order_date TYPE char10
              , matnr TYPE matnr
              , web_link TYPE text100
              , order_price TYPE bsprice
              , waers TYPE waers
            , END OF ts_data_model
            , tt_data_model TYPE STANDARD TABLE OF ts_data_model
            .
    DATA ms_order_info TYPE lcl_cntx_data2customer=>ts_order_info.

    DATA mt_data_model TYPE tt_data_model.



ENDCLASS.

CLASS lcl_dt2cust_ztoo_zor3 IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.



  METHOD lif_strategy_data2customer~set_customer_order_type.
    ms_order_info = is_order_info.
  ENDMETHOD.

  METHOD lif_strategy_data2customer~provide_data.

    DATA lv_data_line TYPE string.
    DATA lr_data_model TYPE REF TO ts_data_model.
    DATA lv_sep TYPE char1 VALUE '|'.
    DATA lv_xml TYPE string.
    DATA file_name TYPE string.

    file_name = 'OUTPUT_' && 'CUST_' && ms_order_info-customer && '_ORDER_' && ms_order_info-order && '.xlsx'.

    mt_data_model = VALUE #(
    ( order_num = '1478' order_posnr = '000010' order_date = '20191020' matnr = '789500' web_link = 'www.olegbash.ru' order_price = '100' waers = 'USD' )
    ( order_num = '1478' order_posnr = '000020' order_date = '20191020' matnr = '789600' web_link = 'www.yandex.ru' order_price = '200' waers = 'USD' )
    ( order_num = '1478' order_posnr = '000030' order_date = '20191020' matnr = '789700' web_link = 'www.google.ru' order_price = '300' waers = 'USD' )
    ).


    put2excel( EXPORTING it_tab       = mt_data_model
                         iv_file_name = file_name ).


    zcl_lsp012_html=>get_instance(
*        iv_title =
            )->add_para_val_ch( EXPORTING iv_id    = 'FILE - ZTOO ZOR3'
                                          iv_value = CONV text200( file_name )
            )->add_tab( it_tab = mt_data_model ).

  ENDMETHOD.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_strategy DEFINITION.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS main.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: ts_order_info TYPE lcl_cntx_data2customer=>ts_order_info.
    TYPES: tt_order_info TYPE lcl_cntx_data2customer=>tt_order_info.

    DATA mt_order_info TYPE lcl_cntx_data2customer=>tt_order_info.
    DATA mo_context TYPE REF TO lcl_cntx_data2customer.

    METHODS run_step1.
    METHODS run_step2.
    METHODS run_step3.

    METHODS fill_order_data.

    METHODS get_strategy
      IMPORTING is_order_info TYPE ts_order_info
      RETURNING VALUE(ro_obj) TYPE REF TO lif_strategy_data2customer.

ENDCLASS.

CLASS lcl_strategy IMPLEMENTATION.

  METHOD constructor.
    mo_context = NEW lcl_cntx_data2customer(  ).
    fill_order_data(  ).
  ENDMETHOD.                    "constructor

  METHOD main.
    run_step1( ).
    run_step2( ).
    run_step3( ).
  ENDMETHOD.                    "main

  METHOD run_step1.
    DATA lr_order_info TYPE REF TO ts_order_info.

    LOOP AT mt_order_info REFERENCE INTO lr_order_info.
      mo_context->set_strategy( io_strategy = get_strategy( is_order_info = lr_order_info->* ) ).
      mo_context->provide_data(
      ).
    ENDLOOP.

  ENDMETHOD.                    "run_step1


  METHOD run_step2.

  ENDMETHOD.

  METHOD run_step3.
    zcl_lsp012_html=>get_instance(
*        iv_title =
        )->show( ).
  ENDMETHOD.

  METHOD fill_order_data.
    mt_order_info = VALUE #(
    ( customer_type = 'ZOAO' customer = '147' order_type = 'ZOR1' order = '52030' )
    ( customer_type = 'ZOOO' customer = '150' order_type = 'ZOR1' order = '52040' )
    ( customer_type = 'ZLTD' customer = '210' order_type = 'ZOR2' order = '52050' )
    ( customer_type = 'ZIP' customer = '310' order_type = 'ZOR3' order = '52060' )
    ( customer_type = 'ZTOO' customer = '580' order_type = 'ZOR3' order = '52060' )
    ).
  ENDMETHOD.

  METHOD get_strategy.
*        IMPORTING is_order_info TYPE ts_order_info
*        RETURNING VALUE(ro_obj) TYPE REF TO lif_strategy_data2customer.

    DATA lv_cls_name TYPE seoclsname.

    lv_cls_name = 'lcl_dt2cust_' && is_order_info-customer_type && '_' && is_order_info-order_type.

    TRY.
        TRANSLATE lv_cls_name TO UPPER CASE.
        CREATE OBJECT ro_obj TYPE (lv_cls_name).
        ro_obj->set_customer_order_type( is_order_info = is_order_info ).
      CATCH cx_root.
        IF lv_cls_name EQ 'LCL_DT2CUST_DEF'.

        ELSE.
          lv_cls_name = 'LCL_DT2CUST_DEF'.
          RETRY.
        ENDIF.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
