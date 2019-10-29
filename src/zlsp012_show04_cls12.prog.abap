*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW04_CLS12
*&---------------------------------------------------------------------*

CLASS lcl_protection_proxy DEFINITION.
  PUBLIC SECTION.

    INTERFACES zif_lsp012_anytab_upd.

    METHODS constructor.


  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mv_proxy_type TYPE string.
    DATA mo_any_tab_updation TYPE REF TO zcl_lsp012_anytab_upd.

    TYPES: BEGIN OF ts_log_line
          , log_point TYPE char15
          , log_val TYPE char20
        , END OF ts_log_line
        , tt_log_line TYPE STANDARD TABLE OF ts_log_line WITH DEFAULT KEY
        .

    METHODS if_tab_exist
      IMPORTING iv_tabname    TYPE tabname
      RETURNING VALUE(rv_val) TYPE char1.

    METHODS authority4tab_access
      IMPORTING iv_tabname    TYPE tabname
      RETURNING VALUE(rv_val) TYPE char1.

    METHODS do_log_record
      IMPORTING iv_tabname    TYPE tabname
      RETURNING VALUE(rv_val) TYPE char1.

ENDCLASS.

CLASS lcl_protection_proxy IMPLEMENTATION.
  METHOD constructor.
    mv_proxy_type = 'PROTECTION'.
    CREATE OBJECT mo_any_tab_updation.
  ENDMETHOD.

  METHOD zif_lsp012_anytab_upd~put2upd_task.

    " check existance of table
    CHECK if_tab_exist( iv_tabname ) EQ abap_true.

    " check auth for table
    CHECK authority4tab_access( iv_tabname ) EQ abap_true.

    " do log action
    do_log_record( iv_tabname ).

    " do update
    IF mo_any_tab_updation->zif_lsp012_anytab_upd~put2upd_task(
        iv_tabname   = iv_tabname                 " Table Name
        it_data      = it_data
        iv_do_commit = iv_do_commit       " Single-Character Flag
    ) EQ 0.
      " good
    ELSE.
      " bad
    ENDIF.

  ENDMETHOD.


  METHOD if_tab_exist.
    "      IMPORTING iv_tabname TYPE tabname.
    DATA lt_dd02l TYPE dd02ltab.

    SELECT tabname as4local as4vers tabclass sqltab datmin datmax
      FROM dd02l
      INTO TABLE lt_dd02l
      WHERE tabname = iv_tabname
        AND as4local = 'A'.

    rv_val = boolc( lines( lt_dd02l ) NE 0 ).
    zcl_lsp012_html=>get_instance( )->add_para_val_ch( iv_id    = 'EXIST' iv_value = rv_val ).
  ENDMETHOD.

  METHOD authority4tab_access.
    "     IMPORTING iv_tabname TYPE tabname.



    rv_val = abap_false.

    AUTHORITY-CHECK OBJECT 'S_TABU_NAM'
     ID 'ACTVT' FIELD '02'
     ID 'TABLE' FIELD iv_tabname.
    IF sy-subrc <> 0.
*     Implement a suitable exception handling here
      zcl_lsp012_html=>get_instance( )->add_para_val_ch( iv_id    = 'AUTH' iv_value = 'NO' ).
      RETURN.
    ELSE.
      zcl_lsp012_html=>get_instance( )->add_para_val_ch( iv_id    = 'AUTH' iv_value = 'YES' ).
      rv_val = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD do_log_record.
    "      IMPORTING iv_tabname TYPE tabname.

    DATA lt_log_line2 TYPE tt_log_line.

    GET TIME.
    DATA(lt_log_line) = VALUE tt_log_line(
      ( log_point = 'START_UPD' log_val = |{ sy-datum }:{ sy-uzeit }| )
      ( log_point = 'USER' log_val = |{ cl_abap_syst=>get_user_name( ) } | )
      ( log_point = 'TAB_NAME' log_val = |{ iv_tabname } | )
      ).

    zcl_lsp012_html=>get_instance(
        iv_title = 'Log'
    )->add_tab_ch( it_tab = lt_log_line ).

    GET TIME.
    zcl_lsp012_html=>get_instance(
        iv_title = 'Log'
    )->add_tab_ch( it_tab = VALUE tt_log_line(
      ( log_point = 'START_UPD' log_val = |FUNC: { sy-datum }:{ sy-uzeit }| )
      ( log_point = 'USER' log_val = |FUNC: { cl_abap_syst=>get_user_name( ) } | )
      ( log_point = 'TAB_NAME' log_val = |FUNC: { iv_tabname } | )
      )
    ).

  ENDMETHOD.

ENDCLASS.



"""""""""""""""""
"""""""""""""""""
CLASS lcl_remote_proxy DEFINITION.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_url TYPE string.

    METHODS get_rates
      RETURNING VALUE(rv_val) TYPE string.

    METHODS get_cities
      RETURNING VALUE(rv_val) TYPE string.

    METHODS close_connection.

  PROTECTED SECTION.


  PRIVATE SECTION.
    DATA mv_url TYPE string.
    DATA mo_client TYPE REF TO if_http_client.

ENDCLASS.


CLASS lcl_remote_proxy IMPLEMENTATION.
  METHOD constructor.
    " http://olegbash.ru/CallABAP/exrate/CHF_RUB_20190512.json
    "    DATA lv_url TYPE string VALUE 'http://olegbash.ru/'.

    mv_url = iv_url.
    cl_http_client=>create_by_url(
  EXPORTING
    url                = mv_url    " URL
*        proxy_host         =     " Logical Destination (Specified in Function Call)
*        proxy_service      =     " Port Number
*        ssl_id             =     " SSL Identity
*        sap_username       =     " ABAP System, User Logon Name
*        sap_client         =     " R/3 System, Client Number from Logon
  IMPORTING
    client             =  mo_client   " HTTP Client Abstraction
  EXCEPTIONS
    argument_not_found = 1
    plugin_not_active  = 2
    internal_error     = 3
    OTHERS             = 4
).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD get_rates.
    DATA lv_add_url TYPE string VALUE '/CallABAP/exrate/CHF_RUB_20190512.json'.
    "    DATA lv_response TYPE string.


    cl_http_utility=>set_request_uri(
    request = mo_client->request
    uri     = lv_add_url ).

    mo_client->send(
      EXCEPTIONS
        OTHERS = 4 ).
    IF sy-subrc <> 0.
      mo_client->get_last_error(
        IMPORTING message = rv_val ).
      cl_demo_output=>display( rv_val ).
      RETURN.
    ENDIF.

    mo_client->receive(
     EXCEPTIONS
       OTHERS = 4 ).
    IF sy-subrc <> 0.
      mo_client->get_last_error(
       IMPORTING message = rv_val ).

      RETURN.
    ENDIF.

    rv_val = mo_client->response->get_cdata( ).

  ENDMETHOD.

  METHOD get_cities.

    DATA lv_add_url TYPE string VALUE '/CallABAP/cities/city.list3kb.json'.
    "    DATA lv_response TYPE string.


    cl_http_utility=>set_request_uri(
    request = mo_client->request
    uri     = lv_add_url ).

    mo_client->send(
      EXCEPTIONS
        OTHERS = 4 ).
    IF sy-subrc <> 0.
      mo_client->get_last_error(
        IMPORTING message = rv_val ).
      cl_demo_output=>display( rv_val ).
      RETURN.
    ENDIF.

    mo_client->receive(
     EXCEPTIONS
       OTHERS = 4 ).
    IF sy-subrc <> 0.
      mo_client->get_last_error(
       IMPORTING message = rv_val ).

      RETURN.
    ENDIF.

    rv_val = mo_client->response->get_cdata( ).

  ENDMETHOD.

  METHOD close_connection.
    mo_client->close(
      EXCEPTIONS
        http_invalid_state = 1                " Invalid state
        OTHERS             = 2
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

ENDCLASS.


"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
*----------------------------------------------------------------------*
*       CLASS lcl_proxy DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_proxy DEFINITION.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS main.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS run_step1.
    METHODS run_step2.
    METHODS run_step3.
    METHODS get_tsl
      RETURNING VALUE(rv_val) TYPE timestampl.


ENDCLASS.                    "lcl_proxy DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_proxy IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_proxy IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD.                    "constructor

  METHOD main.
    run_step1( ).
    run_step2( ). " pure http client
    run_step3( ). " proxy (from ZLSP013)
  ENDMETHOD.                    "main

  METHOD run_step1.
    "   DATA lo_protection_proxy TYPE REF TO lcl_protection_proxy.
    DATA lt_dbsave TYPE STANDARD TABLE OF ztlsp012_dtsave.

    lt_dbsave =   VALUE #(
          FOR tab_line = 0 THEN tab_line + 1 UNTIL tab_line > 10
          ( ts = get_tsl( )
            col_info = |COL: { tab_line }|
            col_info2 = |COL2: { tab_line ** 2 } |
            create_user = sy-uname
            create_date = sy-datum
            create_time = sy-uzeit
            ) ) .

    DATA(lo_protection_proxy) = NEW lcl_protection_proxy( ).

    lo_protection_proxy->zif_lsp012_anytab_upd~put2upd_task(
      EXPORTING
        iv_tabname   = 'ZTLSP012_DTSAVE'                 " Table Name
        it_data      = lt_dbsave
        iv_do_commit = abap_true       " Single-Character Flag
    ).

    zcl_lsp012_html=>get_instance(
*        iv_title =
    )->show( ).

  ENDMETHOD.                    "run_step1

  METHOD get_tsl.
    GET TIME.

    GET TIME STAMP FIELD rv_val.

  ENDMETHOD.

  METHOD run_step2.
    DATA(lo_remote_proxy) = NEW lcl_remote_proxy( 'http://olegbash.ru/' ).

    zcl_lsp012_html=>get_instance(
*        iv_title =
    )->add_para_val_ch(
        iv_id    = 'JSON_RATES'
        iv_value = CONV text200( lo_remote_proxy->get_rates( ) )
    )->add_para_val_ch(
        iv_id    = 'JSON_CITIES'
        iv_value = CONV text200( lo_remote_proxy->get_cities( ) ) )->show( ).

    lo_remote_proxy->close_connection( ).

  ENDMETHOD.

  METHOD run_step3.
    " показать на генерация consumer через SOAP
    " http://www.dneonline.com/calculator.asmx?wsdl
    DATA ls_input  TYPE zcalcadd_soap_in.
    DATA ls_output  TYPE zcalcadd_soap_out.
    DATA lx_sys_fault TYPE REF TO cx_ai_system_fault.

    ls_input-int_a = 5.
    ls_input-int_b = 4.

    TRY .
        DATA(lo_calc_proxy) = NEW zcalcco_calculator_soap( 'DEFAULT' ).

        lo_calc_proxy->add(
          EXPORTING
            input  = ls_input
          IMPORTING
            output = ls_output
        ).

        zcl_lsp012_html=>get_instance(
*        iv_title =
      )->add_para_val_ch(
          iv_id    = 'SOAP_OUTPUT'
          iv_value = CONV text200( ls_output-add_result )
       )->show( ).

      CATCH cx_ai_system_fault INTO lx_sys_fault.

    ENDTRY.



  ENDMETHOD.

ENDCLASS.
