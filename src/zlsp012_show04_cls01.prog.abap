*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW04_CLS01
*&---------------------------------------------------------------------*

CLASS lcl_singleton_sample DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS main.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS run_step1.
    METHODS run_step2.
    METHODS run_step3.
ENDCLASS.


CLASS lcl_singleton_sample IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.

  METHOD main.

    DATA lv_title TYPE text20 VALUE 'HTML Singleton'.

*    ""[[[раскоментировать код - тут ошибка]]]
*    data lo_html TYPE REF TO zcl_lsp010_html. " <<<<<<<<<<<<<<<<<<<<<<<<
*    create OBJECT lo_html.


    ZCL_LSP012_HTML=>get_instance( iv_title = lv_title )->add_para_val_ch(
        iv_id    = 'ZZZID1'
        iv_value = 'Value for ID1'
    ).

    me->run_step1( ).
    me->run_step2( ).
    me->run_step3( ).

  ENDMETHOD.

  METHOD run_step1.
 "   zcl_lsp010_html=>get_instance( )->add_para_val_ch(
    ZCL_LSP012_HTML=>get_instance( )->add_para_val_ch(
        iv_id    = 'ZZZID_STEP1'
        iv_value = 'Value for STEP1'
    ).
  ENDMETHOD.

  METHOD run_step2.
    DATA lo_html TYPE REF TO zcl_lsp012_html.
    DATA lt_ztvarv TYPE STANDARD TABLE OF ztlsp012_tvarvc.

    SELECT * FROM ztlsp012_tvarvc
      INTO TABLE lt_ztvarv
      UP TO 1000 ROWS.

    lo_html = zcl_lsp012_html=>get_instance( ).
    lo_html->add_tab( it_tab = lt_ztvarv ).

*    data lo_htnl TYPE REF TO ZCL_LSP012_HTML.
*    create OBJECT lo_htnl.

  ENDMETHOD.

  METHOD run_step3.
   " zcl_lsp010_html=>get_instance(  )->show( ).
    zcl_lsp012_html=>get_instance(  )->show( ).
  ENDMETHOD.

ENDCLASS.
