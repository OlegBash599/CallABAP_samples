*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW02_CLS01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&  Include           ZLSP010_SHOW02_CLS01
*&---------------------------------------------------------------------*

CLASS lcl_html DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ts_index_str " определяем структуру
              , index TYPE syindex
              , line TYPE string
          , END OF ts_index_str " определяем структур
          , tt_index_str TYPE STANDARD TABLE OF ts_index_str
            WITH DEFAULT KEY
          .

    METHODS constructor
        IMPORTING iv_title TYPE text200 DEFAULT 'HTML abap'.

    METHODS add_para_val
      IMPORTING iv_id TYPE text20
                iv_value TYPE text200.

    METHODS add_tab
      IMPORTING it_tab TYPE table.

    METHODS add_para_val_ch
      IMPORTING iv_id TYPE text20
                iv_value TYPE text200
      RETURNING value(ro_obj) TYPE REF TO lcl_html
                .

    METHODS add_tab_ch
      IMPORTING it_tab TYPE table
        RETURNING value(ro_obj) TYPE REF TO lcl_html.


    METHODS show.

    EVENTS html_action EXPORTING value(ev_action) TYPE string.


  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES: BEGIN OF ts_html_part
              , tag_str TYPE text20
              , content TYPE tt_index_str
          , END OF ts_html_part
          .

    CONSTANTS: c_step_index TYPE syindex VALUE 10.

    DATA mt_head TYPE tt_index_str.
    DATA mt_body TYPE tt_index_str.
    DATA mv_title_win TYPE cl_abap_browser=>title. "text255.


    METHODS fill_default_head
      IMPORTING iv_title TYPE text200 .

    METHODS add_index
      CHANGING cv_index TYPE syindex.

    METHODS add_html_line
      CHANGING ct_block TYPE tt_index_str
               cs_line TYPE ts_index_str.

    METHODS get_html_tab
      RETURNING value(rt_val) TYPE cl_abap_browser=>html_table.
      "RETURNING value(rt_val) TYPE lcl_=>html_table.

    METHODS add_head_line
      IMPORTING iv_line TYPE c.

    METHODS add_body_line
      IMPORTING iv_line TYPE c.

    METHODS add_html_menu.

    METHODS on_html_sapevent
      FOR EVENT sapevent
      OF cl_abap_browser
      IMPORTING action.


ENDCLASS.                    "lcl_html DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_html IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_html IMPLEMENTATION.
  METHOD constructor.
    me->mv_title_win = iv_title.
    fill_default_head( iv_title = iv_title ).
    CLEAR mt_body.
    add_html_menu( ).
  ENDMETHOD.                    "constructor
  METHOD add_para_val.
*      IMPORTING iv_id TYPE text20
*                iv_value TYPE text200.
    DATA ls_body_line TYPE ts_index_str.


    ls_body_line-line = ` <div> <p> `.
    add_index( CHANGING cv_index = ls_body_line-index ).
    add_html_line( CHANGING ct_block = mt_body cs_line = ls_body_line ).

    ls_body_line-line = `<strong>` && iv_id && `: </strong>`.
    add_index( CHANGING cv_index = ls_body_line-index ).
    add_html_line( CHANGING ct_block = mt_body cs_line = ls_body_line ).

    ls_body_line-line = `<em>` && iv_value && `</em>`.
    add_index( CHANGING cv_index = ls_body_line-index ).
    add_html_line( CHANGING ct_block = mt_body cs_line = ls_body_line ).

    ls_body_line-line = ` </p></div>`.
    add_index( CHANGING cv_index = ls_body_line-index ).
    add_html_line( CHANGING ct_block = mt_body cs_line = ls_body_line ).

  ENDMETHOD.                    "add_para_val
  METHOD add_tab.
    "      IMPORTING it_tab TYPE table.

    " WWW_ITAB_TO_HTML - добавляет всю целиком,
    " а нужно в DIV с отдельным стилем и ID

    DATA ls_body_line TYPE ts_index_str.

    DATA lo_desc_table    TYPE REF TO cl_abap_tabledescr.
    DATA lo_desc_struc    TYPE REF TO cl_abap_structdescr.
    DATA lv_text255 TYPE text255.


    FIELD-SYMBOLS <fs_component> TYPE abap_compdescr.
    FIELD-SYMBOLS <fs_itab> TYPE any.
    FIELD-SYMBOLS <fs_field> TYPE any.


    lo_desc_table ?= cl_abap_tabledescr=>describe_by_data( it_tab ).
    lo_desc_struc ?= lo_desc_table->get_table_line_type( ).


    ls_body_line-line = ` <div> <table id="table_id" class="display"> `.
    add_index( CHANGING cv_index = ls_body_line-index ).
    add_html_line( CHANGING ct_block = mt_body cs_line = ls_body_line ).

    " добавляем заголовок
    ls_body_line-line = `<thead> <tr>` .
    add_index( CHANGING cv_index = ls_body_line-index ).
    add_html_line( CHANGING ct_block = mt_body cs_line = ls_body_line ).
    LOOP AT lo_desc_struc->components ASSIGNING <fs_component>.
*      ls_body_line-line = `<th>` .
*      add_index( CHANGING cv_index = ls_body_line-index ).
*      add_html_line( CHANGING ct_block = mt_body cs_line = ls_body_line ).

      add_body_line( iv_line = '<th>' ).
      add_body_line( iv_line =  <fs_component>-name ).
      add_body_line( iv_line = '</th>' ).

    ENDLOOP.

    add_body_line( iv_line = '<tbody>' ).



    LOOP AT it_tab ASSIGNING <fs_itab>.
      add_body_line( iv_line = '<tr>' ).
      LOOP AT lo_desc_struc->components ASSIGNING <fs_component>.
        add_body_line( iv_line = '<td>' ).
        ASSIGN COMPONENT <fs_component>-name OF STRUCTURE <fs_itab> TO <fs_field>.
        IF sy-subrc EQ 0.
          lv_text255 = <fs_field>.
          add_body_line( iv_line = lv_text255 ).
        ENDIF.
        add_body_line( iv_line = '</td>' ).
      ENDLOOP.
      add_body_line( iv_line = '</tr>' ).
    ENDLOOP.
    add_body_line( iv_line = '</tbody>' ).
    add_body_line( iv_line = '</table>' ).
    add_body_line( iv_line = '</div>' ).



  ENDMETHOD.                    "add_tab

  METHOD get_html_tab.
    DATA ls_html_line TYPE cl_abap_browser=>html_line.
    FIELD-SYMBOLS <fs_line> LIKE LINE OF mt_head.

    CLEAR rt_val[].
    LOOP AT mt_head ASSIGNING <fs_line>.
      CLEAR ls_html_line.
      ls_html_line = <fs_line>-line.
      APPEND ls_html_line TO rt_val.
    ENDLOOP.

    APPEND '<body>' TO rt_val.



    LOOP AT mt_body ASSIGNING <fs_line>.
      CLEAR ls_html_line.
      ls_html_line = <fs_line>-line.
      APPEND ls_html_line TO rt_val.
    ENDLOOP.

    APPEND '</body>' TO rt_val.
    APPEND '</html>' TO rt_val.

  ENDMETHOD.                    "get_html_tab

  METHOD show.
    DATA lt_html_errors TYPE STANDARD TABLE OF text255.

    SET HANDLER on_html_sapevent.


    cl_abap_browser=>show_html(
      EXPORTING
        html         =  me->get_html_tab( )  " HTML Table, Line Width 255 Characters
        title        =  me->mv_title_win   " Window Title
*        size         = CL_ABAP_BROWSER=>MEDIUM    " Size (S,M.L,XL)
        modal        = abap_true    " Display as Modal Dialog Box
*        html_string  =     " HTML String
*        printing     = ABAP_FALSE
         buttons      = cl_abap_browser=>navigate_html
*        format       = CL_ABAP_BROWSER=>LANDSCAPE    " Landscape/portrait format
*        position     = CL_ABAP_BROWSER=>TOPLEFT    " Position
*        data_table   =     " External data
*        anchor       =     " Goto Point
*        context_menu = ABAP_FALSE    " Display context menu in browser
*        html_xstring =     " HTML Binary String
*        check_html   = ABAP_TRUE    " Test of HTML File
*        container    =
      IMPORTING
        html_errors  = lt_html_errors    " Error List from Test
    ).
  ENDMETHOD.                    "show

  METHOD fill_default_head.
    DATA ls_head TYPE ts_index_str.

    CLEAR mt_head.

    ls_head-index = 10.
    ls_head-line = '<!DOCTYPE html><html lang="ru">'.
    add_html_line( CHANGING ct_block = mt_head cs_line = ls_head ).
    add_index( CHANGING cv_index = ls_head-index ).
    ls_head-line = '<head>'.
    add_html_line( CHANGING ct_block = mt_head cs_line = ls_head ).
    add_index( CHANGING cv_index = ls_head-index ).
    ls_head-line = `<title>` && iv_title && `</title>`.
    add_html_line( CHANGING ct_block = mt_head cs_line = ls_head ).
    add_index( CHANGING cv_index = ls_head-index ).

*    add_head_line( iv_line = '<link rel="stylesheet" type="text/css"' ).
*    add_head_line( iv_line = 'href="https://cdn.datatables.net/1.10.19/css/jquery.dataTables.css">' ).
*    add_head_line( iv_line = '<script type="text/javascript" charset="utf8" ' ).
*    add_head_line( iv_line = 'src="https://code.jquery.com/jquery-3.3.1.js"></script>' ).
*    add_head_line( iv_line = '<script type="text/javascript" charset="utf8" ' ).
*    add_head_line( iv_line = 'src="https://cdn.datatables.net/1.10.19/js/jquery.dataTables.js"></script>' ).
    add_head_line( iv_line = '<style>' ).
    add_head_line( iv_line = 'table, th, td {').
    add_head_line( iv_line = 'border: 1px solid black;' ).
    add_head_line( iv_line = '}' ).
    add_head_line( iv_line = '</style>' ).
    add_head_line( iv_line = '</head>' ).

  ENDMETHOD.                    "fill_default_head

  METHOD add_index.
    cv_index = cv_index + c_step_index.
  ENDMETHOD.                    "add_index

  METHOD add_html_line.
    APPEND cs_line TO ct_block.
  ENDMETHOD.                    "add_html_line

  METHOD add_head_line.
    DATA ls_head_line TYPE ts_index_str.

    ls_head_line-line = iv_line.
    ls_head_line-index = lines( mt_head ) * c_step_index.
    add_index( CHANGING cv_index = ls_head_line-index ).
    add_html_line( CHANGING ct_block = mt_head cs_line = ls_head_line ).
  ENDMETHOD.                    "add_head_line


  METHOD add_body_line.
    DATA ls_body_line TYPE ts_index_str.

    ls_body_line-line = iv_line.
    ls_body_line-index = lines( mt_body ) * c_step_index.
    add_index( CHANGING cv_index = ls_body_line-index ).
    add_html_line( CHANGING ct_block = mt_body cs_line = ls_body_line ).
  ENDMETHOD.                    "add_body_line

  METHOD add_html_menu.
    add_body_line( iv_line = '<span>' ).
    add_body_line( iv_line = '<A HREF=SAPEVENT:F1>Добавить ID|VAL</A>' ).
    add_body_line( iv_line = '</span>' ).
    add_body_line( iv_line = '<span>' ).
    add_body_line( iv_line = '<A HREF=SAPEVENT:F2>Добавить таблицу</A>' ).
    add_body_line( iv_line = '</span>' ).
    add_body_line( iv_line = '<span>' ).
    add_body_line( iv_line = '<A HREF=SAPEVENT:F3>Method Chaining</A>' ).
    add_body_line( iv_line = '</span>' ).

  ENDMETHOD.                    "add_html_menu

  METHOD on_html_sapevent.
*    DATA msg TYPE string.
*
*    BREAK-POINT.
*
*    msg = `on_html_sapevent: ` && action.
*    MESSAGE msg TYPE 'I'.

    RAISE EVENT html_action EXPORTING ev_action = action.
  ENDMETHOD.                    "on_html_sapevent

  METHOD add_para_val_ch.

    me->add_para_val(
      EXPORTING
        iv_id    = iv_id
        iv_value = iv_value
    ).

    ro_obj = me.

  ENDMETHOD.                    "add_para_val_ch

  METHOD add_tab_ch.

    me->add_tab( it_tab = it_tab ).
    ro_obj = me.

  ENDMETHOD.                    "add_tab_ch

ENDCLASS.                    "lcl_html IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS ltc_simple_test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_simple_test DEFINITION FOR TESTING
    DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA mo_html TYPE REF TO lcl_html.



    METHODS setup.

    METHODS add_line FOR TESTING.
    METHODS add_line2 FOR TESTING.
    METHODS add_line3 FOR TESTING.


    METHODS teardown.

ENDCLASS.                    "ltc_simple_test DEFINITION


*----------------------------------------------------------------------*
*       CLASS ltc_simple_test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_simple_test IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_html.
  ENDMETHOD.                    "setup

  METHOD add_line.
    DATA lv_text20 TYPE text20.
    DATA lv_text200 TYPE text200.

    lv_text20 = 'TEST_ID10'.
    lv_text200 = 'TEST_VALUE10'.

    mo_html->add_para_val(
      EXPORTING iv_id = lv_text20
              iv_value = lv_text200 ).

    mo_html->show( ).

  ENDMETHOD.                    "add_line

  METHOD add_line2.
    DATA lv_text20 TYPE text20.
    DATA lv_text200 TYPE text200.

    lv_text20 = 'TEST_ID201'.
    lv_text200 = 'TEST_VALUE201'.

    mo_html->add_para_val(
      EXPORTING iv_id = lv_text20
              iv_value = lv_text200 ).

    "mo_html->show( ).
    lv_text20 = 'TEST_ID301'.
    lv_text200 = 'TEST_VALUE301'.

    mo_html->add_para_val(
      EXPORTING iv_id = lv_text20
              iv_value = lv_text200 ).

    mo_html->show( ).

  ENDMETHOD.                    "add_line2

    METHOD add_line3.
    DATA lv_text20 TYPE text20.
    DATA lv_text200 TYPE text200.

    data lt_tvarvc TYPE STANDARD TABLE OF tvarvc.

    BREAK-POINT.


    select * from TVARVC
      into TABLE lt_tvarvc
      UP TO 1000 ROWS
      .





    lv_text20 = 'TEST_ID201'.
    lv_text200 = 'TEST_VALUE201'.

    mo_html->add_para_val(
      EXPORTING iv_id = lv_text20
              iv_value = lv_text200 ).

    "mo_html->show( ).
    lv_text20 = 'TEST_ID301'.
    lv_text200 = 'TEST_VALUE301'.

    mo_html->add_para_val(
      EXPORTING iv_id = lv_text20
              iv_value = lv_text200 ).

    mo_html->add_tab( it_tab = lt_tvarvc ).

    mo_html->show( ).

  ENDMETHOD.

  METHOD teardown.
    CLEAR mo_html.
  ENDMETHOD.                    "teardown

ENDCLASS.                    "ltc_simple_test IMPLEMENTATION
