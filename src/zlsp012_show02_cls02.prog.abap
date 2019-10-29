*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW02_CLS02
*&---------------------------------------------------------------------*

CLASS lcl_app DEFINITION.

  PUBLIC SECTION.

    INTERFACES lif_app.
    INTERFACES lif_app_const.

    ALIASES c_onli FOR lif_app_const~c_onli.

    METHODS constructor.


  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ts_outtab
             , num TYPE num8
             , val_num TYPE text20
             , comment_val TYPE text50
             , is_checkbox TYPE char1
          , END OF ts_outtab
          , tt_outtab TYPE STANDARD TABLE OF ts_outtab
            WITH DEFAULT KEY
          .


    DATA ms_screen TYPE ts_screen.
    DATA mo_html TYPE REF TO lcl_html.



    METHODS test_add.

    METHODS fill_tab
      CHANGING ct_tab TYPE tt_outtab.

    METHODS on_html_act
      FOR EVENT html_action OF lcl_html
        IMPORTING ev_action
        .

ENDCLASS.                    "lcl_app DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_app IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_app IMPLEMENTATION.

  METHOD constructor.

  ENDMETHOD.                    "constructor

  METHOD lif_app~set_scr.
    ms_screen = is_scr.
    CASE  iv_syucomm.
      WHEN c_onli.
        me->lif_app~do_check_before( ).
      WHEN OTHERS.
    ENDCASE.


  ENDMETHOD.                    "set_scr

  METHOD lif_app~process.
    IF mo_html IS BOUND.
      FREE mo_html.
    ENDIF.

    CREATE OBJECT mo_html
      EXPORTING
        iv_title = ms_screen-p_text.

    SET HANDLER on_html_act   FOR mo_html.

    mo_html->add_para_val(
      EXPORTING iv_id = ms_screen-p_id
                iv_value = ms_screen-p_idval ).


    me->test_add( ).

  ENDMETHOD.                    "process

  METHOD lif_app~output.
    mo_html->show( ).
  ENDMETHOD.                    "output

  METHOD lif_app~do_check_before.

  ENDMETHOD.                    "lif_app~do_check_before

  METHOD test_add.
    DATA lv_text20 TYPE text20.
    DATA lv_text200 TYPE text200.

    DATA lt_outtab TYPE tt_outtab.

    lv_text20 = 'TEST_ID10'.
    lv_text200 = 'TEST_VALUE10'.

    mo_html->add_para_val(
      EXPORTING iv_id = lv_text20
              iv_value = lv_text200 ).

    lv_text20 = 'TEST_ID90'.
    lv_text200 = 'TEST_VALUE90'.

    mo_html->add_para_val(
      EXPORTING iv_id = lv_text20
              iv_value = lv_text200 ).


    me->fill_tab( CHANGING ct_tab = lt_outtab ).

    mo_html->add_tab( EXPORTING it_tab = lt_outtab ).

  ENDMETHOD.                    "test_add
  METHOD fill_tab.
    "    CHANGING ct_tab TYPE tt_outtab.
    FIELD-SYMBOLS <fs_tab> TYPE ts_outtab.

    APPEND INITIAL LINE TO ct_tab ASSIGNING <fs_tab>.
    <fs_tab>-num = '100'.
    <fs_tab>-val_num = 'VALUE_100'.
    <fs_tab>-comment_val = 'Comment for VAL100'.
    <fs_tab>-is_checkbox = abap_true.

    APPEND INITIAL LINE TO ct_tab ASSIGNING <fs_tab>.
    <fs_tab>-num = '200'.
    <fs_tab>-val_num = 'VALUE_200'.
    <fs_tab>-comment_val = 'Comment for VAL200'.
    <fs_tab>-is_checkbox = abap_false.

  ENDMETHOD.                    "fill_tab

  METHOD on_html_act.
    DATA lv_text20 TYPE text20.
    DATA lv_text200 TYPE text200.

    DATA lv_text20_3 TYPE text20 VALUE 'TEST_ID390'.
    DATA lv_text200_3 TYPE text200 VALUE 'TEST_VALUE390'.

    lv_text20 = 'TEST_ID190'.
    lv_text200 = 'TEST_VALUE190'.

    DATA lt_outtab TYPE tt_outtab.

    CASE ev_action.
      WHEN 'F1'.

        " simple one by one

*        mo_html->add_para_val(
*          EXPORTING iv_id = lv_text20
*                  iv_value = lv_text200 ).
*        mo_html->show( ).

        "vs " method chaining


        mo_html->add_para_val_ch(
          EXPORTING iv_id = lv_text20
                  iv_value = lv_text200
                  )->show( ).

      WHEN 'F2'.
        me->fill_tab( CHANGING ct_tab = lt_outtab ).

        " method chaining
        mo_html->add_tab_ch( EXPORTING it_tab = lt_outtab )->show( ).

      WHEN 'F3'.

        me->fill_tab( CHANGING ct_tab = lt_outtab ).

        BREAK-POINT.

        " method chaining
        mo_html->add_para_val_ch( iv_id = lv_text20 iv_value = lv_text200
         )->add_tab_ch( it_tab = lt_outtab
         )->add_para_val_ch( iv_id = lv_text20_3 iv_value = lv_text200_3
         )->show( ).


      WHEN OTHERS.
    ENDCASE.



  ENDMETHOD.                    "on_html_act

ENDCLASS.                    "lcl_app IMPLEMENTATION
