*&---------------------------------------------------------------------*
*& Include          ZREP_C8A014_DATETIME_CLS99
*&---------------------------------------------------------------------*

CLASS lcl_rep_app DEFINITION.

  PUBLIC SECTION.
    METHODS constructor.
    METHODS init.
    METHODS start_of_sel.
    METHODS end_of_sel.
    METHODS at_sel_screen.
    METHODS at_sel_screen_output.

    METHODS on_val_req_p_mode
      CHANGING cv TYPE char2.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA ms_scr TYPE lif_share=>ts_scr.
    METHODS _fill_screen.

ENDCLASS.

CLASS lcl_rep_app IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.

  METHOD init.

  ENDMETHOD.

  METHOD start_of_sel.

    _fill_screen( ).

    CASE ms_scr-mode_date_sample.
      WHEN 'D1'.
        NEW lcl_sys_vars( ms_scr )->fn( ).
      WHEN 'D2'.
        NEW lcl_date_time_conv( ms_scr )->fn( ).
      WHEN 'D3'.
        NEW lcl_date_time_calc( ms_scr )->fn( ).
      WHEN OTHERS.
        WRITE:/ 'No such mode'.
    ENDCASE.

  ENDMETHOD.

  METHOD end_of_sel.

  ENDMETHOD.

  METHOD at_sel_screen.

  ENDMETHOD.

  METHOD at_sel_screen_output.

  ENDMETHOD.

  METHOD on_val_req_p_mode.
    "CHANGING cv_mode TYPE char2.

    TYPES: BEGIN OF ts_date_mode_f4
          , date_mode TYPE char2
          , date_mode_descr TYPE text50
      , END OF ts_date_mode_f4
      , tt_date_mode_f4 TYPE STANDARD TABLE OF ts_date_mode_f4 WITH DEFAULT KEY
      .

    DATA lt_date_mode_f4 TYPE tt_date_mode_f4.
    DATA lv_user_reset TYPE char1.
    DATA lt_field_tab TYPE STANDARD TABLE OF dfies.
    DATA lt_return_tab TYPE STANDARD TABLE OF ddshretval.
    DATA lt_dynpfld_mapping TYPE STANDARD TABLE OF dselc.

    lt_date_mode_f4 = VALUE #(
    ( date_mode = 'D1' date_mode_descr = 'Системные переменные для дат и времени' )
    ( date_mode = 'D2' date_mode_descr = 'Конвертация форматов дат/времени' )
    ).

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
*       ddic_structure  = space
        retfield        = 'DATE_MODE'
*       pvalkey         = space
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
*       dynprofield     = space
*       stepl           = 0
"       window_title    =
*       value           = space
        value_org       = 'S'
*       multiple_choice = space
*       display         = space
*       callback_program = space
*       callback_form   = space
*       callback_method =
*       mark_tab        =
      IMPORTING
        user_reset      = lv_user_reset
      TABLES
        value_tab       = lt_date_mode_f4
        field_tab       = lt_field_tab
        return_tab      = lt_return_tab
        dynpfld_mapping = lt_dynpfld_mapping
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
    ELSE.
      IF lines( lt_return_tab ) > 0.
        cv = VALUE #( lt_return_tab[ 1 ]-fieldval OPTIONAL ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD _fill_screen.
    ms_scr-mode_date_sample = p_mode.
  ENDMETHOD.

ENDCLASS.

DATA lo_rep_app TYPE REF TO lcl_rep_app.
