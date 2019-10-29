*&---------------------------------------------------------------------*
*& Include          LZLSP012_MEDIATORP07
*&---------------------------------------------------------------------*

CLASS lcl_colleague_alv IMPLEMENTATION.
  METHOD fill_block.

    IF mo_container IS BOUND.
    ELSE.
      CREATE OBJECT mo_container
        EXPORTING
          container_name = CONV text40( mv_cont_name ).
    ENDIF.


    create_alv( ).

    add_functions( ).
    optimize_columns( ).
    register_events( ).



    mo_alv_table->display( ).

  ENDMETHOD.

  METHOD add_functions.
*... §3.1 activate ALV generic Functions
    DATA: lr_functions TYPE REF TO cl_salv_functions,
          l_text       TYPE string,
          l_icon       TYPE string.

    lr_functions = mo_alv_table->get_functions( ).
    lr_functions->set_all( abap_true ).

*... §3.2 include own functions
    l_text = me->mv_func_name.
    l_icon = icon_complete.
    TRY.
        lr_functions->add_function(
          name     = 'FUNC1'
          icon     = l_icon
          text     = l_text
          tooltip  = l_text
          position = if_salv_c_function_position=>right_of_salv_functions ).
      CATCH cx_salv_existing cx_salv_wrong_call.
    ENDTRY.
  ENDMETHOD.

  METHOD optimize_columns.
*... set the columns technical
    DATA: lr_columns TYPE REF TO cl_salv_columns,
          lr_column  TYPE REF TO cl_salv_column_table.

    lr_columns = mo_alv_table->get_columns( ).
    lr_columns->set_optimize( abap_true ).

*... §4 set hotspot column
    TRY.
        lr_column ?= lr_columns->get_column( 'MENGE' ).
        lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.

  METHOD register_events.
*... §6 register to the events of cl_salv_table
    DATA: lr_events TYPE REF TO cl_salv_events_table.

    lr_events = mo_alv_table->get_event( ).

*... §6.1 register to the event USER_COMMAND
    SET HANDLER on_user_command FOR lr_events.

    SET HANDLER on_before_user_command FOR lr_events.

    SET HANDLER on_after_user_command FOR lr_events.
    SET HANDLER handle_double_click FOR lr_events.
    SET HANDLER on_link_click FOR lr_events.

*... set list title
    DATA: lr_display_settings TYPE REF TO cl_salv_display_settings,
          l_title             TYPE lvc_title.

    l_title = me->mv_title.
    lr_display_settings = mo_alv_table->get_display_settings( ).
    lr_display_settings->set_list_header( l_title ).
  ENDMETHOD.

  METHOD on_user_command.

  ENDMETHOD.                    "on_user_command

  METHOD on_before_user_command.

  ENDMETHOD.                    "on_before_user_command

  METHOD on_after_user_command.

  ENDMETHOD.                    "on_after_user_command


  METHOD handle_double_click.

  ENDMETHOD.

  METHOD  on_link_click.

  ENDMETHOD.

  METHOD set_meta.
*      IMPORTING iv_cont_name TYPE string
*                iv_title TYPE string
*                iv_func_name TYPE string.

    mv_cont_name = iv_cont_name.
    mv_title = iv_title.
    mv_func_name = iv_func_name.
  ENDMETHOD.

  METHOD do_refresh.
    mo_alv_table->refresh(
*      EXPORTING
*        s_stable     =                         " ALV Control: Refresh Stability
*        refresh_mode = if_salv_c_refresh=>soft " ALV: Data Element for Constants
    ).
  ENDMETHOD.

ENDCLASS.
