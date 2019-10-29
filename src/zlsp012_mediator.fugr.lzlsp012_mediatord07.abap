*&---------------------------------------------------------------------*
*& Include          LZLSP012_MEDIATORD07
*&---------------------------------------------------------------------*

CLASS lcl_colleague_alv DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS fill_block FINAL.


    METHODS do_refresh.
  PROTECTED SECTION.

    DATA mo_mediator TYPE REF TO lif_mediator.
    DATA mo_container TYPE REF TO cl_gui_custom_container.
    DATA mo_alv_table   TYPE REF TO cl_salv_table.

    DATA mv_cont_name TYPE string.
    DATA   mv_title TYPE string.
    DATA   mv_func_name TYPE string.

*    DATA mt_cons_stock TYPE tt_cons_stock.
*    DATA ms_cons_stock TYPE ts_cons_stock.

    METHODS create_alv ABSTRACT.
    METHODS add_functions FINAL.
    METHODS optimize_columns FINAL.
    METHODS register_events FINAL.

    METHODS set_meta
      IMPORTING iv_cont_name TYPE string
                iv_title     TYPE string
                iv_func_name TYPE string.




    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,
      on_before_user_command FOR EVENT before_salv_function OF cl_salv_events
        IMPORTING e_salv_function,
      on_after_user_command FOR EVENT after_salv_function OF cl_salv_events
        IMPORTING e_salv_function.

    METHODS handle_double_click
      FOR EVENT double_click
                OF cl_salv_events_table
      IMPORTING row column.

    METHODS:
      on_link_click FOR EVENT link_click  "Hotspot Handler
                  OF cl_salv_events_table
        IMPORTING row column
        .

  PRIVATE SECTION.

ENDCLASS.
