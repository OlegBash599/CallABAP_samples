*&---------------------------------------------------------------------*
*& Include          LZLSP012_MEDIATORP05
*&---------------------------------------------------------------------*

CLASS lcl_colleague_log IMPLEMENTATION.
  METHOD constructor.
    CREATE OBJECT mo_container
      EXPORTING
        container_name = 'ZBR'.
  ENDMETHOD.

  METHOD fill_block.
    show_cont( ).
  ENDMETHOD.

  METHOD add_line2log.
    "      IMPORTING iv_line TYPE string.
    zcl_lsp012_html=>get_instance(
*         iv_title =
    )->add_para_val(
      EXPORTING
        iv_id    = iv_src
        iv_value = CONV text200( iv_line )
    ).
  ENDMETHOD.

  METHOD show_cont.
    zcl_lsp012_html=>get_instance( )->show_in_container( io_container = mo_container ).
  ENDMETHOD.

  METHOD do_refresh.
    show_cont( ).
  ENDMETHOD.

ENDCLASS.
