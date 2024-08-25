*&---------------------------------------------------------------------*
*& Report ZREP_C8A016_CLEAR_VS_FREE
*&---------------------------------------------------------------------*
*& FREEDUROV
*&---------------------------------------------------------------------*
REPORT zrep_c8a014_clear_vs_free.

CLASS lcl_clear_var DEFINITION.

  PUBLIC SECTION.
    METHODS constructor.

    METHODS start_of_sel.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS _clear_sample.

    METHODS _free_sample.

    METHODs _free_clear_itab.

ENDCLASS.

CLASS lcl_clear_var IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.

  METHOD start_of_sel.

    _clear_sample( ).
    _free_sample( ).

    _free_clear_itab( ).

  ENDMETHOD.

  METHOD _clear_sample.

    DATA lv_clear_string TYPE string.
    DATA lv_clear_string_sub TYPE string.
    DATA lv_clear_numc TYPE n LENGTH 6 VALUE '240825'.

    lv_clear_string = 'You should not go to France'.
    lv_clear_string_sub = 'You should not go to France'.

    CLEAR lv_clear_string.
    CLEAR lv_clear_string_sub WITH '#'.

    clear lv_clear_numc WITH '2'.

  ENDMETHOD.

  METHOD _free_sample.
    DATA lv_clear_string TYPE string.
    DATA lv_clear_string_sub TYPE string.
    DATA lv_clear_numc TYPE n LENGTH 6 VALUE '240825'.

    lv_clear_string = 'You should not go to France'.
    lv_clear_string_sub = 'You should not go to France'.

    free:
      lv_clear_string,   lv_clear_string_sub, lv_clear_numc.

  endmethod.

  METHOD _free_clear_itab.

    data lt_tab_clear TYPE stringtab.
    data lt_tab_free TYPE stringtab.

    lt_tab_clear = value #(
    ( `You should not go to France` )
    ( `Is it clear?` )
    ( `Yes, it's clear!` )
    ( ` - answered Pavel` )
    ).

    lt_tab_free = value #(
    ( `Free` )
    ( `Pavel` )
    ( `Durov` )
    ( ` - said Musk in Twitter` )
    ).

    clear lt_tab_clear.
    free lt_tab_free.

  ENDMETHOD.

ENDCLASS.

DATA go_clear_var TYPE REF TO lcl_clear_var.


INITIALIZATION.
  go_clear_var = NEW #( ).

START-OF-SELECTION.
  go_clear_var->start_of_sel( ).

end-of-SELECTION.
