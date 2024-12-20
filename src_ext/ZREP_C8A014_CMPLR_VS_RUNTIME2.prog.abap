REPORT zrep_c8a014_cmplr_vs_runtime2.

CLASS lcl_app DEFINITION.

  PUBLIC SECTION.
    METHODS main.
  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES: BEGIN OF ts_name_val
        , key_name TYPE string
        , val TYPE string
        , add_value TYPE string
      ,  END OF ts_name_val
      , tt_name_val TYPE STANDARD TABLE OF ts_name_val WITH DEFAULT KEY
      , tt_name_val_srt TYPE SORTED TABLE OF ts_name_val WITH UNIQUE KEY key_name
      .

    DATA msr_name_val TYPE REF TO ts_name_val.

    METHODS fill_add_value
      CHANGING cs_name_val TYPE ts_name_val.

    METHODS fill_add_value_ref
      IMPORTING isr_name_val TYPE REF TO ts_name_val.

    METHODS fill_add_value_via_attr.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD main.
    DATA lt_name_val TYPE tt_name_val_srt.
    FIELD-SYMBOLS <fs_name_val> TYPE ts_name_val.

    lt_name_val = VALUE #(
    ( key_name = 'NUM100' val = 'base_val_100' )
    ( key_name = 'NUM102' val = 'base_val_102' )
    ).

*    LOOP AT lt_name_val ASSIGNING <fs_name_val>.
*      fill_add_value( CHANGING cs_name_val = <fs_name_val> ).
*    ENDLOOP.

    LOOP AT lt_name_val ASSIGNING <fs_name_val>.
      fill_add_value_ref( isr_name_val = REF #( <fs_name_val> ) ).
    ENDLOOP.

    " also could be used
    LOOP AT lt_name_val REFERENCE INTO msr_name_val.
      fill_add_value_via_attr( ).
    ENDLOOP.


  ENDMETHOD.

  METHOD fill_add_value.
    cs_name_val-add_value = 'some val'.
  ENDMETHOD.

  METHOD fill_add_value_ref.
    "IMPORTING isr_name_val TYPE REF TO ts_name_val.
    isr_name_val->add_value = 'some val'.
  ENDMETHOD.

  METHOD fill_add_value_via_attr.
    IF msr_name_val IS BOUND.
      msr_name_val->add_value  = 'some val'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW lcl_app( )->main( ).
