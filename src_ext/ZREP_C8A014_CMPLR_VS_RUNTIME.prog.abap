REPORT zrep_c8a014_cmplr_vs_runtime.
"" пример, которые вызывает runtime error 
"" MOVE_TO_LIT_NOTALLOWED_NODATA	CX_SY_DYN_CALL_ILLEGAL_TYPE

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

    METHODS fill_add_value
      CHANGING cs_name_val TYPE ts_name_val.

ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD main.
    DATA lt_name_val TYPE tt_name_val_srt.
    FIELD-SYMBOLS <fs_name_val> TYPE ts_name_val.

    lt_name_val = value #(
    ( key_name = 'NUM100' val = 'base_val_100' )
    ( key_name = 'NUM102' val = 'base_val_102' )
    ).

    LOOP AT lt_name_val ASSIGNING <fs_name_val>.
      fill_add_value( CHANGING cs_name_val = <fs_name_val> ).
    ENDLOOP.

  ENDMETHOD.

  METHOD fill_add_value.
    cs_name_val-add_value = 'some val'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  new lcl_app( )->main( ).
