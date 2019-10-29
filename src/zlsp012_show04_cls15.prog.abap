*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW04_CLS15
*&---------------------------------------------------------------------*


CLASS lcl_key_name DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_name TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.


    DATA mv_name TYPE string.
    DATA mt_name_val TYPE mestringmap.

    METHODS gen_data.

ENDCLASS.

CLASS lcl_key_name IMPLEMENTATION.
  METHOD constructor.
    mv_name = iv_name.
    gen_data(  ).
  ENDMETHOD.

  METHOD gen_data.

    mt_name_val =   VALUE #(
     FOR tab_line = 0 THEN tab_line + 1 UNTIL tab_line > 8
     ( name = |{ mv_name }_{ tab_line }|
       value = |val for { mv_name } { tab_line * 3 } | ) ) .

  ENDMETHOD.

ENDCLASS.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

CLASS lcl_string_ley DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_key TYPE string.



  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES: BEGIN OF ts_head_str
               , head_str TYPE text20
               , head_desr TYPE string
           , END OF ts_head_str
           , tt_head_str TYPE SORTED TABLE OF ts_head_str
            WITH NON-UNIQUE KEY head_str
           .

    TYPES: BEGIN OF ts_item_str
               , head_str TYPE text20
               , head_posnr TYPE posnr
               , detail TYPE string
           , END OF ts_item_str
           , tt_item_str TYPE SORTED TABLE OF ts_item_str
            WITH NON-UNIQUE KEY head_str
           .

    TYPES: BEGIN OF MESH ts_mesh,
              head_str TYPE tt_head_str
             ASSOCIATION str_hi
             TO item_str ON head_str = head_str,
             item_str TYPE tt_item_str
           , END OF MESH ts_mesh.


    DATA mv_key TYPE string.

    DATA mt_head_str TYPE tt_head_str.
    DATA mt_item_str TYPE tt_item_str.
    DATA ms_mesh TYPE ts_mesh.

    METHODS gen_data
      IMPORTING iv_prefix TYPE char4.

ENDCLASS.

CLASS lcl_string_ley IMPLEMENTATION.
  METHOD constructor.
    mv_key = iv_key.
    gen_data( iv_prefix = CONV char4( iv_key ) ).
  ENDMETHOD.

  METHOD gen_data.

    mt_head_str =   VALUE #(
      FOR tab_line = 0 THEN tab_line + 1 UNTIL tab_line > 10
      ( head_str = |{ mv_key }: { tab_line }|
        head_desr = |DESCR: { tab_line ** 2 } |
        ) ) .

    mt_item_str =   VALUE #(
      FOR tab_line = 0 THEN tab_line + 1 UNTIL tab_line > 30
      ( head_str = |{ mv_key }: { tab_line - ( tab_line MOD 3 ) }|
        head_posnr = tab_line
        detail = |DESC: { mv_key }: { tab_line ** 2 }|
        ) ) .

    ms_mesh-head_str[] = mt_head_str[].
    ms_mesh-item_str[] = mt_item_str[].

  ENDMETHOD.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


CLASS lcl_iterator DEFINITION.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS main.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS run_step1.
    METHODS run_step2.
    METHODS run_step3.


ENDCLASS.

CLASS lcl_iterator IMPLEMENTATION.

  METHOD constructor.

  ENDMETHOD.                    "constructor

  METHOD main.
    run_step1( ).
    run_step2( ).
    run_step3( ).
  ENDMETHOD.                    "main

  METHOD run_step1.
    " simplte standard iterator
    DATA(lo_obj_map) = NEW cl_object_map(  ).
    DATA lo_obj TYPE REF TO object.
    DATA lv_suffix TYPE string.

    DO 5 TIMES.
      lo_obj_map->put( key      = |SK{ sy-index }|
                       value    =  NEW lcl_key_name( |SK{ sy-index }| )  ).

      lo_obj_map->put( key      = |N{ sy-index }|
                        value    =  NEW lcl_string_ley( |N{ sy-index }| )  ).

    ENDDO.

    DATA(lo_iter) = lo_obj_map->get_values_iterator( ).

    WHILE lo_iter->has_next( ).
      lo_obj = lo_iter->get_next( ).

      IF lo_obj IS INSTANCE OF lcl_key_name.
        lv_suffix = 'lcl_key_name'.
      ENDIF.

      IF lo_obj IS INSTANCE OF lcl_string_ley.
        lv_suffix = 'lcl_string_ley'.
      ENDIF.


      zcl_lsp012_html=>get_instance( )->add_para_val(
        EXPORTING
          iv_id    = |SIMPLE { sy-index }|
          iv_value = | {  lv_suffix } { CONV text20( lo_iter->get_index( ) )  } |
      ).

    ENDWHILE.

  ENDMETHOD.                    "run_step1

  METHOD run_step2.

    DATA(lo_obj_map) = NEW zcl_lsp012_object_map(  ).
    DATA lo_obj TYPE REF TO object.
    DATA lv_suffix TYPE string.


    DO 5 TIMES.
      lo_obj_map->put( key      = |SK{ sy-index }|
                       value    =  NEW lcl_key_name( |SK{ sy-index }| )  ).

    ENDDO.

    DO 5 TIMES.
      lo_obj_map->put( key      = |N{ sy-index + 5 }|
                          value    =  NEW lcl_string_ley( |N{ sy-index + 5 }| )  ).
    ENDDO.

    DATA(lo_iter) = lo_obj_map->get_values_iterator( 'ZCL_OBJECT_COLLECTION_ITER135' ).

    WHILE lo_iter->has_next( ).
      lo_obj = lo_iter->get_next( ).

      IF lo_obj IS INSTANCE OF lcl_key_name.
        lv_suffix = 'lcl_key_name'.
      ENDIF.

      IF lo_obj IS INSTANCE OF lcl_string_ley.
        lv_suffix = 'lcl_string_ley'.
      ENDIF.


      zcl_lsp012_html=>get_instance( )->add_para_val(
        EXPORTING
          iv_id    = |1..2..3.. { sy-index }|
          iv_value = | {  lv_suffix } { CONV text20( lo_iter->get_index( ) )  } |
      ).

    ENDWHILE.


  ENDMETHOD.

  METHOD run_step3.

    zcl_lsp012_html=>get_instance( )->show( ).

  ENDMETHOD.

ENDCLASS.
