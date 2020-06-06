*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW04_CLS09
*&---------------------------------------------------------------------*


INTERFACE lif_ty.
  TYPES: BEGIN OF ts_id_val
    , area TYPE string
    , id TYPE text20
    , val TYPE text255
   , END OF ts_id_val
   , tt_id_val TYPE STANDARD TABLE OF ts_id_val
    WITH DEFAULT KEY
   .
ENDINTERFACE.                    "lif_ty

*----------------------------------------------------------------------*
*       CLASS lcl_xml DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_xml DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES lif_ty.
    ALIASES ts_id_val FOR lif_ty~ts_id_val.
    ALIASES tt_id_val FOR lif_ty~tt_id_val.

    METHODS constructor
      IMPORTING iv_id TYPE text10 DEFAULT 'XML'.

    METHODS get_xml_str
      RETURNING value(rv_xml) TYPE string.

  PROTECTED SECTION.
    DATA mv_id TYPE text10.
    DATA mv_xml TYPE string.
    DATA mt_id_val TYPE tt_id_val.

    METHODS fill_data.
    METHODS gen_xml.

  PRIVATE SECTION.



ENDCLASS.                    "lcl_xml DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_xml IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_xml IMPLEMENTATION.
  METHOD constructor.
    mv_id =  iv_id.
    "fill_data( ).
  ENDMETHOD.                    "constructor

  METHOD get_xml_str.
    gen_xml( ).
    rv_xml = mv_xml.
  ENDMETHOD.                    "get_xml_str

  METHOD fill_data.
    DATA ls_id_val TYPE ts_id_val.

    DO 10 TIMES.
      ls_id_val-area = |'ZZZ{ sy-index * 10 }'|.
      ls_id_val-id = |'ZZZ_{ sy-index * 10 }_ID'|.
      ls_id_val-val = |'ZZZ_{ sy-index * 10 }_VAL'|.

      APPEND ls_id_val TO mt_id_val.
    ENDDO.
  ENDMETHOD.                    "fill_data

  METHOD gen_xml.
    CALL TRANSFORMATION id SOURCE node = mt_id_val
                           RESULT XML mv_xml.
  ENDMETHOD.                    "gen_xml
ENDCLASS.                    "lcl_xml IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_xml_concrete DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_xml_concrete DEFINITION INHERITING FROM lcl_xml.

ENDCLASS.                    "lcl_xml_concrete DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_xml_concrete IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_xml_concrete IMPLEMENTATION.

ENDCLASS.                    "lcl_xml_concrete IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_base_decor DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_base_decor DEFINITION ABSTRACT INHERITING FROM lcl_xml.
  PUBLIC SECTION.

    METHODS constructor
      IMPORTING io_xml TYPE REF TO lcl_xml.

  PROTECTED SECTION.
    DATA mo_xml TYPE REF TO lcl_xml.



  PRIVATE SECTION.

ENDCLASS.                    "lcl_base_decor DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_base_decor IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_base_decor IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mo_xml ?= io_xml.
  ENDMETHOD.                    "constructor
ENDCLASS.                    "lcl_base_decor IMPLEMENTATION



*----------------------------------------------------------------------*
*       CLASS lcl_meta_decor DEFINITION
*----------------------------------------------------------------------*
* " декоратор для добавления меданных в xml
*----------------------------------------------------------------------*
CLASS lcl_meta_decor DEFINITION INHERITING FROM lcl_base_decor.
  PUBLIC SECTION.
    METHODS get_xml_str REDEFINITION.
  PROTECTED SECTION.

    METHODS add_data .


  PRIVATE SECTION.

ENDCLASS.                    "lcl_meta_decor DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_fs_decor DEFINITION
*----------------------------------------------------------------------*
*" декоратор для сохранения данных на AS и прописывание пути в XML
*----------------------------------------------------------------------*
CLASS lcl_fs_decor DEFINITION INHERITING FROM lcl_base_decor.
  PUBLIC SECTION.
    METHODS get_xml_str REDEFINITION.
  PROTECTED SECTION.

    DATA mv_path2file TYPE string VALUE '/usr/sap/UDS/DVEBMGS01/work/zlsp010_decor_fs.xml'.

    METHODS add_data .

  PRIVATE SECTION.
ENDCLASS.                    "lcl_fs_decor DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_meta_decor IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_meta_decor IMPLEMENTATION.
  METHOD get_xml_str.
    IF me->mt_id_val[] is INITIAL.
      me->fill_data( ).
    ENDIF.

    me->add_data( ).

    rv_xml = mo_xml->get_xml_str( ).
  ENDMETHOD.                    "get_xml_str

  METHOD add_data.
    DATA ls_id_val TYPE ts_id_val.


    ls_id_val-area = |'DECOR_META'|.
    ls_id_val-id = |CURRENT_DATE|.
    ls_id_val-val = |{ sy-datum }|.
    APPEND ls_id_val TO me->mt_id_val.

    ls_id_val-area = |'DECOR_META'|.
    ls_id_val-id = |CURRENT_TIME|.
    ls_id_val-val = |{ sy-uzeit }|.
    APPEND ls_id_val TO me->mt_id_val.

    APPEND LINES OF me->mt_id_val TO mo_xml->mt_id_val.
  ENDMETHOD.                    "fill_data
ENDCLASS.                    "lcl_meta_decor IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_fs_decor IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_fs_decor IMPLEMENTATION.


  METHOD add_data.
    DATA ls_id_val TYPE ts_id_val.
    DATA lt_id_val TYPE tt_id_val.

    ls_id_val-area = |'DECOR_FS'|.
    ls_id_val-id = |FILE_PATH|.
    ls_id_val-val = |{ mv_path2file }|.
    APPEND ls_id_val TO me->mt_id_val.

    append LINES OF me->mt_id_val to mo_xml->mt_id_val.

    gen_xml( ).

*    OPEN DATASET mv_path2file FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
*
*    TRANSFER mv_xml TO mv_path2file.
*
*    CLOSE DATASET mv_path2file.
  ENDMETHOD.                    "add_data

  METHOD get_xml_str.
    IF me->mt_id_val[] is INITIAL.
      me->fill_data( ).
    ENDIF.
    me->add_data( ).

    rv_xml = mo_xml->get_xml_str( ).
  ENDMETHOD.                    "get_xml_str
ENDCLASS.                    "lcl_fs_decor IMPLEMENTATION

"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
*----------------------------------------------------------------------*
*       CLASS lcl_decorator DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_decorator DEFINITION.

  PUBLIC SECTION.
    INTERFACES lif_ty.
    ALIASES ts_id_val FOR lif_ty~ts_id_val.
    ALIASES tt_id_val FOR lif_ty~tt_id_val.



    METHODS constructor.
    METHODS main.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_id_val TYPE tt_id_val.

    METHODS run_step1.


ENDCLASS.                    "lcl_decorator DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_decorator IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_decorator IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD.                    "constructor

  METHOD main.
    "DATA lo_xml TYPE REF TO lcl_xml_concrete.
    DATA lo_xml TYPE REF TO lcl_xml.
    DATA lo_xml_abs TYPE REF TO lcl_xml.

    DATA lo_decor_meta TYPE REF TO lcl_meta_decor.
    DATA lo_decor_fs TYPE REF TO lcl_fs_decor.

    DATA lv_xml_str TYPE string.

    CREATE OBJECT lo_xml_abs TYPE lcl_xml_concrete.
    lo_xml = lo_xml_abs.


    CREATE OBJECT lo_decor_fs
      EXPORTING
        io_xml = lo_xml.


    CREATE OBJECT lo_decor_meta
      EXPORTING
        "io_xml = lo_decor_META. " само-рекурсия = дамп (показать как минус декторатора)
        io_xml = lo_decor_fs.


    "lo_xml ?= lo_decor_fs.
    lo_xml ?= lo_decor_meta.


    lv_xml_str = lo_xml->get_xml_str( ).

    IF lv_xml_str IS INITIAL.
"      zcl_lsp010_html=>get_instance( )->add_para_val_ch(
      zcl_lsp012_html=>get_instance( )->add_para_val_ch(
          iv_id    = 'NO_DATA'
          iv_value = 'Нет данных в XML'
      )->show( ).
    ELSE.
      " zcl_lsp010_html=>get_instance( )->show_xml( iv_xml = lv_xml_str ).
       zcl_lsp012_html=>get_instance( )->show_xml( iv_xml = lv_xml_str ).
    ENDIF.


  ENDMETHOD.                    "main

  METHOD run_step1.


  ENDMETHOD.                    "run_step1

ENDCLASS.
