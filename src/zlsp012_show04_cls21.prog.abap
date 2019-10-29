*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW04_CLS21
*&---------------------------------------------------------------------*



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_read_from_stream DEFINITION ABSTRACT.
  PUBLIC SECTION.

    METHODS constructor.
    METHODS output_stream_in_table
      IMPORTING path2stream TYPE string.
    METHODS get_tab ABSTRACT
      EXPORTING et_tab TYPE any .
  PROTECTED SECTION.
    DATA mv_path2file TYPE string.

    METHODS read2tab ABSTRACT.
    METHODS parse_data ABSTRACT.
    METHODS show_data FINAL.

    METHODS hook_before_parse.
    METHODS hook_before_output.

  PRIVATE SECTION.


ENDCLASS.

CLASS lcl_read_from_stream IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.

  METHOD output_stream_in_table.
    mv_path2file = path2stream.
    read2tab( ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "" хук-методы (методы-крюки для зацепки)
    "" могут быть полезны для явного обозначения необходимости разделения
    hook_before_parse(  ).
    parse_data(  ).

    hook_before_output(  ).
    show_data(  ).
  ENDMETHOD.

  METHOD show_data.

  ENDMETHOD.

  METHOD hook_before_parse.

  ENDMETHOD.

  METHOD hook_before_output.

  ENDMETHOD.

ENDCLASS.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

CLASS lcl_read_from_xl DEFINITION INHERITING FROM lcl_read_from_stream.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS get_tab REDEFINITION.
  PROTECTED SECTION.
    METHODS read2tab REDEFINITION.
    METHODS parse_data REDEFINITION.

  PRIVATE SECTION.
    DATA mt_tab TYPE zexcel_t_cell_data.

ENDCLASS.

CLASS lcl_read_from_xl IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.

  METHOD read2tab.
    DATA excel                 TYPE REF TO zcl_excel.
    DATA reader                TYPE REF TO zif_excel_reader.
    DATA lo_iter TYPE REF TO cl_object_collection_iterator.
    DATA lo_worksheet TYPE REF TO zcl_excel_worksheet.



    TRY.
        CREATE OBJECT reader TYPE zcl_excel_reader_2007.
        excel = reader->load_file( i_from_applserver = abap_true i_filename = mv_path2file ).
        lo_iter = excel->get_worksheets_iterator( ).

        WHILE lo_iter->has_next( ).
          lo_worksheet ?= lo_iter->get_next( ).

          IF lo_worksheet->sheet_content[] IS INITIAL.
          ELSE.
            mt_tab = lo_worksheet->sheet_content[] .
            EXIT.
          ENDIF.

        ENDWHILE.

      CATCH zcx_excel.

    ENDTRY.


  ENDMETHOD.

  METHOD parse_data.

  ENDMETHOD.
  METHOD get_tab.
    et_tab = mt_tab.
  ENDMETHOD.
ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

CLASS lcl_read_from_txt DEFINITION INHERITING FROM lcl_read_from_stream.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS get_tab REDEFINITION.
  PROTECTED SECTION.
    METHODS read2tab REDEFINITION.
    METHODS parse_data REDEFINITION.

  PRIVATE SECTION.
    DATA mt_tab TYPE soli_tab.

ENDCLASS.

CLASS lcl_read_from_txt IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.

  METHOD read2tab.
    DATA lv_file_line TYPE soli.

    CLEAR mt_tab.

    OPEN DATASET mv_path2file FOR INPUT IN TEXT MODE ENCODING UTF-8.
    IF sy-subrc EQ 0.
      DO .
        READ DATASET  mv_path2file INTO lv_file_line.
        IF sy-subrc EQ 0.
          APPEND lv_file_line TO mt_tab.
        ELSE.
          EXIT.
        ENDIF.

      ENDDO.
      CLOSE DATASET mv_path2file.
    ENDIF.

  ENDMETHOD.

  METHOD parse_data.

  ENDMETHOD.

  METHOD get_tab.
    et_tab = mt_tab.
  ENDMETHOD.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_template_method DEFINITION.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS main.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS run_step1.
    METHODS run_step2.
    METHODS run_step3.

    METHODS get_read_stream
      IMPORTING iv_type       TYPE char10
      RETURNING VALUE(ro_obj) TYPE REF TO lcl_read_from_stream.

ENDCLASS.

CLASS lcl_template_method IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD.                    "constructor

  METHOD main.
    run_step1( ).
    run_step2( ).
    run_step3( ).
  ENDMETHOD.                    "main

  METHOD run_step1.
    DATA(lo_read_stream) = get_read_stream( 'TXT' ).
    " если файла нет - создать через nano или разработку по курса
    " ФМ для загрузки  ARCHIVFILE_CLIENT_TO_SERVER
    DATA lv_path2file TYPE string VALUE '/usr/sap/NPL/D00/work/ztmpl021.txt'.
    DATA lt_tab TYPE soli_tab.

    lo_read_stream->output_stream_in_table( path2stream = lv_path2file ).
    lo_read_stream->get_tab( IMPORTING et_tab = lt_tab ).
    zcl_lsp012_html=>get_instance( )->add_tab( it_tab = lt_tab ).

  ENDMETHOD.                    "run_step1


  METHOD run_step2.
    DATA(lo_read_stream) = get_read_stream( 'XL' ).
    " если файла нет - создать через nano или разработку по курса
    " ФМ для загрузки  ARCHIVFILE_CLIENT_TO_SERVER
    DATA lv_path2file TYPE string VALUE '/usr/sap/NPL/D00/work/ztmpl021_xl.xlsx'.
    DATA lt_tab TYPE zexcel_t_cell_data.
    DATA lt_tab_std TYPE zexcel_t_cell_data_std.

    lo_read_stream->output_stream_in_table( path2stream = lv_path2file ).
    lo_read_stream->get_tab( IMPORTING et_tab = lt_tab ).
    lt_tab_std = lt_tab.
    zcl_lsp012_html=>get_instance( )->add_tab( it_tab = lt_tab_std ).

    zcl_lsp012_html=>get_instance( )->add_tab_ch( it_tab = lt_tab_std )->show( ).

  ENDMETHOD.

  METHOD get_read_stream.
*    METHODS get_read_stream
*        IMPORTING iv_type TYPE char10
*        RETURNING VALUE(ro_obj) TYPE REF TO lcl_read_from_stream.
    DATA lv_clsname TYPE seoclsname VALUE 'lcl_read_from_txt'.
    CASE iv_type.
      WHEN 'XL'.
        lv_clsname = 'LCL_READ_FROM_XL'.
      WHEN 'TXT'.
        lv_clsname = 'LCL_READ_FROM_TXT'.
    ENDCASE.
    TRANSLATE lv_clsname TO UPPER CASE.
    CREATE OBJECT ro_obj TYPE (lv_clsname).
  ENDMETHOD.

  METHOD run_step3.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " обратить внимание именно на постороение ALV блоков
    " на использование ключевых слов FINAL и ABSTRACT
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'Z_LSP012_SHOW_ONLINE_FLOW'.
  ENDMETHOD.

ENDCLASS.
