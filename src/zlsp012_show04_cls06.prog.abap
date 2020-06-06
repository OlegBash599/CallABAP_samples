*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW04_CLS06
*&---------------------------------------------------------------------*


" показываем данные из XML в форме таблицы

*----------------------------------------------------------------------*
*       CLASS lcl_xml2itab DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_xml2itab DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS get_xml.
    METHODS show.
  PROTECTED SECTION.

  PRIVATE SECTION.

"    DATA mo_html TYPE REF TO zcl_lsp010_html.
    DATA mo_html TYPE REF TO zcl_lsp012_html.
    DATA mv_xml_str TYPE string.
    DATA mv_xml_xstr TYPE xstring.

    DATA mv_path2fold TYPE string
            VALUE '/usr/sap/UDS/DVEBMGS01/work/'.

    DATA mv_path2xml TYPE string
            VALUE '/usr/sap/NPL/D00/work/adapter_show.xml'.

    METHODS  fill_init_data.

ENDCLASS.                    "lcl_xml2itab DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_xml2itab IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_xml2itab IMPLEMENTATION.
  METHOD constructor.
    fill_init_data( ).

"    mo_html = zcl_lsp010_html=>get_instance( ).
    mo_html = zcl_lsp012_html=>get_instance( ).

  ENDMETHOD.                    "constructor

  METHOD fill_init_data.
    DATA lt_sflight TYPE STANDARD TABLE OF sflight.
    DATA ls_sflight TYPE sflight.

    DATA lv_xml TYPE string.

    SELECT * FROM sflight
      INTO TABLE lt_sflight
      UP TO 1000 ROWS.


    CALL TRANSFORMATION id SOURCE sflight = lt_sflight[]
                           RESULT XML lv_xml.



    OPEN DATASET mv_path2xml FOR OUTPUT IN TEXT MODE ENCODING UTF-8.

    TRANSFER lv_xml TO mv_path2xml.

    CLOSE DATASET mv_path2xml.

  ENDMETHOD.                    "fill_init_data

  METHOD get_xml.
    DATA lv_xml TYPE string.
    DATA lt_xml TYPE STANDARD TABLE OF string.
    FIELD-SYMBOLS <fs_xml> TYPE string.

*    OPEN DATASET mv_path2xml FOR INPUT IN TEXT MODE ENCODING UTF-8.
*
*    CLEAR lv_xml.
*    CLEAR lt_xml.
*    DO.
*      READ DATASET mv_path2xml INTO lv_xml.
*      IF sy-subrc EQ 0.
*        APPEND lv_xml TO lt_xml.
*        CLEAR lv_xml.
*      ELSE.
*        EXIT.
*      ENDIF.
*    ENDDO.
*
*    LOOP AT lt_xml ASSIGNING <fs_xml>.
*      CASE sy-tabix.
*        WHEN 1.
*          lv_xml = <fs_xml>.
*        WHEN OTHERS.
*          lv_xml = lv_xml && <fs_xml>.
*      ENDCASE.
*    ENDLOOP.
*
*    CLEAR mv_xml_str.
*    mv_xml_str = lv_xml.




    OPEN DATASET mv_path2xml FOR INPUT IN BINARY MODE.

    CLEAR me->mv_xml_xstr.

    READ DATASET mv_path2xml INTO me->mv_xml_xstr.
    IF sy-subrc EQ 0.

    ELSE.
      EXIT.
    ENDIF.


    CLOSE DATASET mv_path2xml.
  ENDMETHOD.                    "get_xml

  METHOD show.

    DATA lt_sflight TYPE STANDARD TABLE OF sflight.

    "CALL TRANSFORMATION id SOURCE xml = mv_xml_str
    "                       RESULT node = lt_sflight.
    "CALL TRANSFORMATION ('ID') SOURCE xml mv_xml_str "me->mv_xml_xstr
    CALL TRANSFORMATION ('ID') SOURCE XML me->mv_xml_xstr
                           RESULT sflight = lt_sflight[].

    " method chaining
    mo_html->add_tab_ch( it_tab = lt_sflight )->show( ).

  ENDMETHOD.                    "show

ENDCLASS.                    "lcl_xml2itab IMPLEMENTATION

" показываем данные из таблицы в форме xml
*----------------------------------------------------------------------*
*       CLASS lcl_itab2xml DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_itab2xml DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.                    "lcl_itab2xml DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_itab2xml IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_itab2xml IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.                    "constructor
ENDCLASS.                    "lcl_itab2xml IMPLEMENTATION





"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
*----------------------------------------------------------------------*
*       CLASS lcl_adapter DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_adapter DEFINITION.
  PUBLIC SECTION.

    METHODS constructor.
    METHODS main.
  PROTECTED SECTION.
  PRIVATE SECTION.


    METHODS run_step1.
ENDCLASS.                    "lcl_adapter DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_adapter IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_adapter IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.                    "constructor

  METHOD main.

    DATA lo_xml2itab TYPE REF TO lcl_xml2itab.
    CREATE OBJECT lo_xml2itab.


    lo_xml2itab->get_xml( ).

    " клиент не знает, с какой службой по отображению XML он работает
    lo_xml2itab->show( ).
  ENDMETHOD.                    "main

  METHOD run_step1.
  ENDMETHOD.                    "run_step1
ENDCLASS.
