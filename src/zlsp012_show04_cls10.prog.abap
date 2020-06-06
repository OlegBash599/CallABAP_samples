*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW04_CLS10
*&---------------------------------------------------------------------*

INTERFACE lif_facade.
  TYPES: BEGIN OF ts_proc_info
          , step_num TYPE num6
          , step_descr TYPE text20
          , step_comment TYPE string
        , END OF ts_proc_info
        , tt_proc_info TYPE STANDARD TABLE OF ts_proc_info
          WITH DEFAULT KEY
        .

  DATA mv_proc TYPE string.

  METHODS main
   CHANGING ct_steps TYPE tt_proc_info.

ENDINTERFACE.                    "lif_facade

*----------------------------------------------------------------------*
*       CLASS lcl_doc_creation_prep DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_doc_creation_prep DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_facade.

    METHODS constructor.


  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.                    "lcl_doc_creation_prep DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_doc_creation_do DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_doc_creation_do DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_facade.

    METHODS constructor.


  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.                    "lcl_doc_creation_do DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_doc_send_via_xi DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_doc_send_via_xi DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_facade.

    METHODS constructor.


  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.                    "lcl_doc_send_via_xi DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_doc_inform DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_doc_inform DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_facade.

    METHODS constructor.


  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.                    "lcl_doc_inform DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_all_steps DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_all_steps DEFINITION.


  PUBLIC SECTION.
    INTERFACES lif_facade.

    METHODS constructor.


  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.                    "lcl_all_steps DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_doc_creation_prep IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_doc_creation_prep IMPLEMENTATION.
  METHOD constructor.
    lif_facade~mv_proc = 'Подготовка/ПредВыборка'.
  ENDMETHOD.                    "constructor

  METHOD lif_facade~main.
    DATA ls_proc_info TYPE lif_facade=>ts_proc_info.
    DATA lv_line TYPE sytabix.

    lv_line = lines( ct_steps ).

    lv_line = lv_line + 1.
    ls_proc_info-step_num = lv_line.
    ls_proc_info-step_descr = lif_facade~mv_proc.
    ls_proc_info-step_comment = 'Выбираем из таблица для создания документов'.
    APPEND ls_proc_info TO ct_steps.

    lv_line = lv_line + 1.
    ls_proc_info-step_num = lv_line.
    ls_proc_info-step_descr = lif_facade~mv_proc.
    ls_proc_info-step_comment = 'Анализируем и агрегируем данные'.
    APPEND ls_proc_info TO ct_steps.

    lv_line = lv_line + 1.
    ls_proc_info-step_num = lv_line.
    ls_proc_info-step_descr = lif_facade~mv_proc.
    ls_proc_info-step_comment = 'Проверяем полномочия'.
    APPEND ls_proc_info TO ct_steps.

  ENDMETHOD.                    "lif_facade~main
ENDCLASS.                    "lcl_doc_creation_prep IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_doc_creation_do IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_doc_creation_do IMPLEMENTATION.
  METHOD constructor.
    lif_facade~mv_proc = 'Создание документов'.
  ENDMETHOD.                    "constructor

  METHOD lif_facade~main.
    DATA ls_proc_info TYPE lif_facade=>ts_proc_info.
    DATA lv_line TYPE sytabix.

    lv_line = lines( ct_steps ).

    lv_line = lv_line + 1.
    ls_proc_info-step_num = lv_line.
    ls_proc_info-step_descr = lif_facade~mv_proc.
    ls_proc_info-step_comment = 'Создание 1го документа в цепочки'.
    APPEND ls_proc_info TO ct_steps.

    lv_line = lv_line + 1.
    ls_proc_info-step_num = lv_line.
    ls_proc_info-step_descr = lif_facade~mv_proc.
    ls_proc_info-step_comment = 'Создание 2го документа в цепочки'.
    APPEND ls_proc_info TO ct_steps.

    lv_line = lv_line + 1.
    ls_proc_info-step_num = lv_line.
    ls_proc_info-step_descr = lif_facade~mv_proc.
    ls_proc_info-step_comment = 'Создание 2го документа в цепочки'.
    APPEND ls_proc_info TO ct_steps.

  ENDMETHOD.                    "lif_facade~main
ENDCLASS.                    "lcl_doc_creation_do IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_doc_send_via_xi IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_doc_send_via_xi IMPLEMENTATION.
  METHOD constructor.
    lif_facade~mv_proc = 'Отправка данных через XI'.
  ENDMETHOD.                    "constructor

  METHOD lif_facade~main.
    DATA ls_proc_info TYPE lif_facade=>ts_proc_info.
    DATA lv_line TYPE sytabix.

    lv_line = lines( ct_steps ).

    lv_line = lv_line + 1.
    ls_proc_info-step_num = lv_line.
    ls_proc_info-step_descr = lif_facade~mv_proc.
    ls_proc_info-step_comment = 'Сбор данных по созданным документам'.
    APPEND ls_proc_info TO ct_steps.

    lv_line = lv_line + 1.
    ls_proc_info-step_num = lv_line.
    ls_proc_info-step_descr = lif_facade~mv_proc.
    ls_proc_info-step_comment = 'Подгтовка данных для интерфейса'.
    APPEND ls_proc_info TO ct_steps.

    lv_line = lv_line + 1.
    ls_proc_info-step_num = lv_line.
    ls_proc_info-step_descr = lif_facade~mv_proc.
    ls_proc_info-step_comment = 'Вызов proxy'.
    APPEND ls_proc_info TO ct_steps.

  ENDMETHOD.                    "lif_facade~main
ENDCLASS.                    "lcl_doc_send_via_xi IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_doc_inform IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_doc_inform IMPLEMENTATION.
  METHOD constructor.
    lif_facade~mv_proc = 'Информирование и печатные формы'.
  ENDMETHOD.                    "constructor

  METHOD lif_facade~main.
    DATA ls_proc_info TYPE lif_facade=>ts_proc_info.
    DATA lv_line TYPE sytabix.

    lv_line = lines( ct_steps ).

    lv_line = lv_line + 1.
    ls_proc_info-step_num = lv_line.
    ls_proc_info-step_descr = lif_facade~mv_proc.
    ls_proc_info-step_comment = 'Генерация печатных форм'.
    APPEND ls_proc_info TO ct_steps.

    lv_line = lv_line + 1.
    ls_proc_info-step_num = lv_line.
    ls_proc_info-step_descr = lif_facade~mv_proc.
    ls_proc_info-step_comment = 'Рассылка пользователям результатов'.
    APPEND ls_proc_info TO ct_steps.

    lv_line = lv_line + 1.
    ls_proc_info-step_num = lv_line.
    ls_proc_info-step_descr = lif_facade~mv_proc.
    ls_proc_info-step_comment = 'Отправка на принтер в кабинете 354'.
    APPEND ls_proc_info TO ct_steps.

  ENDMETHOD.                    "lif_facade~main
ENDCLASS.                    "lcl_doc_inform IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_doc_inform IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_all_steps IMPLEMENTATION.
  METHOD constructor.
    lif_facade~mv_proc = 'Все шаги'.
  ENDMETHOD.                    "constructor

  METHOD lif_facade~main.
    DATA lo_prep TYPE REF TO lcl_doc_creation_prep.
    DATA lo_create_doc TYPE REF TO lcl_doc_creation_do.
    DATA lo_xi TYPE REF TO lcl_doc_send_via_xi.
    DATA lo_info_mail TYPE REF TO lcl_doc_inform.

    DATA lt_steps TYPE lif_facade~tt_proc_info.

    CREATE OBJECT lo_prep.
    lo_prep->lif_facade~main(
      CHANGING
        ct_steps = lt_steps
    ).


    CREATE OBJECT lo_create_doc.
    lo_create_doc->lif_facade~main(
      CHANGING
        ct_steps = lt_steps
    ).


    CREATE OBJECT lo_xi.
    lo_xi->lif_facade~main(
      CHANGING
        ct_steps = lt_steps
    ).

    CREATE OBJECT lo_info_mail.
    lo_info_mail->lif_facade~main(
      CHANGING
        ct_steps = lt_steps
    ).

    ct_steps = lt_steps.

  ENDMETHOD.                    "lif_facade~main
ENDCLASS.                    "lcl_doc_inform IMPLEMENTATION


"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
"""""""""""""""""
*----------------------------------------------------------------------*
*       CLASS lcl_facade DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_facade DEFINITION.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS main.
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS run_step1.


ENDCLASS.                    "lcl_facade DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_facade IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_facade IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD.                    "constructor

  METHOD main.
    DATA mo_all_operation TYPE REF TO lcl_all_steps.
    DATA lt_steps TYPE lif_facade=>tt_proc_info.

    CREATE OBJECT mo_all_operation.

    mo_all_operation->lif_facade~main(
      CHANGING
        ct_steps = lt_steps
    ).

    "zcl_lsp010_html=>get_instance( )->add_tab_ch( it_tab = lt_steps )->show( ).
    zcl_lsp012_html=>get_instance( )->add_tab_ch( it_tab = lt_steps )->show( ).

  ENDMETHOD.                    "main

  METHOD run_step1.
  ENDMETHOD.                    "run_step1

ENDCLASS.
