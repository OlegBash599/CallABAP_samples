*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW04_CLS22
*&---------------------------------------------------------------------*

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" форматирование текста материала для различных целей
"" основной класс читает данные, связанные с материалом\
"" 3 других собирают различный формат

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_visitor DEFINITION.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS main.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS run_step1.
    METHODS run_step2.
    METHODS run_step3.


ENDCLASS.

CLASS lcl_visitor IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD.                    "constructor

  METHOD main.
    run_step1( ).
    run_step2( ).
    run_step3( ).
  ENDMETHOD.                    "main

  METHOD run_step1.


  ENDMETHOD.                    "run_step1


  METHOD run_step2.

  ENDMETHOD.

  METHOD run_step3.

  ENDMETHOD.

ENDCLASS.
