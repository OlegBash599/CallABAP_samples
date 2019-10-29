*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW04_CLS16
*&---------------------------------------------------------------------*

" mediator - посредник
INTERFACE lif_mediator.
  METHODS notify
    IMPORTING io_obj TYPE REF TO object.
ENDINTERFACE.


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_colleague_consignment DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.


ENDCLASS.

CLASS lcl_colleague_consignment IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.
ENDCLASS.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_colleague_total_stock DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.


ENDCLASS.

CLASS lcl_colleague_total_stock IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.
ENDCLASS.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_colleague_sales DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.


ENDCLASS.

CLASS lcl_colleague_sales IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.
ENDCLASS.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_colleague_log DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.


ENDCLASS.

CLASS lcl_colleague_log IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.
ENDCLASS.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

CLASS lcl_mediator_director DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.


ENDCLASS.

CLASS lcl_mediator_director IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.
ENDCLASS.


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_mediator DEFINITION.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS main.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_mediator_director TYPE REF TO lcl_mediator_director.

    METHODS run_step1.
    METHODS run_step2.
    METHODS run_step3.


ENDCLASS.

CLASS lcl_mediator IMPLEMENTATION.

  METHOD constructor.
    mo_mediator_director = NEW #(  ).
  ENDMETHOD.                    "constructor

  METHOD main.
    run_step1( ).
    run_step2( ).
    run_step3( ).
  ENDMETHOD.                    "main

  METHOD run_step1.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " обратить внимание именно на взаимодействие подэкранов между собой
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'Z_LSP012_SHOW_ONLINE_FLOW'.

  ENDMETHOD.                    "run_step1


  METHOD run_step2.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " timer
    " CL_GUI_TIMER
    " https://blogs.sap.com/2017/04/11/refresh-the-display-using-class-cl_gui_timer/
    " https://abap-blog.ru/sap-dynpro/autorun/

  ENDMETHOD.

  METHOD run_step3.

  ENDMETHOD.

ENDCLASS.
