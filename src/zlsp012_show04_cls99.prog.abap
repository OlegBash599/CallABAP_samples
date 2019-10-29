*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW04_CLS99
*&---------------------------------------------------------------------*



CLASS lcl_app DEFINITION.

  PUBLIC SECTION.

    INTERFACES lif_app.

    ALIASES c_onli FOR zif_lsp012_app~c_onli.

    METHODS constructor.
    METHODS at_sel_scr_out.


  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA ms_screen TYPE ts_screen.
    DATA mv_string TYPE string.


    """"" creational patterns{
    METHODS singleton.
    METHODS factory_method.
    METHODS abstract_factory.
    METHODS builder.
    METHODS prototype.
    """"" creational patterns}
    "=========================================
    """"" structural patterns{
    METHODS adapter.
    METHODS bridge.
    METHODS composite. " компоновщик
    METHODS decorator. " декоратор
    METHODS facade. " фасад
    METHODS flyweight. " приспосбленец , кэш
    METHODS proxy. " заместитель / прокси

    """"" structural patterns}

    """"" behavioral patterns{
    METHODS chain_of_responsibility.
    METHODS command.
    METHODS iterator.
    METHODS mediator. " посрединик
    METHODS memento. " снимок / хранитель
    METHODS observer. " наблюдатель
    METHODS state. " состояние
    METHODS strategy. " стратегия
    METHODS template_method. " шаблонный метод
    METHODS visitor. " посетитель

    """"" behavioral patterns}

ENDCLASS.                    "lcl_app DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_app IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_app IMPLEMENTATION.

  METHOD constructor.

  ENDMETHOD.                    "constructor

  METHOD at_sel_scr_out.

*    LOOP AT SCREEN.
*
*      IF screen-group1 EQ 'QR1'.
*        IF ms_screen-rq EQ abap_true.
*          screen-active = 1.
*        ELSE.
*          screen-active = 0.
*        ENDIF.
*      ENDIF.
*      MODIFY SCREEN.
*
*    ENDLOOP.
  ENDMETHOD.                    "at_sel_scr_out

  METHOD zif_lsp012_app~set_scr.
    ms_screen = is_scr.
    CASE  iv_syucomm.
      WHEN c_onli.
        me->zif_lsp012_app~do_check_before( ).
      WHEN OTHERS.
    ENDCASE.


  ENDMETHOD.                    "set_scr

  METHOD zif_lsp012_app~process.
    BREAK-POINT.
    CASE abap_true.
      WHEN ms_screen-r_absfac.
        me->abstract_factory( ).
      WHEN ms_screen-r_single.
        me->singleton( ).
      WHEN ms_screen-r_facmet.
        me->factory_method( ).
      WHEN ms_screen-r_build.
        me->builder( ).
      WHEN ms_screen-r_proto.
        me->prototype( ).

      WHEN ms_screen-r_adapt.
        me->adapter( ).
      WHEN ms_screen-r_bridg.
        me->bridge( ).
      WHEN ms_screen-r_compo.
        me->composite( ).
      WHEN ms_screen-r_decor.
        me->decorator( ).
      WHEN ms_screen-r_facad.
        me->facade( ).
      WHEN ms_screen-r_flywei.
        me->flyweight( ).
      WHEN ms_screen-r_proxy.
        me->proxy( ).


      WHEN ms_screen-r_chofr.
        me->chain_of_responsibility( ).
      WHEN ms_screen-r_cmd.
        me->command( ).
      WHEN ms_screen-r_itera.
        me->iterator( ).
      WHEN ms_screen-r_mediat.
        me->mediator( ).
      WHEN ms_screen-r_mement.
        me->memento( ).
      WHEN ms_screen-r_observ.
        me->observer( ).
      WHEN ms_screen-r_state.
        me->state( ).
      WHEN ms_screen-strat.
        me->strategy( ).
      WHEN ms_screen-r_tmplme.
        me->template_method( ).
      WHEN ms_screen-r_visito.
        me->visitor( ).


      WHEN OTHERS.
    ENDCASE.


  ENDMETHOD.                    "process

  "zif_lsp010_app~commit

  METHOD zif_lsp012_app~output.

  ENDMETHOD.                    "output

  METHOD zif_lsp012_app~do_check_before.

  ENDMETHOD.                    "lif_app~do_check_before

  METHOD zif_lsp012_app~commit.

  ENDMETHOD.                    "zif_lsp010_app~commit

  METHOD singleton.
    DATA lo_singleton TYPE REF TO lcl_singleton_sample.

    CREATE OBJECT lo_singleton.

    lo_singleton->main( ).


  ENDMETHOD.                    "singleton

  METHOD factory_method.
    DATA lo_factory_method TYPE REF TO lcl_factory_meth.

    CREATE OBJECT lo_factory_method.

    lo_factory_method->main( ).


  ENDMETHOD.                    "factory_method

  METHOD abstract_factory.
    DATA lo_abstract_factory TYPE REF TO lcl_abstract_factory.

    CREATE OBJECT lo_abstract_factory.

    lo_abstract_factory->main( ).

  ENDMETHOD.                    "abstract_factory

  METHOD builder.
    DATA lo_builder TYPE REF TO lcl_builder.

    CREATE OBJECT lo_builder.

    lo_builder->main( ).
  ENDMETHOD.                    "builder

  METHOD prototype.
    DATA lo_prototype TYPE REF TO lcl_prototype.

    CREATE OBJECT lo_prototype.

    lo_prototype->main( ).
  ENDMETHOD.                    "prototype


  METHOD adapter.
    DATA lo_adapter TYPE REF TO lcl_adapter.

    CREATE OBJECT lo_adapter.

    lo_adapter->main( ).

  ENDMETHOD.                    "adapter

  METHOD bridge.

    DATA lo_bridge TYPE REF TO lcl_bridge.
    CREATE OBJECT lo_bridge.
    lo_bridge->main( ).

  ENDMETHOD.                    "bridge

  METHOD composite. " компоновщик

    DATA lo_composite TYPE REF TO lcl_composite.
    CREATE OBJECT lo_composite.
    lo_composite->main( ).

  ENDMETHOD.                    "composite

  METHOD decorator. " декоратор

    DATA lo_decorator TYPE REF TO lcl_decorator.
    CREATE OBJECT lo_decorator.
    lo_decorator->main( ).

  ENDMETHOD.                    "decorator

  METHOD facade. " фасад

    DATA lo_facade TYPE REF TO lcl_facade.
    CREATE OBJECT lo_facade.
    lo_facade->main( ).

  ENDMETHOD.                    "facade

  METHOD flyweight. " приспосбленец , кэш

    DATA lo_flyweight TYPE REF TO lcl_flyweight.
    CREATE OBJECT lo_flyweight.
    lo_flyweight->main( ).

  ENDMETHOD.                    "flyweight

  METHOD proxy. " заместитель / прокси

    DATA lo_proxy TYPE REF TO lcl_proxy.
    CREATE OBJECT lo_proxy.
    lo_proxy->main( ).

  ENDMETHOD.                    "proxy


  """""""""""""""""""""""
  METHOD chain_of_responsibility.
    DATA lo_chain_of_responsibility TYPE REF TO lcl_chain_of_responsibility.
    CREATE OBJECT lo_chain_of_responsibility.
    lo_chain_of_responsibility->main( ).
  ENDMETHOD.

  METHOD command.
    DATA lo_command TYPE REF TO lcl_command.
    CREATE OBJECT lo_command.
    lo_command->main(  ).
  ENDMETHOD.

  METHOD iterator.
    DATA lo_obj TYPE REF TO lcl_iterator.
    CREATE OBJECT lo_obj.
    lo_obj->main(  ).
  ENDMETHOD.

  METHOD mediator. " посредник
    DATA lo_obj TYPE REF TO lcl_mediator.
    CREATE OBJECT lo_obj.
    lo_obj->main(  ).
  ENDMETHOD.

  METHOD memento. " снимок / хранитель
    DATA lo_obj TYPE REF TO lcl_memento.
    CREATE OBJECT lo_obj.
    lo_obj->main(  ).
  ENDMETHOD.

  METHOD observer. " наблюдатель
    DATA lo_obj TYPE REF TO lcl_observer.
    CREATE OBJECT lo_obj.
    lo_obj->main(  ).
  ENDMETHOD.

  METHOD state. " состояние
    DATA lo_obj TYPE REF TO lcl_state.
    CREATE OBJECT lo_obj.
    lo_obj->main(  ).
  ENDMETHOD.

  METHOD strategy. " стратегия
    DATA lo_obj TYPE REF TO lcl_strategy.
    CREATE OBJECT lo_obj.
    lo_obj->main(  ).
  ENDMETHOD.

  METHOD template_method. " шаблонный метод
    DATA lo_obj TYPE REF TO lcl_template_method.
    CREATE OBJECT lo_obj.
    lo_obj->main(  ).
  ENDMETHOD.

  METHOD visitor. " посетитель
    DATA lo_obj TYPE REF TO lcl_visitor.
    CREATE OBJECT lo_obj.
    lo_obj->main(  ).
  ENDMETHOD.

ENDCLASS.                    "lcl_app IMPLEMENTATION
