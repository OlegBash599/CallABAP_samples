*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW04_CLS05
*&---------------------------------------------------------------------*


CLASS lcl_prototype DEFINITION.
  PUBLIC SECTION.

    METHODS constructor.
    METHODS main.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_mail_builder TYPE REF TO lcl_mail_builder.

    DATA mo_body_mail TYPE REF TO lcl_body_mail.

    METHODS run_step1.
    METHODS get_mail_to
      RETURNING VALUE(ro_obj) TYPE REF TO lcl_mail_to.

    METHODS get_subject
      RETURNING VALUE(rv_val) TYPE string.

    METHODS get_body
      RETURNING VALUE(ro_obj) TYPE REF TO lcl_body_mail.

    METHODS get_body_clone
      RETURNING VALUE(ro_obj) TYPE REF TO lcl_body_mail.

    METHODS get_list_n_vars
      IMPORTING iv_type      TYPE char1
      EXPORTING et_atta_list TYPE lcl_body_mail=>tt_add_files
                et_vars      TYPE lcl_body_mail=>tt_var_in_html.


ENDCLASS.                    "lcl_builder DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_builder IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_prototype IMPLEMENTATION.
  METHOD constructor.
    CREATE OBJECT mo_mail_builder.
  ENDMETHOD.                    "constructor

  METHOD main.

    mo_mail_builder->build_to( io_mail = get_mail_to( ) ).

    "mo_mail_builder->build_to_cc( io_mail = get_mail_to( ) ).
    "mo_mail_builder->build_to_cc_blind( io_mail = get_mail_to( ) ).

    mo_mail_builder->build_subject( iv_subj = get_subject( ) ).

    mo_mail_builder->build_body( io_body_mail = get_body( ) ).

    "build_attachments

    mo_mail_builder->send_mail( ).


    mo_mail_builder->build_body( io_body_mail = get_body_clone( ) ).
    mo_mail_builder->send_mail( ).

  ENDMETHOD.                    "main

  METHOD get_mail_to.
    DATA lo_mail_to TYPE REF TO lcl_mail_to.

    CREATE OBJECT lo_mail_to.
    lo_mail_to->add_mail( iv_mail = 'it@olegbash.ru' ).
    "lo_mail_to->add_mail( iv_mail = 'bashkatov_o@utkonos.ru' ).

    ro_obj = lo_mail_to.

  ENDMETHOD.                    "get_mail_to

  METHOD get_subject.
    rv_val = 'Mail Subject from Super Suit & Co'.
  ENDMETHOD.                    "get_subject

  METHOD get_body.

    DATA lt_atta_list TYPE lcl_body_mail=>tt_add_files.
    DATA lt_vars TYPE lcl_body_mail=>tt_var_in_html.

    me->get_list_n_vars( EXPORTING iv_type = 'R' " Cr.Ronaldo
                         IMPORTING et_atta_list = lt_atta_list
                                   et_vars = lt_vars ).

    BREAK-POINT.
    IF mo_body_mail IS NOT BOUND.
      CREATE OBJECT mo_body_mail
        EXPORTING
          iv_template = 'TEML2'.

      mo_body_mail->fill_data( ).
    ENDIF.

    mo_body_mail->set_vars_n_file_list(
      EXPORTING it_vars = lt_vars
                it_atta_list = lt_atta_list ).


    ro_obj = mo_body_mail.

  ENDMETHOD.                    "get_body

  METHOD get_body_clone.

    DATA lt_atta_list TYPE lcl_body_mail=>tt_add_files.
    DATA lt_vars TYPE lcl_body_mail=>tt_var_in_html.

    DATA lo_body_mail_cloned TYPE REF TO lcl_body_mail.

    me->get_list_n_vars( EXPORTING iv_type = 'M' " Messi
                         IMPORTING et_atta_list = lt_atta_list
                                   et_vars = lt_vars ).

    "lo_body_mail_cloned = mo_body_mail->clone( ).
    lo_body_mail_cloned = mo_body_mail->clone_sys( ).

    lo_body_mail_cloned->set_vars_n_file_list(
    EXPORTING it_vars = lt_vars
              it_atta_list = lt_atta_list ).

    ro_obj = lo_body_mail_cloned.

  ENDMETHOD.                    "get_body_clone

  METHOD run_step1.

  ENDMETHOD.                    "run_step1

  METHOD get_list_n_vars.
*        IMPORTING iv_type TYPE char1
*        EXPORTING et_atta_list TYPE lcl_body_mail=>tt_add_files
*                  et_vars TYPE lcl_body_mail=>tt_var_in_html.
    DATA ls_attach_line LIKE LINE OF et_atta_list.
    DATA ls_vars LIKE LINE OF et_vars.


    CLEAR et_atta_list.
    CLEAR et_vars.

    CASE iv_type.
      WHEN 'R'. " Ronaldo

        ls_attach_line-path2file = '/usr/sap/UDS/DVEBMGS01/work/image001.png'.
        ls_attach_line-filetype = 'PNG'.
        ls_attach_line-file_alias = 'image001.png'.
        APPEND ls_attach_line TO et_atta_list.

        ls_vars-var_n = '$1$'.
        ls_vars-var_n = 'Cristiano Ronaldo'.
        APPEND ls_vars TO et_vars.

        ls_vars-var_n = '$2$'.
        ls_vars-var_n = '5000'.
        APPEND ls_vars TO et_vars.

      WHEN 'M'. " Leo Messi

        ls_attach_line-path2file = '/usr/sap/UDS/DVEBMGS01/work/image001_M.png'.
        ls_attach_line-filetype = 'PNG'.
        ls_attach_line-file_alias = 'image001.png'.
        APPEND ls_attach_line TO et_atta_list.

        ls_vars-var_n = '$1$'.
        ls_vars-var_n = 'Leo Messi'.
        APPEND ls_vars TO et_vars.

        ls_vars-var_n = '$2$'.
        ls_vars-var_n = '3000'.
        APPEND ls_vars TO et_vars.

      WHEN OTHERS.
    ENDCASE.




  ENDMETHOD .                    "get_list_n_vars

ENDCLASS.
