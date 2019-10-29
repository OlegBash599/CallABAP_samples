*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW04_CLS04
*&---------------------------------------------------------------------*


*----------------------------------------------------------------------*
*       CLASS lcl_mail_to DEFINITION
*----------------------------------------------------------------------*
" класс mail отвечает за подбор получателей
" мы можем их получать по списку
" из пользователя
" из таб номера / инфотипа
" проверять на вхождения в белый/черный список
" поэтому выделяет адресата - в отдельный класс
*----------------------------------------------------------------------*
CLASS lcl_mail_to DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ts_mail
            , mail TYPE ad_smtpadr
          , END OF ts_mail
          , tt_mail TYPE STANDARD TABLE OF ts_mail
            WITH DEFAULT KEY
          , tt_srt_mail TYPE SORTED TABLE OF ts_mail
            WITH UNIQUE KEY primary_key COMPONENTS mail

          .

    METHODS constructor.
    METHODS add_mail
      IMPORTING iv_mail TYPE ad_smtpadr.

    METHODS get_mail_list
      RETURNING VALUE(rt_val) TYPE tt_srt_mail.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_mails TYPE tt_srt_mail.

ENDCLASS.                    "lcl_mail_to DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_mail_to IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_mail_to IMPLEMENTATION.
  METHOD constructor.
    CLEAR mt_mails.
  ENDMETHOD.                    "constructor

  METHOD add_mail.
    DATA ls_mail TYPE ts_mail.

    IF iv_mail IS INITIAL.
      RETURN.
    ENDIF.

    CLEAR ls_mail.
    ls_mail-mail = iv_mail.
    INSERT ls_mail INTO TABLE mt_mails.

  ENDMETHOD.                    "add_mail

  METHOD get_mail_list.
    rt_val = mt_mails.
  ENDMETHOD.                    "get_mail_list

ENDCLASS.                    "lcl_mail_to IMPLEMENTATION

""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
*----------------------------------------------------------------------*
*       CLASS lcl_body_mail DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_body_mail DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ts_attach
            , atta_type TYPE soodk-objtp
            , atta_subj TYPE sood-objdes
            , atta_size TYPE sood-objlen
            , atta_bin_cont TYPE solix_tab
          , END OF ts_attach
          , tt_attach TYPE STANDARD TABLE OF ts_attach
            WITH DEFAULT KEY
          .

    TYPES: BEGIN OF ts_line
          , line TYPE string
        , END OF ts_line
        , tt_line TYPE STANDARD TABLE OF ts_line
          WITH DEFAULT KEY
        .

    TYPES: BEGIN OF ts_add_files
          , path2file TYPE string
          , filetype TYPE string
          , file_alias TYPE string
        , END OF ts_add_files
        , tt_add_files TYPE STANDARD TABLE OF ts_add_files
          WITH DEFAULT KEY
        .


    TYPES: BEGIN OF ts_var_in_html
            , var_n TYPE string
            , var_val TYPE string
          , END OF ts_var_in_html
          , tt_var_in_html TYPE STANDARD TABLE OF ts_var_in_html
            WITH DEFAULT KEY
          .

    DATA mt_var_in_html TYPE tt_var_in_html.


    METHODS constructor
      IMPORTING iv_template TYPE string OPTIONAL.

    METHODS fill_data.

    METHODS get_attach2mail
      RETURNING VALUE(rt_val) TYPE tt_attach.

    METHODS get_html_lines
      RETURNING VALUE(rt_val) TYPE tt_line.


    METHODS set_vars_n_file_list
      IMPORTING it_vars      TYPE tt_var_in_html
                it_atta_list TYPE tt_add_files.

    METHODS clone
      RETURNING VALUE(ro_obj) TYPE REF TO lcl_body_mail.

    METHODS clone_sys
      RETURNING VALUE(ro_obj) TYPE REF TO lcl_body_mail.

  PROTECTED SECTION.
  PRIVATE SECTION.



    DATA mv_path2html_templ TYPE string.

    DATA mv_template TYPE string.
    DATA mt_line TYPE tt_line.

    DATA mt_attach TYPE tt_attach.
    DATA mt_atta_list TYPE tt_add_files.





    METHODS fill_path2template.
    METHODS fill_attach_info
      IMPORTING is_atta_list TYPE ts_add_files
      CHANGING  cs_attach    TYPE ts_attach
      .

ENDCLASS.                    "lcl_body_mail DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_body_mail IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_body_mail IMPLEMENTATION.
  METHOD constructor.
    mv_template = iv_template.
    me->fill_path2template( ).
  ENDMETHOD.                    "constructor

  METHOD fill_path2template.
    DATA ls_attach_line TYPE ts_add_files.

    CASE mv_template.
      WHEN 'TEML1'.

        mv_path2html_templ = '/usr/sap/NPL/D00/work/TMP.html'.

        CLEAR mt_atta_list.

        CLEAR ls_attach_line.
      "  ls_attach_line-path2file = '/usr/sap/UDS/DVEBMGS01/work/image001.png'.
        ls_attach_line-filetype = 'PNG'.
        ls_attach_line-file_alias = 'image001.png'.
        APPEND ls_attach_line TO mt_atta_list.

      WHEN 'TEML2'.
        "wait UP TO 40 SECONDS.
        mv_path2html_templ = '/usr/sap/NPL/D00/work/TMP2.html'.
      WHEN OTHERS.
    ENDCASE.


  ENDMETHOD.                    "fill_path2template

  METHOD fill_data.
    DATA ls_line TYPE ts_line.
    DATA lv_str TYPE string.
    CLEAR mt_line.


    OPEN DATASET mv_path2html_templ FOR INPUT IN TEXT MODE ENCODING UTF-8 .

    DO.
      CLEAR ls_line.
      READ DATASET mv_path2html_templ INTO lv_str.
      IF sy-subrc EQ 0.
        ls_line-line = lv_str.
        APPEND ls_line TO mt_line.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    CLOSE DATASET mv_path2html_templ.

  ENDMETHOD.                    "fill_data

  METHOD get_attach2mail.
    "RETURNING value(rt_val) TYPE tt_attach.
    FIELD-SYMBOLS <fs_attach> TYPE ts_attach.
    FIELD-SYMBOLS <fs_atta_list> TYPE ts_add_files.
    CLEAR me->mt_attach.

    LOOP AT mt_atta_list ASSIGNING <fs_atta_list>.
      APPEND INITIAL LINE TO me->mt_attach ASSIGNING <fs_attach>.
      fill_attach_info( EXPORTING is_atta_list = <fs_atta_list>
                        CHANGING cs_attach = <fs_attach> ).
    ENDLOOP.

    rt_val = me->mt_attach.
  ENDMETHOD.                    "get_attach2mail

  METHOD get_html_lines.
    rt_val[] =  me->mt_line[].
  ENDMETHOD.                    "get_html_lines


  METHOD fill_attach_info.
*        IMPORTING is_atta_list TYPE ts_add_files
*        CHANGING cs_attach TYPE ts_attach
*        .
    DATA lv_xstring TYPE xstring.
    DATA lv_xstr_tot TYPE xstring.

    cs_attach-atta_type = is_atta_list-filetype.
    cs_attach-atta_subj = is_atta_list-file_alias.

    " https://archive.sap.com/discussions/thread/1963206
    " * Since the file is opened in Binary mode, the entire contents of the
* file is read in one go!

    OPEN DATASET is_atta_list-path2file FOR INPUT IN BINARY MODE.

    DO.
      READ DATASET is_atta_list-path2file INTO lv_xstring.
      IF sy-subrc EQ 0.
        EXIT.
      ELSE.
        CLEAR lv_xstring.
        EXIT.
      ENDIF.
    ENDDO.

    CLOSE DATASET is_atta_list-path2file.


    cs_attach-atta_size = xstrlen( lv_xstring ).
    cs_attach-atta_bin_cont = cl_bcs_convert=>xstring_to_solix( iv_xstring = lv_xstring ).

  ENDMETHOD.                    "fill_attach_info

  METHOD set_vars_n_file_list.
*      IMPORTING it_vars TYPE tt_var_in_html
*                it_atta_list TYPE tt_add_files.

    mt_atta_list[] = it_atta_list[].
    mt_var_in_html[] = it_vars[].

  ENDMETHOD.                    "set_vars_n_file_list

  METHOD clone.
    "RETURNING VALUE(ro_obj) TYPE REF TO lcl_body_mail.
  ENDMETHOD.                    "clone

  METHOD clone_sys.
    "      RETURNING VALUE(ro_obj) TYPE REF TO lcl_body_mail.
    SYSTEM-CALL OBJMGR CLONE me TO ro_obj.
  ENDMETHOD.                    "clone_sys

ENDCLASS.                    "lcl_body_mail IMPLEMENTATION

""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================

*----------------------------------------------------------------------*
*       CLASS lcl_abs_mail_builder DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_abs_mail_builder DEFINITION ABSTRACT.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS build_to
      IMPORTING io_mail TYPE REF TO lcl_mail_to.
    METHODS build_to_cc
      IMPORTING io_mail TYPE REF TO lcl_mail_to.
    METHODS build_to_cc_blind
      IMPORTING io_mail TYPE REF TO lcl_mail_to.
    METHODS build_subject
      IMPORTING iv_subj TYPE string.
    METHODS build_body
      IMPORTING io_body_mail TYPE REF TO lcl_body_mail.
    METHODS build_attachments.

    METHODS send_mail.



  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_mail_to TYPE REF TO lcl_mail_to.
    DATA mv_subject TYPE string.
    DATA mo_body_mail TYPE REF TO lcl_body_mail.

    DATA mt_mails TYPE lcl_mail_to=>tt_srt_mail.
    DATA mt_attachments TYPE lcl_body_mail=>tt_attach.

    METHODS get_body_solix
      RETURNING VALUE(rt_val) TYPE soli_tab.

    METHODS get_subject
      RETURNING VALUE(rv_val) TYPE so_obj_des.


    METHODS prefill_data4mail.
    METHODS prefill_mails_to.
    METHODS prefill_attach_data.

ENDCLASS.                    "lcl_abs_mail_builder DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_abs_mail_builder IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_abs_mail_builder IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.                    "constructor

  METHOD build_to.
    mo_mail_to = io_mail.
  ENDMETHOD.                    "build_to

  METHOD build_to_cc.
    " let be empty
  ENDMETHOD.                    "build_to_cc

  METHOD build_to_cc_blind.
    " let be empty
  ENDMETHOD.                    "build_to_cc_blind

  METHOD build_subject.
    mv_subject = iv_subj.
  ENDMETHOD.                    "build_subject

  METHOD build_body.
    mo_body_mail = io_body_mail.
  ENDMETHOD.                    "build_body

  METHOD build_attachments.

  ENDMETHOD.                    "build_attachments


  METHOD send_mail.
    " as main example - BCS_EXAMPLE_7

    DATA lo_send_request   TYPE REF TO cl_bcs.
    DATA lo_document       TYPE REF TO cl_document_bcs.
    DATA lo_recipient      TYPE REF TO if_recipient_bcs.
    DATA lx_bcs_exception  TYPE REF TO cx_bcs.

    DATA lv_sent_to_all TYPE os_boolean.

    FIELD-SYMBOLS <fs_mail> TYPE lcl_mail_to=>ts_mail.
    FIELD-SYMBOLS <fs_atta> TYPE lcl_body_mail=>ts_attach.

    me->prefill_data4mail( ).

    TRY .
*     -------- create persistent send request ------------------------
        lo_send_request = cl_bcs=>create_persistent( ).

*     -------- create and set document with attachment ---------------
*     create document object from internal table with text
        lo_document = cl_document_bcs=>create_document(
          i_type    = 'HTM'
          i_text    = get_body_solix( )
          i_subject = get_subject( ) ).                     "#EC NOTEXT


**     add the spread sheet as attachment to document object
        LOOP AT me->mt_attachments ASSIGNING <fs_atta>.
          lo_document->add_attachment(
            i_attachment_type    = <fs_atta>-atta_type
            i_attachment_subject = <fs_atta>-atta_subj
            i_attachment_size    = <fs_atta>-atta_size
            i_att_content_hex    = <fs_atta>-atta_bin_cont ).
        ENDLOOP.



*     add document object to send request
        lo_send_request->set_document( lo_document ).

*     --------- add recipient (e-mail address) -----------------------
        LOOP AT me->mt_mails ASSIGNING <fs_mail> .
*     create recipient object
          lo_recipient = cl_cam_address_bcs=>create_internet_address( <fs_mail>-mail ).

*     add recipient object to send request
          lo_send_request->add_recipient( lo_recipient ).
        ENDLOOP.
        IF sy-subrc NE 0.
          RETURN.
        ENDIF.


*     ---------- send document ---------------------------------------
        lv_sent_to_all = lo_send_request->send( i_with_error_screen = 'X' ).

        COMMIT WORK.

        IF lv_sent_to_all IS INITIAL.
          MESSAGE i500(sbcoms).
        ELSE.
          MESSAGE s022(so).
        ENDIF.

      CATCH cx_bcs INTO lx_bcs_exception.
        MESSAGE i865(so) WITH lx_bcs_exception->error_type.

    ENDTRY.

  ENDMETHOD.                    "send_mail

  METHOD prefill_data4mail.
    prefill_mails_to( ).

    prefill_attach_data( ).

  ENDMETHOD.                    "prefill_data4mail

  METHOD prefill_mails_to.
    CLEAR me->mt_mails.

    me->mt_mails = me->mo_mail_to->get_mail_list( ).

  ENDMETHOD.                    "prefill_mails_to

  METHOD prefill_attach_data.
    CLEAR mt_attachments.

    mt_attachments = mo_body_mail->get_attach2mail( ).

  ENDMETHOD.                    "prefill_attach_data

  METHOD get_body_solix.

    DATA lt_html_lines TYPE lcl_body_mail=>tt_line.
    DATA lt_soli_tab TYPE soli_tab.

    DATA lv_string_total TYPE string.
    DATA mt_var_in_html TYPE lcl_body_mail=>tt_var_in_html.

    FIELD-SYMBOLS <fs_html_lines> TYPE lcl_body_mail=>ts_line.
    FIELD-SYMBOLS <fs_var_in> TYPE lcl_body_mail=>ts_var_in_html.



    lt_html_lines = mo_body_mail->get_html_lines( ).

    CLEAR lv_string_total.
    LOOP AT lt_html_lines ASSIGNING <fs_html_lines>.
      lv_string_total = lv_string_total && <fs_html_lines>-line.
    ENDLOOP.

    LOOP AT me->mo_body_mail->mt_var_in_html ASSIGNING <fs_var_in>.
      REPLACE ALL OCCURRENCES OF <fs_var_in>-var_n IN lv_string_total WITH <fs_var_in>-var_val.
    ENDLOOP.

    rt_val = cl_bcs_convert=>string_to_soli( iv_string = lv_string_total ).


  ENDMETHOD.                    "get_body_solix

  METHOD get_subject.
    rv_val = mv_subject.
  ENDMETHOD.                    "get_subject

ENDCLASS.                    "lcl_abs_mail_builder IMPLEMENTATION

""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================

*----------------------------------------------------------------------*
*       CLASS lcl_mail_builder DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_mail_builder DEFINITION INHERITING FROM lcl_abs_mail_builder.
  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.                    "lcl_mail_builder DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_mail_builder IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_mail_builder IMPLEMENTATION.
  METHOD constructor.
    "return.
    super->constructor( ).
  ENDMETHOD.                    "constructor
ENDCLASS.                    "lcl_mail_builder IMPLEMENTATION



""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================
""=============================================================================


*----------------------------------------------------------------------*
*       CLASS lcl_builder DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_builder DEFINITION.
  PUBLIC SECTION.

    METHODS constructor.
    METHODS main.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_mail_builder TYPE REF TO lcl_mail_builder.

    METHODS run_step1.
    METHODS get_mail_to
      RETURNING VALUE(ro_obj) TYPE REF TO lcl_mail_to.

    METHODS get_subject
      RETURNING VALUE(rv_val) TYPE string.

    METHODS get_body
      RETURNING VALUE(ro_obj) TYPE REF TO lcl_body_mail.

ENDCLASS.                    "lcl_builder DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_builder IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_builder IMPLEMENTATION.
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
    DATA lo_body_mail TYPE REF TO lcl_body_mail.

    BREAK-POINT.

    CREATE OBJECT lo_body_mail
      EXPORTING
        iv_template = 'TEML1'.

    lo_body_mail->fill_data( ).

    ro_obj = lo_body_mail.

  ENDMETHOD.                    "get_body

  METHOD run_step1.

  ENDMETHOD.                    "run_step1

ENDCLASS.
