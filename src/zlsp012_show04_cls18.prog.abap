*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW04_CLS18
*&---------------------------------------------------------------------*

INTERFACE lif_publisher  LOAD.
INTERFACE lif_publisher  DEFERRED.


INTERFACE lif_subscriber.
  METHODS set_publisher
    IMPORTING io_publisher TYPE REF TO lif_publisher.
  METHODS notify.
ENDINTERFACE.


INTERFACE lif_publisher.
  METHODS subscribe
    IMPORTING io_subscriber TYPE REF TO lif_subscriber.
  METHODS unsubscribe
    IMPORTING io_subscriber TYPE REF TO lif_subscriber.
  METHODS notify_subscribers.
  METHODS get_rates
    RETURNING VALUE(rt_val) TYPE ztlsp012_rates_tab.

ENDINTERFACE.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_publisher DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_publisher.
    METHODS constructor.

    METHODS check_new_exchange_rates.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mv_check_try TYPE syindex.
    DATA mt_rates TYPE STANDARD TABLE OF ztlsp012_rates.
    DATA mo_subscribers TYPE REF TO zcl_lsp012_object_collection.



ENDCLASS.

CLASS lcl_publisher IMPLEMENTATION.
  METHOD constructor.
    CREATE OBJECT mo_subscribers.
    mv_check_try = 0.
  ENDMETHOD.

  METHOD lif_publisher~subscribe.
    mo_subscribers->add( element = io_subscriber ).
  ENDMETHOD.

  METHOD lif_publisher~unsubscribe.
    mo_subscribers->remove( element = io_subscriber ).
  ENDMETHOD.

  METHOD lif_publisher~notify_subscribers.
    DATA(lo_iter) = mo_subscribers->get_iterator( ).
    DATA lo_subsscriber TYPE REF TO lif_subscriber.
    WHILE lo_iter->has_next( ).
      lo_subsscriber ?= lo_iter->get_next( ).
      lo_subsscriber->notify( ).
    ENDWHILE.

  ENDMETHOD.

  METHOD lif_publisher~get_rates.
    rt_val = mt_rates.
  ENDMETHOD.

  METHOD check_new_exchange_rates.
    DATA lv_do TYPE char1.
    mv_check_try = 1 + mv_check_try.

    zcl_lsp012_html=>get_instance( )->add_para_val( iv_id = 'PUBLISHER' iv_value = 'Check for any updates' ).

    CALL FUNCTION 'Z_LSP012_GET_RATES'
      DESTINATION 'NONE'
      EXPORTING
        iv_trynum  = mv_check_try                 " Loop Index
      IMPORTING
        et_rates   = mt_rates                 " ZTLSP012_RATES -> tab
        ev_changed = lv_do.                 " Single-Character Flag

    IF lv_do EQ abap_true.
      lif_publisher~notify_subscribers( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_publisher_event DEFINITION.
  PUBLIC SECTION.
    "https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abapset_handler_instance.htm
    " Addition 2 ... FOR ALL INSTANCES
    " Addition 3 ... ACTIVATION act

    INTERFACES lif_publisher.
    METHODS constructor.

    METHODS check_new_exchange_rates.

    METHODS check_async_answer
      IMPORTING p_task TYPE clike.

    EVENTS rates_changed
      EXPORTING VALUE(eval) TYPE string DEFAULT '0'.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mv_steps_receive TYPE syindex.
    DATA mv_lines_total TYPE syindex.
    DATA mv_check_try TYPE syindex.
    DATA mt_rates TYPE STANDARD TABLE OF ztlsp012_rates.
    DATA mo_subscribers TYPE REF TO zcl_lsp012_object_collection.



ENDCLASS.

CLASS lcl_publisher_event IMPLEMENTATION.
  METHOD constructor.
    "  CREATE OBJECT mo_subscribers.
    mv_check_try = 0.
    zcl_lsp012_html=>get_instance( )->add_para_val( iv_id = 'PUBLISH' iv_value = 'Event Beg' ).
  ENDMETHOD.

  METHOD lif_publisher~subscribe.
    "  mo_subscribers->add( element = io_subscriber ).
  ENDMETHOD.

  METHOD lif_publisher~unsubscribe.
    "  mo_subscribers->remove( element = io_subscriber ).
  ENDMETHOD.

  METHOD lif_publisher~notify_subscribers.
*    DATA(lo_iter) = mo_subscribers->get_iterator( ).
*    DATA lo_subsscriber TYPE REF TO lif_subscriber.
*    WHILE lo_iter->has_next( ).
*      lo_subsscriber ?= lo_iter->get_next( ).
*      lo_subsscriber->notify( ).
*    ENDWHILE.

  ENDMETHOD.

  METHOD lif_publisher~get_rates.
    rt_val = mt_rates.
  ENDMETHOD.

  METHOD check_new_exchange_rates.

    DATA lv_taskname4par TYPE c LENGTH 8.
    DATA lv_msg TYPE text200.
    DATA lv_do TYPE char1.

*    DO 1 TIMES.
*      lv_taskname4par = 'ZLSP012' && sy-index.
*      mv_check_try = 1 + mv_check_try.
*      zcl_lsp012_html=>get_instance( )->add_para_val( iv_id = 'PUBLISHER' iv_value = |Check for any updates { sy-index } | ).
*
*      CALL FUNCTION 'Z_LSP012_GET_RATES'
*        STARTING NEW TASK lv_taskname4par
*       DESTINATION IN GROUP DEFAULT
* "       DESTINATION 'NONE'
*        CALLING check_async_answer ON END OF TASK
*        EXPORTING
*          iv_trynum             = mv_check_try
*        EXCEPTIONS
*          system_failure        = 1 MESSAGE lv_msg
*          communication_failure = 2 MESSAGE lv_msg
*          resource_failure      = 3.
*      CASE sy-subrc.
*        WHEN 0.
*
*        WHEN OTHERS.
*          zcl_lsp012_html=>get_instance( )->add_para_val( iv_id = 'GET_RATES' iv_value = |Exception| ).
*      ENDCASE.
*      IF lv_do EQ abap_true.
**        "      lif_publisher~notify_subscribers( ).
**      ENDIF.
*
*
*    ENDDO.
*
*    WAIT UNTIL me->mv_steps_receive GE me->mv_lines_total
*    UP TO 3600 SECONDS.

    DO 10 TIMES.
      CLEAR lv_do.
      mv_check_try = 1 + mv_check_try.
      CALL FUNCTION 'Z_LSP012_GET_RATES'
        EXPORTING
          iv_trynum  = mv_check_try                 " Loop Index
        IMPORTING
          et_rates   = mt_rates                 " ZTLSP012_RATES -> tab
          ev_changed = lv_do.                 " Single-Character Flag

      IF lv_do EQ abap_true.
        zcl_lsp012_html=>get_instance( )->add_para_val( iv_id = 'PUBLISHER' iv_value = |Event Raise| ).

        RAISE EVENT rates_changed   EXPORTING   eval = CONV string( mv_check_try ) .
      ENDIF.

    ENDDO.



  ENDMETHOD.

  METHOD check_async_answer.
    DATA lv_do TYPE char1.
    me->mv_steps_receive =
    me->mv_steps_receive + 1.
    RECEIVE RESULTS FROM FUNCTION 'Z_LSP012_GET_RATES'
        KEEPING TASK
        IMPORTING
      et_rates   = mt_rates                 " ZTLSP012_RATES -> tab
      ev_changed = lv_do.                 " Single-Character Flag

    IF lv_do EQ abap_true.
      zcl_lsp012_html=>get_instance( )->add_para_val( iv_id = 'PUBLISHER' iv_value = |Event Raise| ).
      RAISE EVENT rates_changed   EXPORTING   eval = CONV string( mv_check_try ) .
    ENDIF.
  ENDMETHOD.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_listener2file DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_subscriber.
    METHODS constructor.

    METHODS handle_get_rates
        FOR EVENT rates_changed OF lcl_publisher_event.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_publisher TYPE REF TO lif_publisher.
    METHODS save2file.
ENDCLASS.


CLASS lcl_listener2file IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.

  METHOD lif_subscriber~notify.
    save2file(  ).
  ENDMETHOD.

  METHOD save2file.
    DATA(lt_rates) = mo_publisher->get_rates( ).
    DATA lv_file_name TYPE string VALUE 'rates'.
    DATA lv_file_content TYPE string.
    lv_file_name = 'Rates_' && sy-datum && sy-uzeit && '.txt'.

    LOOP AT lt_rates ASSIGNING FIELD-SYMBOL(<fs>).
      lv_file_content = lv_file_name && <fs>-partner && <fs>-partner_type
                      && <fs>-rating_deliver_euro && <fs>-rating_deliver_ru.
    ENDLOOP.


    OPEN DATASET lv_file_name FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
    IF sy-subrc EQ 0.
      TRANSFER lv_file_content TO lv_file_name.
      CLOSE DATASET lv_file_name.
    ENDIF.

    zcl_lsp012_html=>get_instance( )->add_para_val( iv_id = 'FILE' iv_value = CONV text255( lv_file_name ) ).

  ENDMETHOD.

  METHOD lif_subscriber~set_publisher.
    mo_publisher ?= io_publisher.
  ENDMETHOD.

  METHOD handle_get_rates.
    "        FOR EVENT rates_changed OF lcl_publisher_event.
    save2file(  ).
  ENDMETHOD.
ENDCLASS.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_listener2mail DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_subscriber.
    METHODS constructor.

    METHODS handle_get_rates
        FOR EVENT rates_changed OF lcl_publisher_event.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_publisher TYPE REF TO lif_publisher.
    METHODS do_mail.
ENDCLASS.


CLASS lcl_listener2mail IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.

  METHOD lif_subscriber~notify.
    do_mail(  ).
  ENDMETHOD.

  METHOD do_mail.
    DATA(lt_rates) = mo_publisher->get_rates( ).

    CLASS ca_sapuser_bcs     DEFINITION LOAD.
    CLASS cl_cam_address_bcs DEFINITION LOAD.

    DATA: send_request       TYPE REF TO cl_bcs.
    DATA: text               TYPE bcsy_text.
    DATA: att_text           TYPE bcsy_text.
    DATA: document           TYPE REF TO cl_document_bcs.
    DATA: sender             TYPE REF TO cl_sapuser_bcs.
    DATA: recipient          TYPE REF TO if_recipient_bcs.
    DATA: bcs_exception      TYPE REF TO cx_bcs.
    DATA: sent_to_all        TYPE os_boolean.

    zcl_lsp012_html=>get_instance( )->add_para_val( iv_id = 'MAIL' iv_value = 'START' ).

    LOOP AT lt_rates ASSIGNING FIELD-SYMBOL(<fs>).
      APPEND INITIAL LINE TO text ASSIGNING FIELD-SYMBOL(<fs_text>).
      <fs_text>-line = <fs>-partner && <fs>-partner_type
                    && <fs>-rating_deliver_euro && <fs>-rating_deliver_ru.
    ENDLOOP.

    TRY.
*     -------- create persistent send request ------------------------
        send_request = cl_bcs=>create_persistent( ).

*     -------- create and set document with attachment ---------------
*     create document from internal table with text
        APPEND 'New Rates' TO text.
        document = cl_document_bcs=>create_document(
                        i_type    = 'RAW'
                        i_text    = text
                        i_subject = 'New exchange rates' ).


        CALL METHOD document->add_attachment
          EXPORTING
            i_attachment_type    = 'RAW'
            i_attachment_subject = 'ExchangeRateData'
            i_att_content_text   = text.

*     add document to send request
        CALL METHOD send_request->set_document( document ).

*     --------- add recipient (fax) -----------------------
**     create recipient - please replace fax number !!!
*        recipient = cl_cam_address_bcs=>create_fax_address(
*          i_country = 'DE'
*          i_number  = '09999-123456' ).

        recipient = cl_cam_address_bcs=>create_internet_address(
                      i_address_string = 'test@olegbash.ru'
*                      i_address_name   =
*                      i_incl_sapuser   =
                    ).
*                    CATCH cx_address_bcs. " BCS: Address Exceptions

*     add recipient with its respective attributes to send request
        CALL METHOD send_request->add_recipient
          EXPORTING
            i_recipient = recipient
            i_express   = 'X'.

*     ---------- send document ---------------------------------------
        send_request->send_request->set_link_to_outbox( 'X' ).

        CALL METHOD send_request->send(
          EXPORTING
            i_with_error_screen = 'X'
          RECEIVING
            result              = sent_to_all ).
        IF sent_to_all = 'X'.
          zcl_lsp012_html=>get_instance( )->add_para_val( iv_id = 'MAIL' iv_value = TEXT-003 ).
        ENDIF.

        COMMIT WORK.


* -----------------------------------------------------------
* *                     exception handling
* -----------------------------------------------------------
* * replace this very rudimentary exception handling
* * with your own one !!!
* -----------------------------------------------------------
      CATCH cx_bcs INTO bcs_exception.
        zcl_lsp012_html=>get_instance( )->add_para_val( iv_id = 'MAIL' iv_value = 'Errors' ).
        EXIT.

    ENDTRY.



  ENDMETHOD.

  METHOD lif_subscriber~set_publisher.
    mo_publisher ?= io_publisher.
  ENDMETHOD.

  METHOD handle_get_rates.
    "        FOR EVENT rates_changed OF lcl_publisher_event.
    do_mail( ).
  ENDMETHOD.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

CLASS lcl_listener2db DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_subscriber.
    METHODS constructor.

    METHODS handle_get_rates
        FOR EVENT rates_changed OF lcl_publisher_event.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_publisher TYPE REF TO lif_publisher.
    METHODS save2db.
ENDCLASS.


CLASS lcl_listener2db IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.

  METHOD lif_subscriber~notify.
    save2db(  ).
  ENDMETHOD.

  METHOD save2db.

    DATA(lo_tab_upd) = NEW zcl_lsp012_anytab_upd(  ).

    IF lo_tab_upd->zif_lsp012_anytab_upd~put2upd_task(
      EXPORTING
        iv_tabname   = 'ZTLSP012_RATES2'                 " Table Name
        it_data      = mo_publisher->get_rates( )
        iv_do_commit = abap_true       " Single-Character Flag
    ) EQ 0.

      zcl_lsp012_html=>get_instance(
*          iv_title =
      )->add_para_val(
        EXPORTING
          iv_id    = 'SAVE2DB'
          iv_value = 'OK'
      ).

    ELSE.
      zcl_lsp012_html=>get_instance(
*          iv_title =
       )->add_para_val(
         EXPORTING
           iv_id    = 'SAVE2DB'
           iv_value = 'NO'
       ).
    ENDIF.


  ENDMETHOD.

  METHOD lif_subscriber~set_publisher.
    mo_publisher ?= io_publisher.
  ENDMETHOD.

  METHOD handle_get_rates.
    "        FOR EVENT rates_changed OF lcl_publisher_event.
    save2db( ).
  ENDMETHOD.
ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

CLASS lcl_observer DEFINITION.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS main.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_publisher TYPE REF TO lcl_publisher.
    DATA mo_publisher_event TYPE REF TO lcl_publisher_event.

    METHODS run_step1.
    METHODS run_step2.
    METHODS run_step3.


ENDCLASS.

CLASS lcl_observer IMPLEMENTATION.

  METHOD constructor.
    mo_publisher = NEW #(  ).
    mo_publisher_event = NEW #(  ).
  ENDMETHOD.                    "constructor

  METHOD main.
    run_step1( ).
    run_step2( ).
    run_step3( ).
  ENDMETHOD.                    "main

  METHOD run_step1.
    DATA(lo_sub2db) = NEW lcl_listener2db(  ).
    DATA(lo_sub2mail) = NEW lcl_listener2mail(  ).
    DATA(lo_sub2txt) = NEW lcl_listener2file(  ).

    lo_sub2db->lif_subscriber~set_publisher( io_publisher = mo_publisher ).
    lo_sub2mail->lif_subscriber~set_publisher( io_publisher = mo_publisher ).
    lo_sub2txt->lif_subscriber~set_publisher( io_publisher = mo_publisher ).

    mo_publisher->lif_publisher~subscribe( io_subscriber = lo_sub2db ).
    mo_publisher->lif_publisher~subscribe( io_subscriber = lo_sub2mail ).
    mo_publisher->lif_publisher~subscribe( io_subscriber = lo_sub2txt ).


    mo_publisher->check_new_exchange_rates( ).
    mo_publisher->check_new_exchange_rates( ).
    mo_publisher->check_new_exchange_rates( ).
    mo_publisher->check_new_exchange_rates( ).
    mo_publisher->check_new_exchange_rates( ).
    mo_publisher->check_new_exchange_rates( ).
    mo_publisher->lif_publisher~unsubscribe( io_subscriber = lo_sub2mail ).
    mo_publisher->check_new_exchange_rates( ).
    mo_publisher->check_new_exchange_rates( ).
    mo_publisher->check_new_exchange_rates( ).
    mo_publisher->lif_publisher~unsubscribe( io_subscriber = lo_sub2txt ).

    zcl_lsp012_html=>get_instance( )->show( ).

  ENDMETHOD.                    "run_step1


  METHOD run_step2.
    DATA(lo_sub2db) = NEW lcl_listener2db(  ).
    DATA(lo_sub2mail) = NEW lcl_listener2mail(  ).
    DATA(lo_sub2txt) = NEW lcl_listener2file(  ).

    lo_sub2db->lif_subscriber~set_publisher( io_publisher = mo_publisher_event ).
    lo_sub2mail->lif_subscriber~set_publisher( io_publisher = mo_publisher_event ).
    lo_sub2txt->lif_subscriber~set_publisher( io_publisher = mo_publisher_event ).

    SET HANDLER lo_sub2db->handle_get_rates FOR ALL INSTANCES ACTIVATION abap_true.
    SET HANDLER lo_sub2mail->handle_get_rates FOR ALL INSTANCES ACTIVATION abap_true.
    SET HANDLER lo_sub2txt->handle_get_rates FOR ALL INSTANCES ACTIVATION abap_true.

    mo_publisher_event->check_new_exchange_rates( ).
    SET HANDLER lo_sub2mail->handle_get_rates FOR ALL INSTANCES ACTIVATION abap_false.
    SET HANDLER lo_sub2txt->handle_get_rates FOR ALL INSTANCES ACTIVATION abap_false.
    mo_publisher_event->check_new_exchange_rates( ).

    zcl_lsp012_html=>get_instance( )->show( ).

  ENDMETHOD.

  METHOD run_step3.

  ENDMETHOD.

ENDCLASS.
