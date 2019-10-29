*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW04_CLS17
*&---------------------------------------------------------------------*

CLASS lcl_sales_order DEFINITION LOAD.
CLASS lcl_sales_order DEFINITION DEFERRED.

CLASS lcl_obj_memento DEFINITION LOAD.
CLASS lcl_obj_memento DEFINITION DEFERRED.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

INTERFACE lif_obj_memento.

  METHODS restore.

ENDINTERFACE.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_sales_order DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ts_sales_order_data
                , customer TYPE char1
                , order_number TYPE char10
                , amount TYPE bsprice
                , waers TYPE waers
            , END OF ts_sales_order_data

            .

    METHODS constructor.

    METHODS set_order_info
      IMPORTING is_order_info TYPE ts_sales_order_data.

    METHODS do_backup.
    METHODS restore.

    METHODS show_data.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA ms_order_info TYPE ts_sales_order_data.
    DATA mo_backup TYPE REF TO lcl_obj_memento.

ENDCLASS.

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


CLASS lcl_obj_memento DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_obj_memento.
    METHODS constructor
      IMPORTING io_originator TYPE REF TO lcl_sales_order
                is_data       TYPE lcl_sales_order=>ts_sales_order_data
      .



  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_sales_order TYPE REF TO lcl_sales_order.
    DATA ms_backup_data TYPE lcl_sales_order=>ts_sales_order_data.

ENDCLASS.

CLASS lcl_obj_memento IMPLEMENTATION.
  METHOD constructor.
    mo_sales_order ?= io_originator.
    ms_backup_data = is_data.
  ENDMETHOD.

  METHOD lif_obj_memento~restore.
    mo_sales_order->set_order_info( is_order_info = ms_backup_data ).
  ENDMETHOD.

ENDCLASS.


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


CLASS lcl_sales_order IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.

  METHOD set_order_info.
    ms_order_info = is_order_info.
  ENDMETHOD.

  METHOD do_backup.
    CREATE OBJECT mo_backup
      EXPORTING
        io_originator = me
        is_data       = ms_order_info.
  ENDMETHOD.

  METHOD restore.
    mo_backup->lif_obj_memento~restore( ).
  ENDMETHOD.

  METHOD show_data.

    DATA lt_data TYPE STANDARD TABLE OF ts_sales_order_data.

    APPEND ms_order_info TO lt_data.

    zcl_lsp012_html=>get_instance( )->add_tab_ch( it_tab = lt_data ).

  ENDMETHOD.

ENDCLASS.

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

CLASS lcl_memento DEFINITION.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS main.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_receiver TYPE REF TO zcl_lsp012_linux_os.
    DATA mo_invoker TYPE REF TO lcl_invoker.

    DATA mv_fold_tmp TYPE salfile-longname VALUE '/usr/sap/NPL/D00/work/wget_tmp/'.
    DATA mv_fold_tmp_unrar TYPE salfile-longname VALUE '/usr/sap/NPL/D00/work/wget_tmp/unrar/'.
    DATA mv_target_rar TYPE salfile-longname VALUE 'http://olegbash.ru/nftp/fias_delta.rar'.
    DATA mv_local_rar TYPE text200 VALUE '/usr/sap/NPL/D00/work/wget_tmp/fias_delta.rar'.
    DATA mv_local_fold TYPE text200 VALUE '/usr/sap/NPL/D00/work/wget_tmp/unrar'.


    METHODS run_step1.
    METHODS run_step2.
    METHODS run_step3.

    METHODS put2cluster
      IMPORTING iv_xml            TYPE string
                iv_type           TYPE char4
                iv_receiver_class TYPE seoclsname
      RETURNING VALUE(rv_val)     TYPE sysuuid_c.

    METHODS create_queue_n_start
      IMPORTING iv_guid TYPE sysuuid_c.

ENDCLASS.

CLASS lcl_memento IMPLEMENTATION.

  METHOD constructor.
    mo_receiver = NEW zcl_lsp012_linux_os( ).
  ENDMETHOD.                    "constructor

  METHOD main.
    run_step1( ).
    run_step2( ).
    run_step3( ).
  ENDMETHOD.                    "main

  METHOD run_step1.

    DATA(lo_sales_order) = NEW lcl_sales_order(  ).

    DATA(ls_order_data_init) =
        VALUE lcl_sales_order=>ts_sales_order_data(
            customer = '147852' order_number = '1050' amount = '5000'  waers = 'RUB'    ).

    lo_sales_order->set_order_info( is_order_info = ls_order_data_init ).
    lo_sales_order->show_data( ).
    lo_sales_order->do_backup( ).



    DATA(ls_order_data_after_calc) =
        VALUE lcl_sales_order=>ts_sales_order_data(
            customer = '147852' order_number = '1050' amount = '10000'  waers = 'USD'    ).

    lo_sales_order->set_order_info( is_order_info = ls_order_data_after_calc ).
    lo_sales_order->show_data( ).

    lo_sales_order->restore( ).
    lo_sales_order->show_data( ).


    zcl_lsp012_html=>get_instance(
*        iv_title =
    )->show( ).
  ENDMETHOD.                    "run_step1


  METHOD run_step2.

    DATA lv_serialized_command TYPE string.
    DATA lv_guid TYPE sysuuid_c.
    " у команды должен быть INTERFACES if_serializable_object.

    DATA(lo_command_wget) =  NEW zcl_command4memento(
      it_params   = VALUE mestringmap( ( name = 'FOLD1' value = mv_fold_tmp )
                                       ( name = 'FOLD2' value = mv_fold_tmp_unrar ) ) ).


    CALL TRANSFORMATION id_indent
    SOURCE obj = lo_command_wget
    RESULT XML lv_serialized_command.

    " сохраняем команду в кластер
    lv_guid = put2cluster( iv_xml = lv_serialized_command
                           iv_type = 'EMP1'
                           iv_receiver_class = 'ZCL_LSP012_LINUX_OS' ).


    create_queue_n_start( lv_guid ).

  ENDMETHOD.

  METHOD put2cluster.
*METHODS put2cluster
*      IMPORTING iv_xml            TYPE string
*                iv_type           TYPE char4
*                iv_receiver_class TYPE seoclsname.

    DATA ls_cluster TYPE ztlsp012_extcomm.
    DATA ls_cluster_data_inside TYPE zslsp012_comm.

    GET TIME.

    TRY.
        ls_cluster-client = cl_abap_syst=>get_client( ).
        ls_cluster-guid = cl_system_uuid=>create_uuid_c32_static( ).

        ls_cluster-crdate = sy-datum.
        ls_cluster-crtime = sy-uzeit.
        ls_cluster-cuname = sy-uname.
        ls_cluster-comm_type   = iv_type.

        ls_cluster-comm_meta1   = iv_receiver_class.
        ls_cluster-comm_meta2   = 'PATTERN_MEMENTO'.
        ls_cluster-comm_meta3   = ''.

        ls_cluster_data_inside-crd = sy-datum.
        ls_cluster_data_inside-crt = sy-uzeit.
        ls_cluster_data_inside-cru = sy-uname.
        ls_cluster_data_inside-xml_data = iv_xml.

        EXPORT comm_data = ls_cluster_data_inside
          TO DATABASE ztlsp012_extcomm(i0)
          FROM ls_cluster
          ID ls_cluster-guid.

        rv_val = ls_cluster-guid.

        COMMIT WORK AND WAIT.

      CATCH cx_root.
        CLEAR rv_val.
    ENDTRY.

  ENDMETHOD.

  METHOD create_queue_n_start.
    "        IMPORTING iv_guid TYPE sysuuid_c.

    DATA lv_qname TYPE  trfcqout-qname .

    IF iv_guid IS INITIAL. RETURN. ENDIF.

    lv_qname = |MEME_{ sy-datum+6 }{ sy-uzeit }|.

    CALL FUNCTION 'TRFC_SET_QUEUE_NAME'
      EXPORTING
        qname = lv_qname.


    CALL FUNCTION 'Z_LSP012_START_COMM'
      IN BACKGROUND TASK
      EXPORTING
        iv_guid = iv_guid.

    COMMIT WORK.

  ENDMETHOD.

  METHOD run_step3.
    " https://help.sap.com/saphelp_nwpi711/helpdata/en/48/927c2caa6b17cee10000000a421937/content.htm?no_cache=true
    " https://sapland.ru/kb/articles/spj/vizov-bgrfc-kak-sredstvo-povisheniya-effektivnosti
    "-rfc-vizovov-masshtabiruemaya-operativnaya-infrastruktura-vzaimodeistviya-prilozhenii.html
  ENDMETHOD.

ENDCLASS.
