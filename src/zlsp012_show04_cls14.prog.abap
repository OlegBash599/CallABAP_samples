*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW04_CLS14
*&---------------------------------------------------------------------*

INTERFACE lif_command_obj.

  METHODS execute
    RETURNING VALUE(rv_val) TYPE boolean.

ENDINTERFACE.



CLASS lcl_invoker DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.

    METHODS add_command
      IMPORTING io_command TYPE REF TO lif_command_obj.

    METHODS execute_all.



  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES: BEGIN OF ts_command_line
                , command TYPE REF TO lif_command_obj
                , posnr TYPE syindex
                , done TYPE syindex
            , END OF ts_command_line
            , tt_command_line TYPE STANDARD TABLE OF ts_command_line
                    WITH DEFAULT KEY
            .

    DATA mt_command_line TYPE tt_command_line.

    METHODS unexecute_all.
    METHODS put2history
      IMPORTING ir_command_line TYPE REF TO ts_command_line.

ENDCLASS.

CLASS lcl_invoker IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.

  METHOD add_command.
    APPEND VALUE #( command = io_command posnr = CONV sytabix( lines( mt_command_line ) + 1 ) ) TO mt_command_line.
  ENDMETHOD.

  METHOD execute_all.
    DATA lr_command_line TYPE REF TO ts_command_line.

    LOOP AT mt_command_line REFERENCE INTO lr_command_line.
      IF lr_command_line->command->execute( ) EQ abap_true.
        put2history( lr_command_line ).
      ELSE.
        unexecute_all(  ).
      ENDIF.
    ENDLOOP.

    CLEAR mt_command_line.

  ENDMETHOD.

  METHOD unexecute_all.

  ENDMETHOD.

  METHOD put2history.

  ENDMETHOD.

ENDCLASS.



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

*CLASS lcl_receiver DEFINITION.
**  PUBLIC SECTION.
**    METHODS constructor.
**    METHODS os_linux_comm
**      IMPORTING iv_name       TYPE sxpglogcmd
**                iv_parameter  TYPE btcxpgpar OPTIONAL
**      RETURNING VALUE(rv_val) TYPE syindex.
**
**    METHODS os_linux_empty
**      IMPORTING it_params     TYPE mestringmap
**      RETURNING VALUE(rv_val) TYPE syindex.
**
**    METHODS os_read_file_in_dir
**      IMPORTING iv_name       TYPE sxpglogcmd
**                iv_parameter  TYPE btcxpgpar OPTIONAL
**      RETURNING VALUE(rv_val) TYPE syindex.
**
**
***    METHODS inform_done
***      IMPORTING iv_name       TYPE sxpglogcmd
***                iv_parameter  TYPE btcxpgpar
***      RETURNING VALUE(rv_val) TYPE syindex.
**
**
**  PROTECTED SECTION.
**
**  PRIVATE SECTION.
**    TYPES: tt_salfldir TYPE STANDARD TABLE OF salfldir WITH DEFAULT KEY.
**
**    METHODS exe_bash
**      IMPORTING iv_command_del TYPE sxpglogcmd
**                iv_parameter   TYPE btcxpgpar
**      EXPORTING ev_status      TYPE btcxpgstat
**                ev_exit_code   TYPE btcxpgexit
**                et_protocol    TYPE dba_exec_protocol.
**
**    METHODS get_files IMPORTING iv_dir        TYPE pfeflnamel
**                      RETURNING VALUE(rt_val) TYPE tt_salfldir.
**
*
*
*ENDCLASS.
*
*CLASS lcl_receiver IMPLEMENTATION.
**  METHOD constructor.
**
**  ENDMETHOD.
**
**  METHOD os_linux_comm.
**    " wget -P /usr/sap/MDD/D00/work/TECHLOAD/ http://data.nalog.ru/Public/Downloads/20191014/fias_delta_xml.rar
***    METHODS os_linux_comm
***        IMPORTING iv_name TYPE sxpglogcmd
***                  iv_parameter TYPE BTCXPGPAR.
**    DATA lv_status      TYPE btcxpgstat.
**    DATA lv_exit_code   TYPE btcxpgexit.
**    DATA lt_protocol    TYPE dba_exec_protocol.
**
**    rv_val = 0.
**
**    IF iv_name IS INITIAL.
**      rv_val = 9.
**      RETURN.
**    ENDIF.
**
**    exe_bash(
**      EXPORTING
**        iv_command_del = iv_name
**        iv_parameter   = iv_parameter
**      IMPORTING
**        ev_status      = lv_status
**        ev_exit_code   = lv_exit_code
**        et_protocol    = lt_protocol
**    ).
**
**    IF lv_status EQ 'E'.
**      rv_val = 4.
**    ENDIF.
**
**
**  ENDMETHOD.
**
**  METHOD os_linux_empty.
***    DATA lv_fold_tmp TYPE salfile-longname VALUE '/usr/sap/NPL/D00/work/wget_tmp/'.
***    DATA lv_fold_tmp_unrar TYPE salfile-longname VALUE '/usr/sap/NPL/D00/work/wget_tmp/unrar/'.
**    DATA lt_files TYPE tt_salfldir.
**    DATA lr_file TYPE REF TO salfldir.
**    rv_val = 0.
**    LOOP AT it_params REFERENCE INTO DATA(lr_params).
**      lt_files = get_files( iv_dir = CONV pfeflnamel( lr_params->value ) ).
**
**      LOOP AT lt_files REFERENCE INTO lr_file.
**        exe_bash(
**          EXPORTING
**            iv_command_del = 'ZWGET_CLEAR'
**            iv_parameter   = CONV btcxpgpar( CONV pfeflnamel( lr_params->value ) && lr_file->name )
***    IMPORTING
***      ev_status      = lv_status
***      ev_exit_code   = lv_exit_code
***      et_protocol    = lt_protocol
**    ).
**      ENDLOOP.
**    ENDLOOP.
**
**  ENDMETHOD.
**
**  METHOD os_read_file_in_dir.
***        IMPORTING iv_name       TYPE sxpglogcmd
***                iv_parameter  TYPE btcxpgpar OPTIONAL
***      RETURNING VALUE(rv_val) TYPE syindex.
**    DATA lt_files TYPE tt_salfldir.
**    rv_val = 9.
**    lt_files = get_files( iv_dir = CONV pfeflnamel( iv_parameter ) ).
**    IF lines( lt_files ) GE 1.
**      rv_val = 0.
**    ENDIF.
**
**    zcl_lsp012_html=>get_instance(
**        iv_title = |Files in folder' { iv_parameter } |
**    )->add_tab_ch( it_tab =  lt_files )->show( ).
**
**  ENDMETHOD.
**
**  METHOD get_files.
**    DATA lv_msg TYPE string.
**
**    CALL FUNCTION 'RZL_READ_DIR_LOCAL'
**      EXPORTING
**        name               = iv_dir                 " Dataset name
***       fromline           = 0                " Read as of line ( first line = 0 )
***       nrlines            = 1000             " Read number of lines
**      TABLES
**        file_tbl           = rt_val                " Lines
**      EXCEPTIONS
**        argument_error     = 1                " Incorrect call
**        not_found          = 2                " File not found
**        no_admin_authority = 3
**        OTHERS             = 4.
**    IF sy-subrc <> 0.
**      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
**    ELSE.
**      DELETE rt_val WHERE name EQ '.'.
**      DELETE rt_val WHERE name EQ '..'.
**      DELETE rt_val WHERE name CP '*.sh'.
**      DELETE rt_val WHERE name NP '*.*'.
**    ENDIF.
**
**  ENDMETHOD.
**
**  METHOD exe_bash.
***      IMPORTING iv_command_del TYPE sxpglogcmd
***                iv_parameter   TYPE btcxpgpar
***      EXPORTING ev_status      TYPE btcxpgstat
***                ev_exit_code   TYPE btcxpgexit
***                et_protocol    TYPE dba_exec_protocol.
**
**    CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
**      EXPORTING
**        commandname                   = iv_command_del                " Name of External Command
**        additional_parameters         = iv_parameter                 " Arguments for the External Command
***       operatingsystem               = SY-OPSYS         " Executing Host System
***       targetsystem                  = SY-HOST          " Target System
***       destination                   =
***       stdout                        = 'X'              " Log STDOUT Output Data
***       stderr                        = 'X'              " Log STDERR Output Data
***       terminationwait               = 'X'              " Wait for Termination of Exteranl Command
***       trace                         =                  " Perform Trace (Only for Tests)
***       dialog                        =                  " Reference type CHAR1 for background processing
**      IMPORTING
**        status                        = ev_status                 " Status
**        exitcode                      = ev_exit_code                 " Exitcode of External Program
**      TABLES
**        exec_protocol                 = et_protocol[]
**      EXCEPTIONS
**        no_permission                 = 1                " User Does Not Have Authorization
**        command_not_found             = 2                " External Command Not Defined
**        parameters_too_long           = 3                " Parameters Too Long
**        security_risk                 = 4                " Command Rejected for Security Reasons
**        wrong_check_call_interface    = 5                " Incorrect Call of a Function Module
**        program_start_error           = 6                " Error while Starting the External Command
**        program_termination_error     = 7                " Return Code with Errors
**        x_error                       = 8                " Reserved
**        parameter_expected            = 9                " No Additional Arguments Specified
**        too_many_parameters           = 10               " Too Many Parameters Specified
**        illegal_command               = 11               " External Command Not Defined Properly
**        wrong_asynchronous_parameters = 12               " Reserved
**        cant_enq_tbtco_entry          = 13               " Reserved
**        jobcount_generation_error     = 14               " Reserved
**        OTHERS                        = 15.
**    IF sy-subrc <> 0.
***     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
***       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
**    ENDIF.
**
**
**  ENDMETHOD.
*ENDCLASS.

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

CLASS lcl_command_empty_wget_fold DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_command_obj.
    METHODS constructor
      IMPORTING io_receiver TYPE REF TO zcl_lsp012_linux_os
                it_params   TYPE mestringmap.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS cv_comm TYPE sxpglogcmd VALUE 'ZWGET_CLEAR'.

    DATA mo_receiver TYPE REF TO zcl_lsp012_linux_os.
    DATA mt_params TYPE mestringmap.

ENDCLASS.

CLASS lcl_command_empty_wget_fold IMPLEMENTATION.
  METHOD constructor.
    mo_receiver ?= io_receiver.
    mt_params = it_params.
  ENDMETHOD.

  METHOD lif_command_obj~execute.
    rv_val = boolc( mo_receiver->os_linux_empty( mt_params ) EQ 0 ).
  ENDMETHOD.

ENDCLASS.

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

CLASS lcl_command_download DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_command_obj.
    METHODS constructor
      IMPORTING io_receiver TYPE REF TO zcl_lsp012_linux_os
                it_params   TYPE mestringmap.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS c_download_comm TYPE sxpglogcmd VALUE 'ZBASH'.

    DATA mo_receiver TYPE REF TO zcl_lsp012_linux_os.
    DATA mt_params TYPE mestringmap.

ENDCLASS.

CLASS lcl_command_download IMPLEMENTATION.
  METHOD constructor.
    mo_receiver ?= io_receiver.
    mt_params = it_params.
  ENDMETHOD.

  METHOD lif_command_obj~execute.
    rv_val = boolc( mo_receiver->os_linux_comm( iv_name      = me->c_download_comm
                                                iv_parameter = CONV btcxpgpar(
                                                mt_params[ name = 'LOC_DIR' ]-value
                                                && ` ` &&
                                                mt_params[ name = 'TRG1' ]-value )
                                              ) EQ 0
                  ).
  ENDMETHOD.

ENDCLASS.

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

CLASS lcl_command_unrar DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_command_obj.
    METHODS constructor
      IMPORTING io_receiver TYPE REF TO zcl_lsp012_linux_os
                it_params   TYPE mestringmap.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS cv_comm TYPE sxpglogcmd VALUE 'ZUNRAR'.

    DATA mo_receiver TYPE REF TO zcl_lsp012_linux_os.
    DATA mt_params TYPE mestringmap.

ENDCLASS.

CLASS lcl_command_unrar IMPLEMENTATION.
  METHOD constructor.
    mo_receiver ?= io_receiver.
    mt_params = it_params.
  ENDMETHOD.

  METHOD lif_command_obj~execute.
    DATA lv_param TYPE btcxpgpar.

    lv_param = mt_params[ name = 'RAR' ]-value && ` ` && mt_params[ name = 'FOLD' ]-value.

    rv_val = boolc( mo_receiver->os_linux_comm( iv_name      = me->cv_comm
                                                iv_parameter = lv_param
                                              ) EQ 0
                  ).
  ENDMETHOD.

ENDCLASS.

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

CLASS lcl_command_check_any_files DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_command_obj.
    METHODS constructor
      IMPORTING io_receiver TYPE REF TO zcl_lsp012_linux_os
                it_params   TYPE mestringmap.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS cv_comm TYPE sxpglogcmd VALUE 'ZUNRAR'.

    DATA mo_receiver TYPE REF TO zcl_lsp012_linux_os.
    DATA mt_params TYPE mestringmap.

ENDCLASS.

CLASS lcl_command_check_any_files IMPLEMENTATION.
  METHOD constructor.
    mo_receiver ?= io_receiver.
    mt_params = it_params.
  ENDMETHOD.

  METHOD lif_command_obj~execute.
    DATA lv_param TYPE btcxpgpar.

    lv_param = mt_params[ name = 'FOLD' ]-value.

    rv_val = boolc( mo_receiver->os_read_file_in_dir( iv_name      = me->cv_comm
                                                      iv_parameter = lv_param
                                                     ) EQ 0
                  ).
  ENDMETHOD.

ENDCLASS.

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_command DEFINITION.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS main.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_invoker TYPE REF TO lcl_invoker.
    DATA mo_receiver TYPE REF TO zcl_lsp012_linux_os.

    DATA mv_fold_tmp TYPE salfile-longname VALUE '/usr/sap/NPL/D00/work/wget_tmp/'.
    DATA mv_fold_tmp_unrar TYPE salfile-longname VALUE '/usr/sap/NPL/D00/work/wget_tmp/unrar/'.
    DATA mv_target_rar TYPE salfile-longname VALUE 'http://olegbash.ru/nftp/fias_delta.rar'.
    DATA mv_local_rar TYPE text200 VALUE '/usr/sap/NPL/D00/work/wget_tmp/fias_delta.rar'.
    DATA mv_local_fold TYPE text200 VALUE '/usr/sap/NPL/D00/work/wget_tmp/unrar'.

    METHODS run_step1.
    METHODS run_step2.
    METHODS run_step3.


ENDCLASS.

CLASS lcl_command IMPLEMENTATION.

  METHOD constructor.
    mo_invoker = NEW lcl_invoker(  ).
    "    mo_receiver = NEW zcl_lsp012_linux_os(  ).
    " vs
    "    create OBJECT mo_receiver.
    " vs
    mo_receiver = NEW #(  ).
  ENDMETHOD.                    "constructor

  METHOD main.
    run_step1( ).
    run_step2( ).
    run_step3( ).
  ENDMETHOD.                    "main

  METHOD run_step1.


    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    mo_invoker->add_command( io_command = NEW lcl_command_empty_wget_fold(
      io_receiver = mo_receiver
      it_params   = VALUE mestringmap( ( name = 'FOLD1' value = mv_fold_tmp )
                                       ( name = 'FOLD2' value = mv_fold_tmp_unrar ) ) ) ).
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(lo_command_download) = NEW lcl_command_download(
      io_receiver = mo_receiver
      it_params   = VALUE mestringmap( ( name = 'TRG1' value = mv_target_rar )
                                         ( name = 'LOC_DIR' value = mv_fold_tmp ) ) ).
    mo_invoker->add_command( io_command = lo_command_download ).
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    mo_invoker->add_command( io_command = NEW lcl_command_unrar(
       io_receiver = mo_receiver
       it_params   = VALUE mestringmap( ( name = 'RAR' value = mv_local_rar )
                                        ( name = 'FOLD' value = mv_local_fold ) ) ) ).
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  ENDMETHOD.                    "run_step1

  METHOD run_step2.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    mo_invoker->add_command( io_command = NEW lcl_command_check_any_files(
       io_receiver = mo_receiver
       it_params   = VALUE mestringmap( ( name = 'RAR' value = mv_local_rar )
                                        ( name = 'FOLD' value = mv_local_fold ) ) ) ).
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " last command

  ENDMETHOD.

  METHOD run_step3.
    mo_invoker->execute_all( ).
  ENDMETHOD.

ENDCLASS.
