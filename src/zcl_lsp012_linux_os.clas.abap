CLASS zcl_lsp012_linux_os DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.
    METHODS os_linux_comm
      IMPORTING iv_name       TYPE sxpglogcmd
                iv_parameter  TYPE btcxpgpar OPTIONAL
      RETURNING VALUE(rv_val) TYPE syindex.

    METHODS os_linux_empty
      IMPORTING it_params     TYPE mestringmap
      RETURNING VALUE(rv_val) TYPE syindex.

    METHODS os_read_file_in_dir
      IMPORTING iv_name       TYPE sxpglogcmd
                iv_parameter  TYPE btcxpgpar OPTIONAL
      RETURNING VALUE(rv_val) TYPE syindex.


*    METHODS inform_done
*      IMPORTING iv_name       TYPE sxpglogcmd
*                iv_parameter  TYPE btcxpgpar
*      RETURNING VALUE(rv_val) TYPE syindex.


  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES: tt_salfldir TYPE STANDARD TABLE OF salfldir WITH DEFAULT KEY.

    METHODS exe_bash
      IMPORTING iv_command_del TYPE sxpglogcmd
                iv_parameter   TYPE btcxpgpar
      EXPORTING ev_status      TYPE btcxpgstat
                ev_exit_code   TYPE btcxpgexit
                et_protocol    TYPE dba_exec_protocol.

    METHODS get_files IMPORTING iv_dir        TYPE pfeflnamel
                      RETURNING VALUE(rt_val) TYPE tt_salfldir.


ENDCLASS.



CLASS zcl_lsp012_linux_os IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.

  METHOD os_linux_comm.
  " example where OS is more efficient than through ABAP environment
    " wget -P /usr/sap/MDD/D00/work/TECHLOAD/ http://data.nalog.ru/Public/Downloads/20191014/fias_delta_xml.rar
*    METHODS os_linux_comm
*        IMPORTING iv_name TYPE sxpglogcmd
*                  iv_parameter TYPE BTCXPGPAR.
    DATA lv_status      TYPE btcxpgstat.
    DATA lv_exit_code   TYPE btcxpgexit.
    DATA lt_protocol    TYPE dba_exec_protocol.

    rv_val = 0.

    IF iv_name IS INITIAL.
      rv_val = 9.
      RETURN.
    ENDIF.

    exe_bash(
      EXPORTING
        iv_command_del = iv_name
        iv_parameter   = iv_parameter
      IMPORTING
        ev_status      = lv_status
        ev_exit_code   = lv_exit_code
        et_protocol    = lt_protocol
    ).

    IF lv_status EQ 'E'.
      rv_val = 4.
    ENDIF.


  ENDMETHOD.

  METHOD os_linux_empty.
*    DATA lv_fold_tmp TYPE salfile-longname VALUE '/usr/sap/NPL/D00/work/wget_tmp/'.
*    DATA lv_fold_tmp_unrar TYPE salfile-longname VALUE '/usr/sap/NPL/D00/work/wget_tmp/unrar/'.
    DATA lt_files TYPE tt_salfldir.
    DATA lr_file TYPE REF TO salfldir.
    rv_val = 0.
    LOOP AT it_params REFERENCE INTO DATA(lr_params).
      lt_files = get_files( iv_dir = CONV pfeflnamel( lr_params->value ) ).

      LOOP AT lt_files REFERENCE INTO lr_file.
        exe_bash(
          EXPORTING
            iv_command_del = 'ZWGET_CLEAR'
            iv_parameter   = CONV btcxpgpar( CONV pfeflnamel( lr_params->value ) && lr_file->name )
*    IMPORTING
*      ev_status      = lv_status
*      ev_exit_code   = lv_exit_code
*      et_protocol    = lt_protocol
    ).
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

  METHOD os_read_file_in_dir.
*        IMPORTING iv_name       TYPE sxpglogcmd
*                iv_parameter  TYPE btcxpgpar OPTIONAL
*      RETURNING VALUE(rv_val) TYPE syindex.
    DATA lt_files TYPE tt_salfldir.
    rv_val = 9.
    lt_files = get_files( iv_dir = CONV pfeflnamel( iv_parameter ) ).
    IF lines( lt_files ) GE 1.
      rv_val = 0.
    ENDIF.

    zcl_lsp012_html=>get_instance(
        iv_title = |Files in folder' { iv_parameter } |
    )->add_tab_ch( it_tab =  lt_files )->show( ).

  ENDMETHOD.

  METHOD get_files.
    DATA lv_msg TYPE string.

    CALL FUNCTION 'RZL_READ_DIR_LOCAL'
      EXPORTING
        name               = iv_dir                 " Dataset name
*       fromline           = 0                " Read as of line ( first line = 0 )
*       nrlines            = 1000             " Read number of lines
      TABLES
        file_tbl           = rt_val                " Lines
      EXCEPTIONS
        argument_error     = 1                " Incorrect call
        not_found          = 2                " File not found
        no_admin_authority = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
    ELSE.
      DELETE rt_val WHERE name EQ '.'.
      DELETE rt_val WHERE name EQ '..'.
      DELETE rt_val WHERE name CP '*.sh'.
      DELETE rt_val WHERE name NP '*.*'.
    ENDIF.

  ENDMETHOD.

  METHOD exe_bash.
*      IMPORTING iv_command_del TYPE sxpglogcmd
*                iv_parameter   TYPE btcxpgpar
*      EXPORTING ev_status      TYPE btcxpgstat
*                ev_exit_code   TYPE btcxpgexit
*                et_protocol    TYPE dba_exec_protocol.

    CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
      EXPORTING
        commandname                   = iv_command_del                " Name of External Command
        additional_parameters         = iv_parameter                 " Arguments for the External Command
*       operatingsystem               = SY-OPSYS         " Executing Host System
*       targetsystem                  = SY-HOST          " Target System
*       destination                   =
*       stdout                        = 'X'              " Log STDOUT Output Data
*       stderr                        = 'X'              " Log STDERR Output Data
*       terminationwait               = 'X'              " Wait for Termination of Exteranl Command
*       trace                         =                  " Perform Trace (Only for Tests)
*       dialog                        =                  " Reference type CHAR1 for background processing
      IMPORTING
        status                        = ev_status                 " Status
        exitcode                      = ev_exit_code                 " Exitcode of External Program
      TABLES
        exec_protocol                 = et_protocol[]
      EXCEPTIONS
        no_permission                 = 1                " User Does Not Have Authorization
        command_not_found             = 2                " External Command Not Defined
        parameters_too_long           = 3                " Parameters Too Long
        security_risk                 = 4                " Command Rejected for Security Reasons
        wrong_check_call_interface    = 5                " Incorrect Call of a Function Module
        program_start_error           = 6                " Error while Starting the External Command
        program_termination_error     = 7                " Return Code with Errors
        x_error                       = 8                " Reserved
        parameter_expected            = 9                " No Additional Arguments Specified
        too_many_parameters           = 10               " Too Many Parameters Specified
        illegal_command               = 11               " External Command Not Defined Properly
        wrong_asynchronous_parameters = 12               " Reserved
        cant_enq_tbtco_entry          = 13               " Reserved
        jobcount_generation_error     = 14               " Reserved
        OTHERS                        = 15.
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
