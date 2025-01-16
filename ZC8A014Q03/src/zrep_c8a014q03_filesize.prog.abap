*&---------------------------------------------------------------------*
*& Report ZREP_C8A014Q03_FILESIZE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrep_c8a014q03_filesize.

PARAMETERS: srv_file TYPE string DEFAULT 'simple_text.txt' LOWER CASE
  OBLIGATORY.
PARAMETERS: do_break AS CHECKBOX DEFAULT ' '.

CLASS lcl_app DEFINITION.

  PUBLIC SECTION.

    METHODS main.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mv_path2file TYPE string.

    METHODS option01_strlen
      RETURNING VALUE(rv) TYPE syindex.

    METHODS option02_actual_size
      RETURNING VALUE(rv) TYPE syindex.

    METHODS option03_os_command
      RETURNING VALUE(rv) TYPE syindex.

    METHODS option04_os_func_syscom
      RETURNING VALUE(rv) TYPE syindex.

ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD main.

    DATA lv_file_size_opt1 TYPE syindex.
    DATA lv_file_size_opt2 TYPE syindex.
    DATA lv_file_size_opt3 TYPE syindex.
    DATA lv_file_size_opt4 TYPE syindex.

    IF do_break EQ abap_true.
      BREAK-POINT.
    ENDIF.

    mv_path2file = srv_file.

    lv_file_size_opt1 = option01_strlen( ).
    lv_file_size_opt2 = option02_actual_size( ).
    lv_file_size_opt3 = option03_os_command( ).
    lv_file_size_opt4 = option04_os_func_syscom( ).

  ENDMETHOD.


  METHOD option01_strlen.
    DATA lv_file_size_all TYPE syindex.
    DATA lv_file_size_part TYPE syindex.
    DATA lv_file_bin TYPE xstring.
    DATA lv_file_bin_part TYPE xstring.


    OPEN DATASET mv_path2file FOR INPUT IN BINARY MODE.
    IF sy-subrc EQ 0.
      DO.
        READ DATASET mv_path2file INTO lv_file_bin_part.
        IF sy-subrc EQ 0.
          IF lv_file_bin IS INITIAL.
            lv_file_bin = lv_file_bin_part.
          ELSE.
            lv_file_bin = lv_file_bin && lv_file_bin_part.
          ENDIF.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
      CLOSE DATASET mv_path2file.
      lv_file_size_all = xstrlen( lv_file_bin ).
    ELSE.
      MESSAGE i000(cl) WITH 'file ' mv_path2file ' does not exist'.
    ENDIF.

    rv = lv_file_size_all.

  ENDMETHOD.

  METHOD option02_actual_size.
    "RETURNING VALUE(rv) TYPE syindex.
    DATA lv_file_size_all TYPE syindex.
    DATA lv_file_size_part TYPE syindex.
    DATA lv_file_bin TYPE xstring.
    DATA lv_file_bin_part TYPE xstring.

    lv_file_size_all = 0.
    OPEN DATASET mv_path2file FOR INPUT IN BINARY MODE.
    IF sy-subrc EQ 0.
      DO.
        READ DATASET mv_path2file INTO lv_file_bin_part ACTUAL LENGTH lv_file_size_part.
        IF sy-subrc EQ 0.
          IF lv_file_size_all IS INITIAL.
            lv_file_size_all = lv_file_size_part.
          ELSE.
            lv_file_size_all = lv_file_size_all + lv_file_size_part.
          ENDIF.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
      CLOSE DATASET mv_path2file.
    ENDIF.

    rv = lv_file_size_all.

  ENDMETHOD.

  METHOD option03_os_command.
    "RETURNING VALUE(rv) TYPE syindex.

    " Linux/UNIX sample (as Server Operation System)
    DATA lv_trg_folder TYPE string VALUE '/usr/sap/A4H/D00/work/'.
    DATA lv_filepath_dataset_name TYPE string.

    DATA lv_command_in_sm49 TYPE sxpglogcmd VALUE 'DIR'.
    DATA lv_command_add_params TYPE btcxpgpar.
    DATA lv_oper_sys_as_in_sm49 TYPE syopsys VALUE 'UNIX'.
    DATA lt_exec_protocoll TYPE STANDARD TABLE OF btcxpm.

    DATA lt_stdout_comm TYPE STANDARD TABLE OF string.
    DATA lv_file_size_all TYPE syindex.
    DATA ls_ext_status TYPE extcmdexex.
    FIELD-SYMBOLS <fs_stdout_comm> TYPE string.

    lv_filepath_dataset_name = lv_trg_folder && mv_path2file.

    lv_command_add_params = lv_filepath_dataset_name && ` ` && '-ls1'.

    CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
      EXPORTING
        commandname                   = lv_command_in_sm49
        additional_parameters         = lv_command_add_params
        operatingsystem               = lv_oper_sys_as_in_sm49
*       targetsystem                  = SY-HOST
*       destination                   =
*       stdout                        = 'X'
*       stderr                        = 'X'
*       terminationwait               = 'X'
*       trace                         =
*       dialog                        =
      IMPORTING
        status                        = ls_ext_status-status
        exitcode                      = ls_ext_status-exitcode
      TABLES
        exec_protocol                 = lt_exec_protocoll
      EXCEPTIONS
        no_permission                 = 1
        command_not_found             = 2
        parameters_too_long           = 3
        security_risk                 = 4
        wrong_check_call_interface    = 5
        program_start_error           = 6
        program_termination_error     = 7
        x_error                       = 8
        parameter_expected            = 9
        too_many_parameters           = 10
        illegal_command               = 11
        wrong_asynchronous_parameters = 12
        cant_enq_tbtco_entry          = 13
        jobcount_generation_error     = 14
        OTHERS                        = 15.
    IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      SPLIT VALUE #( lt_exec_protocoll[ 1 ]-message OPTIONAL )
         AT ` `  " проблем в формате строке
         INTO TABLE lt_stdout_comm.

      " современно
      lv_file_size_all = VALUE #( lt_stdout_comm[ 6 ] OPTIONAL ).

      " консервативно
      LOOP AT lt_stdout_comm ASSIGNING <fs_stdout_comm>. " where by type casting? )))) - no in abap
        IF sy-tabix EQ 6.
          lv_file_size_all = <fs_stdout_comm>.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

    rv = lv_file_size_all.
  ENDMETHOD.

  METHOD option04_os_func_syscom.
    "RETURNING VALUE(rv) TYPE syindex.
    DATA lv_long_file_name TYPE  eps2filnam.
    DATA lv_long_dir_name TYPE eps2path VALUE '/usr/sap/A4H/D00/work/'.

    DATA lv_file_size TYPE  epsf-epsfilsiz.
    DATA lv_file_owner TYPE  epsf-epsfilown.
    DATA lv_file_mode TYPE  epsf-epsfilmod.
    DATA lv_file_type TYPE  epsf-epsfiltyp.
    DATA lv_file_mtime TYPE string.
    DATA lv_file_size_long TYPE  p.

    TRY.
        lv_long_file_name = EXACT #( mv_path2file ).
      CATCH cx_sy_conversion_error INTO DATA(exc).
        rv = 0.
        RETURN.
    ENDTRY.



    CALL FUNCTION 'EPS_GET_FILE_ATTRIBUTES'
      EXPORTING
*       file_name              =
        iv_long_file_name      = lv_long_file_name
*       dir_name               =
        iv_long_dir_name       = lv_long_dir_name
      IMPORTING
        file_size              = lv_file_size
        file_owner             = lv_file_owner
        file_mode              = lv_file_mode
        file_type              = lv_file_type
        file_mtime             = lv_file_type
        file_size_long         = lv_file_size_long
      EXCEPTIONS
        read_directory_failed  = 1
        read_attributes_failed = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
    ENDIF.

    rv = lv_file_size.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW lcl_app( )->main( ).

end-of-SELECTION.
