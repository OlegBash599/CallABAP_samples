*&---------------------------------------------------------------------*
*& Include          ZLSP012_SHOW04_CLS03
*&---------------------------------------------------------------------*

INTERFACE lif_reader.
  TYPES: BEGIN OF ts_task_list.
          INCLUDE TYPE ztlsp012_tsklist.
  TYPES: END OF ts_task_list
        , tt_task_list TYPE STANDARD TABLE OF ts_task_list
            WITH DEFAULT KEY
        .

  METHODS get_data.
  METHODS show_data.

  DATA mv_id TYPE string READ-ONLY.
  DATA mt_task_list TYPE tt_task_list.


ENDINTERFACE.                    "lif_reader

*----------------------------------------------------------------------*
*       INTERFACE lif_writer
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
INTERFACE lif_writer.
  TYPES: BEGIN OF ts_task_list.
          INCLUDE TYPE ztlsp012_tsklist.
  TYPES: END OF ts_task_list
        , tt_task_list TYPE STANDARD TABLE OF ts_task_list
            WITH DEFAULT KEY
        .

  METHODS write_data.
  METHODS set_data
    IMPORTING it_task_list TYPE tt_task_list.

  DATA mv_id TYPE string READ-ONLY.
  DATA mt_task_list TYPE tt_task_list.

ENDINTERFACE.                    "lif_writer


*----------------------------------------------------------------------*
*       INTERFACE lif_factory
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
INTERFACE lif_factory.

  DATA mv_id TYPE string.

  METHODS get_writer
    RETURNING VALUE(ro_obj) TYPE REF TO lif_writer.

  METHODS get_reader
    RETURNING VALUE(ro_obj) TYPE REF TO  lif_reader.
ENDINTERFACE.                    "lif_factory


*----------------------------------------------------------------------*
*       CLASS lcl_reader_fs DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_reader_fs DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_reader.

    METHODS constructor
      IMPORTING iv_id TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS: cv_path2file TYPE string VALUE 'C:\20181008\task_list.txt'.

    METHODS check_exist
      RETURNING VALUE(rv_val) TYPE char1.

    METHODS fill_init_data.


ENDCLASS.                    "lif_reader DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_reader_fs IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_reader_fs IMPLEMENTATION.
  METHOD constructor.
    lif_reader~mv_id = iv_id.
    IF check_exist( ) EQ abap_false.
      fill_init_data( ).
    ENDIF.
  ENDMETHOD.                    "constructor

  METHOD lif_reader~get_data.

    DATA lt_task_list TYPE lif_reader~tt_task_list.

    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = cv_path2file
*       filetype                = 'ASC'
*       has_field_separator     = SPACE
*       header_length           = 0
*       read_by_line            = 'X'
*       dat_mode                = SPACE
*       codepage                = SPACE
*       ignore_cerr             = ABAP_TRUE
*       replacement             = '#'
*       virus_scan_profile      =
*       isdownload              = SPACE
*      IMPORTING
*       filelength              =
*       header                  =
      CHANGING
        data_tab                = lt_task_list
*       isscanperformed         = SPACE
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19.
    IF sy-subrc <> 0.
*     Implement suitable error handling here
    ENDIF.

    IF lt_task_list[] IS INITIAL.
    ELSE.
      me->lif_reader~mt_task_list[]  =  lt_task_list[].
    ENDIF.
  ENDMETHOD.                    "lif_reader~get_data

  METHOD check_exist.
    CLEAR rv_val.
    CALL METHOD cl_gui_frontend_services=>file_exist
      EXPORTING
        file                 = cv_path2file
      RECEIVING
        result               = rv_val
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.
    IF sy-subrc <> 0.
*     Implement suitable error handling here
    ENDIF.

  ENDMETHOD.                    "check_exist

  METHOD fill_init_data.
    DATA ls_task_list TYPE lif_reader~ts_task_list.
    CLEAR lif_reader~mt_task_list.

    DO 10 TIMES.
      ls_task_list-mandt = cl_abap_syst=>get_client( ).
      ls_task_list-taskn = 10 * sy-index.
      ls_task_list-taskn_sub = 1.
      ls_task_list-descr = |Init task line: { sy-index }|.
      ls_task_list-onuser = cl_abap_syst=>get_user_name( ).
      ls_task_list-status = 'A1'.
      ls_task_list-status_tx = 'Создан'.
      ls_task_list-comment_text = 'Авто генерация'.
      APPEND ls_task_list TO me->lif_reader~mt_task_list.
      CLEAR ls_task_list.
    ENDDO.

  ENDMETHOD.                    "fill_init_data

  METHOD lif_reader~show_data.
    ZCL_LSP012_HTML=>get_instance( iv_title = 'ABAP Table' )->add_para_val_ch(
        iv_id    = 'Reader FileSys'
        iv_value = | Кол-во записей { lines( me->lif_reader~mt_task_list ) }|
    )->add_tab_ch( it_tab = me->lif_reader~mt_task_list )->show( ).
  ENDMETHOD.                    "lif_reader~show_data

ENDCLASS.                    "lcl_reader_fs IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_writer_fs DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_writer_fs DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_writer.

    METHODS constructor
      IMPORTING iv_id TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS: cv_path2file TYPE string VALUE 'C:\20181008\task_list.txt'.

ENDCLASS.                    "lcl_writer_fs DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_writer_fs IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_writer_fs IMPLEMENTATION.
  METHOD constructor.
    lif_writer~mv_id = iv_id.
  ENDMETHOD.                    "constructor

  METHOD lif_writer~write_data.
    DATA lv_str TYPE string.

    IF lif_writer~mt_task_list IS INITIAL.
    ELSE.

      lv_str = cv_path2file.

      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
*         bin_filesize            =
          filename                = lv_str
          filetype                = 'TXT'
*         append                  = SPACE
*         write_field_separator   = SPACE
*         header                  = '00'
*         trunc_trailing_blanks   = SPACE
*         write_lf                = 'X'
*         col_select              = SPACE
*         col_select_mask         = SPACE
*         dat_mode                = SPACE
*         confirm_overwrite       = SPACE
*         no_auth_check           = SPACE
*         codepage                = SPACE
*         ignore_cerr             = ABAP_TRUE
*         replacement             = '#'
*         write_bom               = SPACE
*         trunc_trailing_blanks_eol = 'X'
*         wk1_n_format            = SPACE
*         wk1_n_size              = SPACE
*         wk1_t_format            = SPACE
*         wk1_t_size              = SPACE
*         show_transfer_status    = 'X'
*         fieldnames              =
*         write_lf_after_last_line  = 'X'
*         virus_scan_profile      = '/SCET/GUI_DOWNLOAD'
*           IMPORTING
*         filelength              =
        CHANGING
          data_tab                = lif_writer~mt_task_list
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          not_supported_by_gui    = 22
          error_no_gui            = 23
          OTHERS                  = 24.
      IF sy-subrc <> 0.
*          Implement suitable error handling here
      ENDIF.


    ENDIF.
  ENDMETHOD.                    "lif_writer~write_data
  METHOD lif_writer~set_data.
    lif_writer~mt_task_list[] = it_task_list[].
  ENDMETHOD.                    "lif_writer~set_data
ENDCLASS.                    "lcl_writer_fs IMPLEMENTATION




*----------------------------------------------------------------------*
*       CLASS lcl_reader_fs DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_reader_db DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_reader.

    METHODS constructor
      IMPORTING iv_id TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.


    METHODS fill_init_data.


ENDCLASS.                    "lif_reader DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_reader_fs IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_reader_db IMPLEMENTATION.
  METHOD constructor.
    lif_reader~mv_id = iv_id.

  ENDMETHOD.                    "constructor

  METHOD lif_reader~get_data.

"    SELECT * FROM ztlsp010_tsklist
    SELECT * FROM ztlsp012_tsklist
      INTO TABLE lif_reader~mt_task_list
      UP TO 1000 ROWS.

    IF sy-subrc EQ 0.
    ELSE.
      fill_init_data( ).
    ENDIF.

  ENDMETHOD.                    "lif_reader~get_data

  METHOD fill_init_data.
    DATA ls_task_list TYPE lif_reader~ts_task_list.
    CLEAR lif_reader~mt_task_list.

    DO 10 TIMES.
      ls_task_list-mandt = cl_abap_syst=>get_client( ).
      ls_task_list-taskn = 10 * sy-index.
      ls_task_list-taskn_sub = 1.
      ls_task_list-descr = |Init task line: { sy-index }|.
      ls_task_list-onuser = cl_abap_syst=>get_user_name( ).
      ls_task_list-status = 'A1'.
      ls_task_list-status_tx = 'Создан'.
      ls_task_list-comment_text = 'Авто генерация'.
      APPEND ls_task_list TO me->lif_reader~mt_task_list.
      CLEAR ls_task_list.
    ENDDO.

  ENDMETHOD.                    "fill_init_data

  METHOD lif_reader~show_data.
    ZCL_LSP012_HTML=>get_instance( iv_title = 'ABAP Table' )->add_para_val_ch(
        iv_id    = 'Reader DataBase'
        iv_value = | Кол-во записей { lines( me->lif_reader~mt_task_list ) }|
    )->add_tab_ch( it_tab = me->lif_reader~mt_task_list )->show( ).
  ENDMETHOD.                    "lif_reader~show_data

ENDCLASS.                    "lcl_reader_fs IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_writer_fs DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_writer_db DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_writer.

    METHODS constructor
      IMPORTING iv_id TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.                    "lcl_writer_fs DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_writer_fs IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_writer_db IMPLEMENTATION.
  METHOD constructor.
    lif_writer~mv_id = iv_id.
  ENDMETHOD.                    "constructor

  METHOD lif_writer~write_data.
"    MODIFY ztlsp010_tsklist FROM TABLE  lif_writer~mt_task_list.
    MODIFY ztlsp012_tsklist FROM TABLE  lif_writer~mt_task_list.
  ENDMETHOD.                    "lif_writer~write_data
  METHOD lif_writer~set_data.
    lif_writer~mt_task_list[] = it_task_list[].
  ENDMETHOD.                    "lif_writer~set_data
ENDCLASS.                    "lcl_writer_fs IMPLEMENTATION


"=================================
"=================================
"=================================
"=================================
"=================================
*----------------------------------------------------------------------*
*       CLASS lcl_factory_fs DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_factory_fs DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_factory.

    METHODS constructor
      IMPORTING iv_id TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_writer TYPE REF TO lcl_writer_fs.
    DATA mo_reader TYPE REF TO lcl_reader_fs.

ENDCLASS.                    "lcl_factory_fs DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_factory_fs IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_factory_fs IMPLEMENTATION.
  METHOD constructor.
    lif_factory~mv_id = iv_id.
  ENDMETHOD.                    "constructor

  METHOD lif_factory~get_reader.
    IF mo_reader IS BOUND.
      ro_obj ?= me->mo_reader.
      RETURN.
    ENDIF.

    CREATE OBJECT me->mo_reader
      EXPORTING
        iv_id = lif_factory~mv_id.
    ro_obj ?= me->mo_reader.
  ENDMETHOD.                    "lif_factory~get_reader

  METHOD lif_factory~get_writer.
    IF mo_writer IS BOUND.
      ro_obj ?= me->mo_writer.
      RETURN.
    ENDIF.

    CREATE OBJECT me->mo_writer
      EXPORTING
        iv_id = lif_factory~mv_id.
    ro_obj ?= me->mo_writer.
  ENDMETHOD.                    "lif_factory~get_writer

ENDCLASS.                    "lcl_factory_fs IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_factory_db DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_factory_db DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_factory.

    METHODS constructor
      IMPORTING iv_id TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_writer TYPE REF TO lcl_writer_db.
    DATA mo_reader TYPE REF TO lcl_reader_db.

ENDCLASS.                    "lcl_factory_db DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_factory_db IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_factory_db IMPLEMENTATION.
  METHOD constructor.
    lif_factory~mv_id = iv_id.
  ENDMETHOD.                    "constructor

  METHOD lif_factory~get_reader.
    IF mo_reader IS BOUND.
      ro_obj ?= me->mo_reader.
      RETURN.
    ENDIF.

    CREATE OBJECT me->mo_reader
      EXPORTING
        iv_id = lif_factory~mv_id.
    ro_obj ?= me->mo_reader.
  ENDMETHOD.                    "lif_factory~get_reader

  METHOD lif_factory~get_writer.
    IF mo_writer IS BOUND.
      ro_obj ?= me->mo_writer.
      RETURN.
    ENDIF.

    CREATE OBJECT me->mo_writer
      EXPORTING
        iv_id = lif_factory~mv_id.
    ro_obj ?= me->mo_writer.
  ENDMETHOD.                    "lif_factory~get_writer

ENDCLASS.                    "lcl_factory_db IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_factory DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*CLASS lcl_factory DEFINITION.
*  INTERFACES lif_factory.
*ENDCLASS.                    "lcl_factory DEFINITION
*
**----------------------------------------------------------------------*
**       CLASS lcl_factory IMPLEMENTATION
**----------------------------------------------------------------------*
**
**----------------------------------------------------------------------*
*CLASS lcl_factory IMPLEMENTATION.
*  METHOD lif_factory~get_reader.
*
*  ENDMETHOD.                    "lif_factory~get_reader
*  METHOD lif_factory~get_writer.
*
*  ENDMETHOD.                    "lif_factory~get_writer
*ENDCLASS.                    "lcl_factory IMPLEMENTATION
*

*----------------------------------------------------------------------*
*       CLASS lcl_abstract_factory DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_abstract_factory DEFINITION.
  PUBLIC SECTION.

    METHODS constructor.
    METHODS main.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_type_factory TYPE char2 VALUE 'FS'. " DB

    METHODS run_step1.

ENDCLASS.                    "lcl_abstract_factory DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_abstract_factory IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_abstract_factory IMPLEMENTATION.

  METHOD constructor.

  ENDMETHOD.                    "constructor

  METHOD main.
    mv_type_factory = 'FS'.
    run_step1( ).
    mv_type_factory = 'DB'.
    run_step1( ).

  ENDMETHOD.                    "main

  METHOD run_step1.
    DATA lo_factory TYPE REF TO lif_factory.
    DATA lo_reader TYPE REF TO lif_reader.
    DATA lo_writer TYPE REF TO lif_writer.

    CASE mv_type_factory.
      WHEN 'FS'.
        CREATE OBJECT lo_factory TYPE lcl_factory_fs
          EXPORTING
            iv_id = 'FS'.
      WHEN 'DB'.
        CREATE OBJECT lo_factory TYPE lcl_factory_db
          EXPORTING
            iv_id = 'DB'.
      WHEN OTHERS.
    ENDCASE.
    lo_reader = lo_factory->get_reader( ).

    lo_reader->get_data( ).
    lo_reader->show_data( ).


    lo_writer = lo_factory->get_writer( ).
    lo_writer->set_data( it_task_list = lo_reader->mt_task_list ).
    lo_writer->write_data( ).

  ENDMETHOD.                    "run_step1


ENDCLASS.
