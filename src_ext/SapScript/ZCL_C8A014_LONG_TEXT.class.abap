
CLASS zcl_c8a014_long_text DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING iv_sycprog TYPE sycprog.

    METHODS r
      IMPORTING is_txt    TYPE thead
      RETURNING VALUE(rv) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_sycprog TYPE sycprog.

    METHODS _read_text_lines
      IMPORTING is_txt_head  TYPE thead
      EXPORTING es_txt_head  TYPE thead
                et_txt_lines TYPE tline_tab.

    METHODS _replace_include_n_controls
      CHANGING cs_txt_head  TYPE thead
               ct_txt_lines TYPE tline_tab.

    METHODS _convert2string
      CHANGING ct_txt_lines TYPE tline_tab
               cv_txt_str   TYPE string.

    METHODS _convert2html
      IMPORTING is_txt_head  TYPE thead
      CHANGING  ct_txt_lines TYPE tline_tab
                cv_txt_str   TYPE string.
ENDCLASS.



CLASS zcl_c8a014_long_text IMPLEMENTATION.


  METHOD constructor.
    mv_sycprog = iv_sycprog.
  ENDMETHOD.


  METHOD r.
    "IMPORTING is        TYPE ts_in_var
    "RETURNING VALUE(rv) TYPE string.
    DATA ls_txt_head_out  TYPE thead.
    DATA lt_txt_lines     TYPE tline_tab.


    _read_text_lines( EXPORTING is_txt_head = is_txt
                      IMPORTING es_txt_head = ls_txt_head_out
                                et_txt_lines = lt_txt_lines ).

    _replace_include_n_controls( CHANGING cs_txt_head = ls_txt_head_out
                                      ct_txt_lines = lt_txt_lines ).

    _convert2html( EXPORTING is_txt_head = ls_txt_head_out
                     CHANGING ct_txt_lines = lt_txt_lines
                              cv_txt_str = rv ).
  ENDMETHOD.


  METHOD _convert2html.
    "CHANGING ct_txt_lines TYPE tline_tab
    "         cv_txt_str   TYPE string.

    DATA lt_htmlline TYPE htmltable.
    FIELD-SYMBOLS <fs_html_line> TYPE htmlline.

    CLEAR cv_txt_str .

    CALL FUNCTION 'CONVERT_ITF_TO_HTML'
      EXPORTING
*       i_codepage     =                  " Target character set
        i_header       = is_txt_head                 " Text header of input text
*       i_page         = space            " Page specification for page window format
*       i_window       = space            " Window specification for page window format
*       i_syntax_check = space            " Activating the ITF syntax check
*       i_replace      = 'X'              " Expanding symbols and includes
*       i_print_commands   = space            " Outputting commands and text elements
*       i_html_header  = 'X'              " Output of the HTML header tag
*       i_funcname     = space            " Exit module for link interpretation
*       i_title        = space            " Title in HTML header
*       i_background   = space            " File name for HTML image as background
*       i_bgcolor      = space            " Background color of text
*       i_unescape_formats =                  " Exception Formats for Masking Special Characters
*       i_escape_spaces    = space            " Masking of Blank Characters
*       i_encoding     = space            " Define Character Set Coding in Header
*      IMPORTING
*       e_html_text    =                  " Text Content in HTML as Xstring
      TABLES
        t_itf_text     = ct_txt_lines                 " Text lines in ITF (input)
        t_html_text    = lt_htmlline                 " Text lines in HTML (output)
*       t_conv_charformats =                  " Table for character formats
*       t_conv_parformats  =                  " Table for paragraph formats
      EXCEPTIONS
        syntax_check   = 1                " Incorrect inout text ITF syntax
        replace        = 2                " Errors expanding includes and symbols
        illegal_header = 3                " Text header incorrect
        OTHERS         = 4.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    LOOP AT lt_htmlline ASSIGNING <fs_html_line>.
      IF cv_txt_str  IS INITIAL.
        cv_txt_str  = <fs_html_line>-tdline.
      ELSE.
        cv_txt_str  = cv_txt_str && <fs_html_line>-tdline.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD _convert2string.
    "CHANGING ct_txt_lines TYPE tline_tab
    "         cv_txt_str   TYPE string.

    DATA lv_sep TYPE string.

    DATA lt_stream_lines_wdyn TYPE  string_table.
    DATA lt_stream_lines_gui TYPE  STANDARD TABLE OF txt255 WITH DEFAULT KEY.
    FIELD-SYMBOLS <fs_stream_line> TYPE string.


    CLEAR cv_txt_str.

    IF ct_txt_lines IS INITIAL.
      RETURN.
    ENDIF.

    lv_sep = cl_abap_char_utilities=>newline.

    CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
      EXPORTING
*       language     = SY-LANGU         " Text Language
        lf           = abap_true
      IMPORTING
        stream_lines = lt_stream_lines_wdyn
      TABLES
        itf_text     = ct_txt_lines
        text_stream  = lt_stream_lines_gui.

    LOOP AT lt_stream_lines_wdyn ASSIGNING <fs_stream_line>.
      IF cv_txt_str IS INITIAL.
        cv_txt_str = <fs_stream_line>.
      ELSE.
        cv_txt_str = cv_txt_str && lv_sep && <fs_stream_line>.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD _read_text_lines.
    "IMPORTING is_txt_head  TYPE thead
    "EXPORTING es_txt_head  TYPE thead
    "          et_txt_lines TYPE tline_tab.

    DATA ls_txt_head_out  TYPE thead.
    DATA lt_txt_lines     TYPE tline_tab.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = is_txt_head-tdid     " Text ID of text to be read
        language                = is_txt_head-tdspras  " Language of text to be read
        name                    = is_txt_head-tdname   " Name of text to be read
        object                  = is_txt_head-tdobject " Object of text to be read
*       archive_handle          = 0                " Archive Handle
*       local_cat               = space            " Text catalog local
      IMPORTING
        header                  = ls_txt_head_out
*       old_line_counter        =                  " Original Number of Text Lines
      TABLES
        lines                   = lt_txt_lines
      EXCEPTIONS
        id                      = 1                " Text ID invalid
        language                = 2                " Invalid language
        name                    = 3                " Invalid text name
        not_found               = 4                " Text not found
        object                  = 5                " Invalid text object
        reference_check         = 6                " Reference chain interrupted
        wrong_access_to_archive = 7                " Archive handle invalid for access
        OTHERS                  = 8.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
      RETURN.
    ENDIF.

    es_txt_head = ls_txt_head_out.
    et_txt_lines = lt_txt_lines.

  ENDMETHOD.


  METHOD _replace_include_n_controls.
    "CHANGING cs_txt_head  TYPE thead
    "         ct_txt_lines TYPE tline_tab.

    DATA lv_was_changed_replace TYPE abap_bool.
    DATA lv_was_changed_control TYPE abap_bool.
    DATA lv_was_changed_symbol TYPE abap_bool.

    IF ct_txt_lines IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'TEXT_INCLUDE_REPLACE'
      EXPORTING
*       all_level = 'X'              " Expand nested INCLUDES
*       endline   = 99999            " End line for INCLUDE replacement
        header    = cs_txt_head                " Text header
*       startline = 1                " Start line for INCLUDE replacement
*       program   = space            " Program name for symbol replacement
      IMPORTING
        changed   = lv_was_changed_replace                 " Indicator if text was changed
*       error_type =                  " Error type
        newheader = cs_txt_head                 " Text header (new)
      TABLES
        lines     = ct_txt_lines.                 " Text lines


    CALL FUNCTION 'TEXT_CONTROL_REPLACE'
      EXPORTING
        header    = cs_txt_head                 " Text header
        program   = mv_sycprog            " Program name for program symbol replacement
*       replace_comment = 'X'              " Remove comment lines
      IMPORTING
        changed   = lv_was_changed_control                 " Replace control structures
        newheader = cs_txt_head                 " Text header (new)
      TABLES
        lines     = ct_txt_lines.                 " Text lines

    CALL FUNCTION 'TEXT_SYMBOL_REPLACE'
      EXPORTING
*       endline   = 99999
        header    = cs_txt_head
*       init      = ' '
*       option_dialog    = ' '
        program   = mv_sycprog
*       replace_program  = 'X'
*       replace_standard = 'X'
*       replace_system   = 'X'
*       replace_text     = 'X'
*       startline = 1
      IMPORTING
        changed   = lv_was_changed_symbol
        newheader = cs_txt_head
      TABLES
        lines     = ct_txt_lines.

  ENDMETHOD.
ENDCLASS.
