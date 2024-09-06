*&---------------------------------------------------------------------*
*& Report ZREP_C8A014_READ_LONG_TXT
*&---------------------------------------------------------------------*
*& Read Complex LOng text with include
*&---------------------------------------------------------------------*
REPORT zrep_c8a014_read_long_txt.

PARAMETERS:   p_tdname TYPE thead-tdname DEFAULT 'ZSD_MAIL_ORDER_CREATED_ALL'
            , p_tdobj TYPE thead-tdid DEFAULT 'TEXT'
            , p_tdid TYPE thead-tdid DEFAULT 'ST'
            , p_langu TYPE sylangu DEFAULT 'RU'
            .

PARAMETERS: do_html AS CHECKBOX DEFAULT ''.

CLASS lcl_read_include_txt DEFINITION.

  PUBLIC SECTION.
    METHODS fn.
  PROTECTED SECTION.

  PRIVATE SECTION.

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

    METHODS _show_string
      IMPORTING iv_str_show TYPE string
                iv_no_wrap  TYPE abap_bool DEFAULT abap_false .

ENDCLASS.

CLASS lcl_read_include_txt IMPLEMENTATION.
  METHOD fn.

    DATA ls_txt_head_in   TYPE thead.
    DATA ls_txt_head_out  TYPE thead.
    DATA lt_txt_lines     TYPE tline_tab.

    DATA lv_txt_full_str TYPE string.
    DATA lv_txt_full_html TYPE string.

    ls_txt_head_in-tdid     = p_tdid.
    ls_txt_head_in-tdspras  = p_langu.
    ls_txt_head_in-tdobject = p_tdobj.
    ls_txt_head_in-tdname   = p_tdname.

    _read_text_lines( EXPORTING is_txt_head = ls_txt_head_in
                      IMPORTING es_txt_head = ls_txt_head_out
                                et_txt_lines = lt_txt_lines ).
    CASE abap_true.

      WHEN do_html.
        _convert2html( EXPORTING is_txt_head  = ls_txt_head_out
                        CHANGING ct_txt_lines = lt_txt_lines
                                 cv_txt_str   = lv_txt_full_html ).

        _show_string( EXPORTING iv_str_show = lv_txt_full_html
                                iv_no_wrap  = abap_true ).

      WHEN OTHERS.

        _replace_include_n_controls( CHANGING cs_txt_head = ls_txt_head_out
                                      ct_txt_lines = lt_txt_lines ).

        _convert2string( CHANGING ct_txt_lines = lt_txt_lines
                                  cv_txt_str = lv_txt_full_str ).

        _show_string( lv_txt_full_str ).
    ENDCASE.

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
*       program   = space            " Program name for program symbol replacement
*       replace_comment = 'X'              " Remove comment lines
      IMPORTING
        changed   = lv_was_changed_control                 " Replace control structures
        newheader = cs_txt_head                 " Text header (new)
      TABLES
        lines     = ct_txt_lines.                 " Text lines

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

  METHOD _show_string.
    "IMPORTING iv_str_show TYPE string.

    DATA lv_as_html_out TYPE string.

    IF iv_no_wrap EQ abap_false.
      lv_as_html_out =
        | <html dir="ltr" lang="ru"> |
    && |<head> <meta charset="UTF-8">|
    && |<meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1">|
    && |<title>ABAP String Show </title>|
    && |</head> <body>|
    && |<div> { iv_str_show } </div>|
    && |</body> </html>|
    .
    ELSE.
      lv_as_html_out = iv_str_show.
    ENDIF.


    cl_abap_browser=>show_html(
      EXPORTING
*        html         =                            " HTML Table, Line Width 255 Characters
*        title        =                            " Window Title
*        size         = cl_abap_browser=>medium    " Size (S,M.L,XL)
*        modal        = abap_true                  " Dialog box is modal (else modeless)
        html_string  = lv_as_html_out                           " HTML String
*        printing     = abap_false                 " Key for printing
*        buttons      = navigate_off               " Navigation Keys navigate_...
*        format       = cl_abap_browser=>landscape " Landscape/portrait format
*        position     = cl_abap_browser=>topleft   " Position
*        data_table   =                            " External data
*        anchor       =                            " Goto Point
*        context_menu = abap_false                 " Display context menu in browser
*        html_xstring =                            " HTML Binary String
*        check_html   = abap_true                  " Test of HTML File
*        container    =                            " Container for display
*        dialog       = abap_true                  " Display in dialog box
*      IMPORTING
*        html_errors  =                            " Error List from Test
    ).

  ENDMETHOD.
ENDCLASS.



START-OF-SELECTION.
  NEW lcl_read_include_txt( )->fn( ).
