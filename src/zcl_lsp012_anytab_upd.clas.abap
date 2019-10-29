class ZCL_LSP012_ANYTAB_UPD definition
  public
  create public .

public section.

  interfaces ZIF_LSP012_ANYTAB_UPD .
protected section.
private section.

  methods GET_JSON_STRING
    importing
      !IT_DATA type ANY
    exporting
      !EV_JSON_STR type STRING .
ENDCLASS.



CLASS ZCL_LSP012_ANYTAB_UPD IMPLEMENTATION.


  METHOD get_json_string.

    DATA lo_writer TYPE REF TO cl_sxml_string_writer.
    DATA lv_xstr_json TYPE xstring.
    DATA lv_str_json TYPE string.
    DATA lv_reponse2pend TYPE string.
    lo_writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).

    CALL TRANSFORMATION id SOURCE json = it_data
                           RESULT XML lo_writer.

    lv_xstr_json = lo_writer->get_output( ).
    TRY.
        lv_str_json = cl_abap_codepage=>convert_from(
            source      = lv_xstr_json
*        codepage    = `UTF-8`
*        endian      =
*        replacement = '#'
*        ignore_cerr = ABAP_FALSE
        ).
      CATCH cx_parameter_invalid_range.  " Parameter with Invalid Range
      CATCH cx_sy_codepage_converter_init.  " System Exception for Code Page Converter Initialization
      CATCH cx_sy_conversion_codepage.  " System Exception Converting Character Set
      CATCH cx_parameter_invalid_type.  " Parameter with Invalid Type
    ENDTRY.




    "REPLACE FIRST OCCURRENCE OF '{"JSON":[' IN ev_json_str WITH '{"request":['.
    REPLACE ALL OCCURRENCES OF '{"JSON":' IN lv_str_json WITH ''.
    lv_str_json = shift_right( val = lv_str_json places = 1 ).
    " as in ZCL_JSON

    ev_json_str = lv_str_json.

  ENDMETHOD.


  METHOD zif_lsp012_anytab_upd~put2upd_task.


    DATA lv_json_str TYPE string.
    DATA ls_return TYPE bapiret2.

    CLEAR rv_val.

    me->get_json_string( EXPORTING it_data     = it_data
                         IMPORTING ev_json_str = lv_json_str ).



    CALL FUNCTION 'Z_LSP012_ANYTAB_MODIFY'
      IN UPDATE TASK
      EXPORTING
        iv_tabname  = iv_tabname                 " Table Name
        iv_json_str = lv_json_str.

    IF iv_do_commit EQ abap_true.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait   = abap_true                 " Use of Command `COMMIT AND WAIT`
        IMPORTING
          return = ls_return.                " Return Messages

      IF ls_return-type EQ 'E'.
        rv_val = 4.
      ELSE.
        CLEAR rv_val.
      ENDIF.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
