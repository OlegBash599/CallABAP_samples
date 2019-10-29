FUNCTION z_lsp012_anytab_modify.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TABNAME) TYPE  TABNAME
*"     VALUE(IV_JSON_STR) TYPE  STRING
*"----------------------------------------------------------------------

  DATA lv_json_str TYPE string.
  DATA lo_rtti_util TYPE REF TO lcl_rtti_util.
  DATA lr_db_content     TYPE REF TO data.
  DATA lr_reader TYPE REF TO if_sxml_reader.

  FIELD-SYMBOLS <fs_dyn_tab> TYPE STANDARD TABLE.

  CREATE OBJECT lo_rtti_util
    EXPORTING
      iv_tabname = iv_tabname.

  TRY .
      lo_rtti_util->create_tab_n_line(
    IMPORTING
      et_ref     = lr_db_content ).

    CATCH zcx_lsp012_error.
      MESSAGE e000(cl).
      RETURN.
  ENDTRY.


  ASSIGN lr_db_content->* TO  <fs_dyn_tab> .
  lv_json_str = '{"JSON":' && iv_json_str && '}'.


  TRY.

      CLEAR <fs_dyn_tab>.

      CALL TRANSFORMATION id
        SOURCE XML lv_json_str
        RESULT json = <fs_dyn_tab>.

    CATCH  cx_root.
      MESSAGE e000(cl).
  ENDTRY.


  TRY.
      MODIFY (iv_tabname) FROM TABLE <fs_dyn_tab>.
      " OK
    CATCH cx_sy_open_sql_db.
      MESSAGE e000(cl).
      RETURN.
  ENDTRY.




ENDFUNCTION.
