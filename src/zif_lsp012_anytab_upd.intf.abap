interface ZIF_LSP012_ANYTAB_UPD
  public .


  methods PUT2UPD_TASK
    importing
      !IV_TABNAME type TABNAME
      !IT_DATA type ANY
      !IV_DO_COMMIT type CHAR1 default ABAP_FALSE
    returning
      value(RV_VAL) type SYTABIX .
endinterface.
