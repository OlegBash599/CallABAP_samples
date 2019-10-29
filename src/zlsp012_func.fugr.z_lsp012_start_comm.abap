FUNCTION z_lsp012_start_comm.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_GUID) TYPE  SYSUUID_C
*"----------------------------------------------------------------------

  DATA ls_cluster TYPE ztlsp012_extcomm.
  DATA ls_cluster_data_inside TYPE zslsp012_comm.

  DATA lo_serializable_obj TYPE REF TO if_serializable_object.
  DATA lo_command_wget TYPE REF TO zcl_command4memento.

  SELECT SINGLE * FROM ztlsp012_extcomm
      INTO ls_cluster WHERE guid = iv_guid.

  IF ls_cluster IS INITIAL.
    RETURN.
  ENDIF.

  IMPORT comm_data = ls_cluster_data_inside
    FROM DATABASE ztlsp012_extcomm(i0)
    ID ls_cluster-guid.


  TRY.
      CALL TRANSFORMATION id_indent
            SOURCE XML ls_cluster_data_inside-xml_data
            RESULT     obj = lo_serializable_obj .

      lo_command_wget ?= lo_serializable_obj .

      lo_command_wget->zif_lsp012_command~execute( ).

    CATCH cx_root.
      MESSAGE x000(cl). " this is an epic fail
  ENDTRY.


ENDFUNCTION.
