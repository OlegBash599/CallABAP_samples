CLASS zcl_command4memento DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_lsp012_command.
    INTERFACES if_serializable_object.

    METHODS constructor
      IMPORTING it_params TYPE mestringmap.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_params TYPE mestringmap.
    DATA mo_receiver TYPE REF TO zcl_lsp012_linux_os.
ENDCLASS.



CLASS zcl_command4memento IMPLEMENTATION.
  METHOD constructor.
    mt_params = it_params.
  ENDMETHOD.

  METHOD zif_lsp012_command~execute.
    IF mo_receiver IS BOUND.
    ELSE.
      mo_receiver = NEW #(  ).
    ENDIF.
    mo_receiver->os_linux_empty( it_params = mt_params ).
  ENDMETHOD.
ENDCLASS.
