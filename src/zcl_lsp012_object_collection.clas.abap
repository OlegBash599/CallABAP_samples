CLASS zcl_lsp012_object_collection DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

*"* public components of class ZCL_LSP012_OBJECT_COLLECTION
*"* do not include other source files here!!!
  PUBLIC SECTION.


    INTERFACES zif_lsp012_object_collection .

    ALIASES get
      FOR zif_lsp012_object_collection~get .
    ALIASES get_iterator
      FOR zif_lsp012_object_collection~get_iterator .
    ALIASES is_empty
      FOR zif_lsp012_object_collection~is_empty .
    ALIASES size
      FOR zif_lsp012_object_collection~size .

    ALIASES add FOR zif_lsp012_object_collection~add .
    ALIASES remove FOR zif_lsp012_object_collection~remove .
    ALIASES clear FOR zif_lsp012_object_collection~clear .

    TYPES:
      ty_collection TYPE STANDARD TABLE OF REF TO object .

    DATA collection TYPE ty_collection READ-ONLY .

*    METHODS add
*      IMPORTING
*        !element TYPE REF TO object .
*    METHODS remove
*      IMPORTING
*        !element TYPE REF TO object .
*    METHODS clear .
  PROTECTED SECTION.
*"* protected components of class ZCL_LSP012_OBJECT_COLLECTION
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_LSP012_OBJECT_COLLECTION
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_lsp012_object_collection IMPLEMENTATION.


  METHOD add .
    APPEND element TO collection.
  ENDMETHOD.


  METHOD clear .
    CLEAR collection.
  ENDMETHOD.


  METHOD zif_lsp012_object_collection~get.
    READ TABLE collection INDEX index INTO object.
  ENDMETHOD.

  METHOD zif_lsp012_object_collection~get_iterator .

    CREATE OBJECT iterator TYPE (iv_cls)
      EXPORTING
        collection = me.
  ENDMETHOD.

  METHOD zif_lsp012_object_collection~is_empty .
    DATA count TYPE i.
    DESCRIBE TABLE collection LINES count.
    IF count = 0.
      is_empty = 'X'.
    ELSE.
      is_empty = ' '.
    ENDIF.
  ENDMETHOD.


  METHOD zif_lsp012_object_collection~size .
    DESCRIBE TABLE collection LINES size.
  ENDMETHOD.


  METHOD remove .
    DATA obj TYPE REF TO object.
    LOOP AT collection INTO obj.
      IF obj = element.
        DELETE collection.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
