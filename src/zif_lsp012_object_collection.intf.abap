INTERFACE zif_lsp012_object_collection
  PUBLIC .

  METHODS size
    RETURNING
      VALUE(size) TYPE i .
  METHODS is_empty
    RETURNING
      VALUE(is_empty) TYPE flag .
  METHODS get
    IMPORTING
      !index        TYPE i
    RETURNING
      VALUE(object) TYPE REF TO object .
  METHODS get_iterator
    IMPORTING !iv_cls    TYPE seoclsname DEFAULT 'ZCL_OBJECT_COLLECTION_ITERATOR'
    RETURNING VALUE(iterator) TYPE REF TO if_object_collection_iterator .
  METHODS add
      IMPORTING
        !element TYPE REF TO object .
    METHODS remove
      IMPORTING
        !element TYPE REF TO object .
    METHODS clear .
ENDINTERFACE.
