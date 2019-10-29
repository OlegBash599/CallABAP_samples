INTERFACE zif_lsp012_object_map
  PUBLIC .


  METHODS get
    IMPORTING
      !key         TYPE any
    RETURNING
      VALUE(value) TYPE REF TO object .
  METHODS contains_key
    IMPORTING
      !key          TYPE any
    RETURNING
      VALUE(return) TYPE flag .
  METHODS contains_value
    IMPORTING
      !value        TYPE REF TO object
    RETURNING
      VALUE(return) TYPE flag .
  METHODS is_empty
    RETURNING
      VALUE(is_empty) TYPE flag .
  METHODS size
    RETURNING
      VALUE(size) TYPE i .
  METHODS get_values
    IMPORTING
      !iv_cls       TYPE seoclsname DEFAULT 'ZCL_LSP012_OBJECT_COLLECTION'
    RETURNING
      VALUE(values) TYPE REF TO zif_lsp012_object_collection .
  METHODS get_values_iterator
   IMPORTING !iv_cls TYPE seoclsname DEFAULT 'ZCL_OBJECT_COLLECTION_ITERATOR'
    RETURNING
      VALUE(value_iterator) TYPE REF TO if_object_collection_iterator .
  METHODS get_keys
    RETURNING
      VALUE(keys) TYPE string_table .
  METHODS get_by_position
    IMPORTING
      VALUE(position) TYPE int4
    RETURNING
      VALUE(value)    TYPE REF TO object .
  METHODS get_position
    IMPORTING
      !value          TYPE REF TO object
    RETURNING
      VALUE(position) TYPE int4 .
  METHODS get_values_table
    RETURNING
      VALUE(values) TYPE wdy_object_map_element_table .
ENDINTERFACE.
