CLASS zcl_object_collection_iterator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

*"* public components of class ZCL_OBJECT_COLLECTION_ITERATOR
*"* do not include other source files here!!!
  PUBLIC SECTION.

    INTERFACES if_object_collection_iterator .

    ALIASES get_index
      FOR if_object_collection_iterator~get_index .
    ALIASES get_next
      FOR if_object_collection_iterator~get_next .
    ALIASES has_next
      FOR if_object_collection_iterator~has_next .

    METHODS constructor
      IMPORTING
        !collection TYPE REF TO zif_lsp012_object_collection .
  PROTECTED SECTION.
*"* protected components of class ZCL_OBJECT_COLLECTION_ITERATOR
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_OBJECT_COLLECTION_ITERATOR
*"* do not include other source files here!!!

    DATA collection TYPE REF TO zif_lsp012_object_collection .
    DATA index TYPE i VALUE 0 .
ENDCLASS.



CLASS zcl_object_collection_iterator IMPLEMENTATION.


  METHOD constructor .
    me->collection = collection.
  ENDMETHOD.


  METHOD if_object_collection_iterator~get_index .
    index = me->index.
  ENDMETHOD.


  METHOD if_object_collection_iterator~get_next .
    DATA obj TYPE REF TO object.
    index = index + 1.
    object = collection->get( index ).
  ENDMETHOD.


  METHOD if_object_collection_iterator~has_next .
    DATA obj TYPE REF TO object.
    DATA idx TYPE i.
    idx = index + 1.
    obj = collection->get( idx ).
    IF obj IS INITIAL.
      has_next = ' '.
    ELSE.
      has_next = 'X'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
