class ZCL_OBJECT_COLLECTION_ITER135 definition
  public
  final
  create public .

*"* public components of class ZCL_OBJECT_COLLECTION_ITER135
*"* do not include other source files here!!!
public section.

  interfaces IF_OBJECT_COLLECTION_ITERATOR .

  aliases GET_INDEX
    for IF_OBJECT_COLLECTION_ITERATOR~GET_INDEX .
  aliases GET_NEXT
    for IF_OBJECT_COLLECTION_ITERATOR~GET_NEXT .
  aliases HAS_NEXT
    for IF_OBJECT_COLLECTION_ITERATOR~HAS_NEXT .

  methods CONSTRUCTOR
    importing
      !COLLECTION type ref to ZIF_LSP012_OBJECT_COLLECTION .
  PROTECTED SECTION.
*"* protected components of class ZCL_OBJECT_COLLECTION_ITER135
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_OBJECT_COLLECTION_ITER135
*"* do not include other source files here!!!

    DATA collection TYPE REF TO zif_lsp012_object_collection .
    DATA index TYPE i VALUE 0 .
ENDCLASS.



CLASS ZCL_OBJECT_COLLECTION_ITER135 IMPLEMENTATION.


  METHOD CONSTRUCTOR .
" also is here sample
" https://github.com/OlegBash599/AnyTabUpdateTask/blob/main/src/zcl_c8a005_group_tab.clas.abap
    me->collection = collection.
  ENDMETHOD.


  METHOD IF_OBJECT_COLLECTION_ITERATOR~GET_INDEX .
    index = me->index.
  ENDMETHOD.


  METHOD IF_OBJECT_COLLECTION_ITERATOR~GET_NEXT .
    DATA obj TYPE REF TO object.
    index = index + 2.
    object = collection->get( index ).
  ENDMETHOD.


  METHOD IF_OBJECT_COLLECTION_ITERATOR~HAS_NEXT .
    DATA obj TYPE REF TO object.
    DATA idx TYPE i.
"    idx = index + 1.
    idx = index + 2.
    obj = collection->get( idx ).
    IF obj IS INITIAL.
      has_next = abap_false.
    ELSE.
      has_next = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
