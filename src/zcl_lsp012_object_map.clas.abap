CLASS zcl_lsp012_object_map DEFINITION
  PUBLIC
  CREATE PUBLIC .

*"* public components of class ZCL_LSP012_OBJECT_MAP
*"* do not include other source files here!!!
  PUBLIC SECTION.

    INTERFACES zif_lsp012_object_map .

    ALIASES contains_key
      FOR zif_lsp012_object_map~contains_key .
    ALIASES contains_value
      FOR zif_lsp012_object_map~contains_value .
    ALIASES get
      FOR zif_lsp012_object_map~get .
    ALIASES get_by_position
      FOR zif_lsp012_object_map~get_by_position .
    ALIASES get_keys
      FOR zif_lsp012_object_map~get_keys .
    ALIASES get_position
      FOR zif_lsp012_object_map~get_position .
    ALIASES get_values
      FOR zif_lsp012_object_map~get_values .
    ALIASES get_values_iterator
      FOR zif_lsp012_object_map~get_values_iterator .
    ALIASES get_values_table
      FOR zif_lsp012_object_map~get_values_table .
    ALIASES is_empty
      FOR zif_lsp012_object_map~is_empty .
    ALIASES size
      FOR zif_lsp012_object_map~size .

    TYPES:
      BEGIN OF ty_map_element,
        key   TYPE string,
        value TYPE REF TO object,
      END OF ty_map_element .
    TYPES:
      ty_map TYPE STANDARD TABLE OF ty_map_element .
    TYPES:
      ty_sorted_map TYPE SORTED TABLE OF ty_map_element WITH UNIQUE KEY key .

    DATA map TYPE ty_map READ-ONLY .

    METHODS constructor
      IMPORTING
        !map TYPE ty_map OPTIONAL .
    METHODS put
      IMPORTING
        !key      TYPE any
        !position TYPE int4 OPTIONAL
        !value    TYPE REF TO object .
    METHODS remove
      IMPORTING
        !key TYPE any .
    METHODS clear .
    METHODS move
      IMPORTING
        !value              TYPE REF TO object
        VALUE(new_position) TYPE int4 .
    METHODS change_key
      IMPORTING
        VALUE(old_key) TYPE any
        VALUE(new_key) TYPE any .
  PROTECTED SECTION.
*"* protected components of class ZCL_LSP012_OBJECT_MAP
*"* do not include other source files here!!!

    METHODS get_key_of_object
      IMPORTING
        !value     TYPE REF TO object
      RETURNING
        VALUE(key) TYPE string .
  PRIVATE SECTION.
*"* private components of class ZCL_LSP012_OBJECT_MAP
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_lsp012_object_map IMPLEMENTATION.


  METHOD change_key.

    DATA internal_key TYPE string.

    FIELD-SYMBOLS: <obj> TYPE ty_map_element.

    internal_key = old_key.
    TRANSLATE internal_key TO UPPER CASE.
    READ TABLE me->map WITH KEY key = internal_key ASSIGNING <obj>.
    IF sy-subrc = 0.
*   check if there's already an entry with the new key, if yes, don't change the old key
      internal_key = new_key.
      TRANSLATE internal_key TO UPPER CASE.
      READ TABLE me->map WITH KEY key = internal_key TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        <obj>-key = internal_key.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD clear .
    CLEAR map.
  ENDMETHOD.


  METHOD constructor.
    me->map = map.
  ENDMETHOD.


  METHOD get_key_of_object .
    DATA map_elem TYPE ty_map_element.
    LOOP AT map INTO map_elem.
      IF map_elem-value = value.
        key = map_elem-key.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_lsp012_object_map~contains_key .
    DATA internal_key TYPE string.
    internal_key = key.
    TRANSLATE internal_key TO UPPER CASE.
    READ TABLE map WITH KEY key = internal_key TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      return = 'X'.
    ELSE.
      return = ' '.
    ENDIF.
  ENDMETHOD.


  METHOD zif_lsp012_object_map~contains_value .
    DATA map_elem TYPE ty_map_element.
    LOOP AT map INTO map_elem.
      IF map_elem-value = value.
        return = 'X'.
        RETURN.
      ENDIF.
    ENDLOOP.
    return = ' '.
  ENDMETHOD.


  METHOD zif_lsp012_object_map~get .
    DATA map_elem TYPE ty_map_element.
    DATA internal_key TYPE string.
    internal_key = key.
    TRANSLATE internal_key TO UPPER CASE.
    READ TABLE map WITH KEY key = internal_key INTO map_elem.
    IF sy-subrc = 0.
      value = map_elem-value.
    ENDIF.
  ENDMETHOD.


  METHOD zif_lsp012_object_map~get_by_position .
    DATA map_elem TYPE ty_map_element.
    READ TABLE map INTO map_elem INDEX position.
    IF sy-subrc = 0.
      value = map_elem-value.
    ENDIF.
  ENDMETHOD.


  METHOD zif_lsp012_object_map~get_keys .
    DATA map_elem TYPE ty_map_element.
    LOOP AT map INTO map_elem.
      APPEND map_elem-key TO keys.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_lsp012_object_map~get_position .
    DATA map_elem TYPE ty_map_element.
    LOOP AT map INTO map_elem.
      IF map_elem-value = value.
        position = sy-tabix.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_lsp012_object_map~get_values .
    "data object_collection type ref to cl_object_collection.
    DATA object_collection TYPE REF TO zif_lsp012_object_collection.
    CREATE OBJECT object_collection type (iv_cls).
    DATA map_elem TYPE ty_map_element.
    LOOP AT map INTO map_elem.
      object_collection->add( map_elem-value ).
    ENDLOOP.
    values = object_collection.
  ENDMETHOD.


  METHOD zif_lsp012_object_map~get_values_iterator .
    DATA collection TYPE REF TO zif_lsp012_object_collection.
    collection = get_values( ).
    value_iterator = collection->get_iterator( iv_cls ).
  ENDMETHOD.


  METHOD zif_lsp012_object_map~get_values_table .
    DATA map_elem TYPE ty_map_element.
    LOOP AT map INTO map_elem.
      APPEND map_elem-value TO values.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_lsp012_object_map~is_empty .
    DATA count TYPE i.
    DESCRIBE TABLE map LINES count.
    IF count = 0.
      is_empty = 'X'.
    ELSE.
      is_empty = ' '.
    ENDIF.
  ENDMETHOD.


  METHOD zif_lsp012_object_map~size .
    DESCRIBE TABLE map LINES size.
  ENDMETHOD.


  METHOD move .

    DATA old_position TYPE int4.
    old_position = get_position( value ).

    DATA key TYPE string.
    key = get_key_of_object( value ).

    IF old_position > new_position.
      remove( key ).
      put( key = key   value = value position = new_position ).
    ELSEIF old_position < new_position.
      remove( key ).
      DATA insert_position TYPE int4.
      insert_position = new_position - 1.
      put( key = key value = value position = new_position ).
    ENDIF.
  ENDMETHOD.


  METHOD put .
    DATA map_elem TYPE ty_map_element.
    DATA internal_key TYPE string.
    internal_key = key.
    TRANSLATE internal_key TO UPPER CASE.

    READ TABLE map WITH KEY key = key TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      map_elem-key = internal_key.
      map_elem-value = value.
      IF position IS INITIAL.
        APPEND map_elem TO map.
      ELSE.
        INSERT map_elem INTO map INDEX position.
        IF sy-subrc <> 0.
          APPEND map_elem TO map.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD remove .
    DATA internal_key TYPE string.
    internal_key = key.
    TRANSLATE internal_key TO UPPER CASE.
    DELETE map WHERE key = internal_key.
  ENDMETHOD.
ENDCLASS.
