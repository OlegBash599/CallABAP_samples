*&---------------------------------------------------------------------*
*& Report ZREP_C8A014Q08_DEBUGGER_REF
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrep_c8a014q08_debugger_ref.

PARAMETERS: p_dummy TYPE char1 DEFAULT 'Q'.

CLASS lcl_app DEFINITION.

  PUBLIC SECTION.

    METHODS main
      RETURNING VALUE(rc) TYPE sysubrc.

  PROTECTED SECTION.

  PRIVATE SECTION.


ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD main.
    "RETURNING VALUE(rc) TYPE sysubrc.
    TYPES: tt_t000 TYPE STANDARD TABLE OF t000 WITH DEFAULT KEY.

    DATA lt_t000 TYPE tt_t000.

    DATA lr_tab_t000 TYPE REF TO tt_t000.

    FIELD-SYMBOLS <fs_tab_t000> TYPE tt_t000.


    SELECT
        mandt mtext ort01
        FROM t000
      INTO CORRESPONDING FIELDS OF TABLE lt_t000
      UP TO 1000 ROWS.

    ASSIGN lt_t000 TO <fs_tab_t000>.

    lr_tab_t000 = REF #( lt_t000 ).

    BREAK-POINT.
    " in the Debugger:
    " = GoTO to Standard
    " == SubScreen with Variables
    " == put var lt_t000 and select the line and click References

    " == in my version only reference for field symbol is displayed ( <fs_tab_t000> )
    " == and reference lr_tab_t000 is not shown

  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.
  NEW lcl_app( )->main( ).

end-of-SELECTION.
