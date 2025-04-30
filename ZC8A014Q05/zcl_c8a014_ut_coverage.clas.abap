CLASS zcl_c8a014_ut_coverage DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS compare_status
      IMPORTING !iv_status_in TYPE string
      EXPORTING !ev_res       TYPE sysubrc.

    METHODS coverage_for_if_no_else
      IMPORTING !iv_status_in TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.


ENDCLASS.



CLASS ZCL_C8A014_UT_COVERAGE IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_C8A014_UT_COVERAGE->COMPARE_STATUS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_STATUS_IN                   TYPE        STRING
* | [<---] EV_RES                         TYPE        SYSUBRC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD compare_status.
*      IMPORTING !iv_status_in TYPE string
*      EXPORTING !ev_res       TYPE sysubrc.

    IF iv_status_in EQ 1.
      ev_res = 10.
    ELSEIF iv_status_in EQ 2.
      ev_res = 20.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_C8A014_UT_COVERAGE->COVERAGE_FOR_IF_NO_ELSE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_STATUS_IN                   TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD coverage_for_if_no_else.
    "IMPORTING !iv_status_in TYPE string.

    DATA lv_status_in TYPE string VALUE '10'.

    IF iv_status_in EQ lv_status_in.
      MESSAGE s000(cl) INTO sy-msgli.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
