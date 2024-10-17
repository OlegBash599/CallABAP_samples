*&---------------------------------------------------------------------*
*& Report ZREP_C8A014_DATETIME
*&---------------------------------------------------------------------*
*& RustABAP QuarzToolBox
*&---------------------------------------------------------------------*
REPORT zrep_c8a014_datetime.

include zrep_c8a014_datetime_scrn if FOUND.
include zrep_c8a014_datetime_itf01 if FOUND.
include zrep_c8a014_datetime_cls10 if FOUND. " lcl_sys_vars( ms_scr )->fn( ).
include zrep_c8a014_datetime_cls11 if FOUND. " lcl_date_time_conv( ms_scr )->fn( ).
include zrep_c8a014_datetime_cls12 if FOUND. " lcl_date_time_calc( ms_scr )->fn( ).
include zrep_c8a014_datetime_cls99 if FOUND.
include zrep_c8a014_datetime_evnts if FOUND.
