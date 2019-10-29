*&---------------------------------------------------------------------*
*& Report ZLSP012_SHOW01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zlsp012_show01 MESSAGE-ID zlsp012_msg.

INCLUDE zlsp012_show01_data IF FOUND.
INCLUDE zlsp012_show01_scr IF FOUND.
INCLUDE zlsp012_show01_cls01 IF FOUND.
INCLUDE zlsp012_show01_cls02 IF FOUND.
INCLUDE zlsp012_show01_cls03 IF FOUND.
INCLUDE zlsp012_show01_cls03_fib IF FOUND.


INCLUDE zlsp012_show01_evnt IF FOUND.
