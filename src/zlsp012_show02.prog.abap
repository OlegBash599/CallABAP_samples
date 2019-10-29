*&---------------------------------------------------------------------*
*& Report ZLSP012_SHOW02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zlsp012_show02 MESSAGE-ID zlsp010_msg.

INCLUDE zlsp012_show02_data.

INCLUDE zlsp012_show02_intf1. " interface
INCLUDE zlsp012_show02_cls01. " html output
INCLUDE zlsp012_show02_cls02. " app


INCLUDE zlsp012_show02_scr.
INCLUDE zlsp012_show02_evnt.
