*&---------------------------------------------------------------------*
*& Report ZLSP012_SHOW04
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zlsp012_show04 MESSAGE-ID zlsp012_msg.

INCLUDE zlsp012_show04_data IF FOUND.

INCLUDE zlsp012_show04_intf1 IF FOUND.

INCLUDE zlsp012_show04_cls00 IF FOUND. " html class

INCLUDE zlsp012_show04_cls01 IF FOUND. " singleton sample
INCLUDE zlsp012_show04_cls02 IF FOUND. " factory method
INCLUDE zlsp012_show04_cls03 IF FOUND. " abstract factory
INCLUDE zlsp012_show04_cls04 IF FOUND. " builder
INCLUDE zlsp012_show04_cls05 IF FOUND. " prototype

INCLUDE zlsp012_show04_cls06 IF FOUND. " adapter
INCLUDE zlsp012_show04_cls07 IF FOUND. " bridge
INCLUDE zlsp012_show04_cls08 IF FOUND. " composite
INCLUDE zlsp012_show04_cls09 IF FOUND. " decorator
INCLUDE zlsp012_show04_cls10 IF FOUND. " facade
INCLUDE zlsp012_show04_cls11 IF FOUND. " flyweight
INCLUDE zlsp012_show04_cls12 IF FOUND. " proxy


INCLUDE zlsp012_show04_cls13 IF FOUND. " chain of responsibility
INCLUDE zlsp012_show04_cls14 IF FOUND. " command
INCLUDE zlsp012_show04_cls15 IF FOUND. " iterator
INCLUDE zlsp012_show04_cls16 IF FOUND. " mediator
INCLUDE zlsp012_show04_cls17 IF FOUND. " memento
INCLUDE zlsp012_show04_cls18 IF FOUND. " observer
INCLUDE zlsp012_show04_cls19 IF FOUND. " state
INCLUDE zlsp012_show04_cls20 IF FOUND. " strategy
INCLUDE zlsp012_show04_cls21 IF FOUND. " template method
INCLUDE zlsp012_show04_cls22 IF FOUND. " visitor

INCLUDE zlsp012_show04_cls99 IF FOUND. " lcl_app


INCLUDE zlsp012_show04_scr IF FOUND. " initial screen
INCLUDE zlsp012_show04_evnt IF FOUND.
INCLUDE zlsp012_show04_mods IF FOUND.
