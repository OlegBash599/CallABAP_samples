FUNCTION-POOL zlsp012_mediator.             "MESSAGE-ID ..



CLASS lcl_app DEFINITION LOAD.
CLASS lcl_app DEFINITION DEFERRED.
DATA go_app TYPE REF TO lcl_app.


" mediator - посредник
INCLUDE lzlsp012_mediatord01.              " " mediator - посредник
INCLUDE lzlsp012_mediatord07.              " " lcl_collegue_alv
INCLUDE lzlsp012_mediatord02.              " " lcl_colleague_consignment
INCLUDE lzlsp012_mediatord03.              " " lcl_colleague_total_stock
INCLUDE lzlsp012_mediatord04.              " " lcl_colleague_sales
INCLUDE lzlsp012_mediatord05.              " " lcl_colleague_log
INCLUDE lzlsp012_mediatord06.              " " lcl_mediator_director



INCLUDE lzlsp012_mediatord99.              " " lcl_app
