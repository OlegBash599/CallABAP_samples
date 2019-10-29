FUNCTION z_lsp012_get_rates.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRYNUM) TYPE  SYINDEX
*"  EXPORTING
*"     VALUE(ET_RATES) TYPE  ZTLSP012_RATES_TAB
*"     VALUE(EV_CHANGED) TYPE  CHAR1
*"----------------------------------------------------------------------
  DATA(lo_number) = NEW lcl_number_oper( ).

  IF iv_trynum EQ 1
      OR ( ( iv_trynum MOD 3 ) EQ 0 ).
    ev_changed = abap_true.
    et_rates = VALUE #(
    FOR tab_line = 0 THEN tab_line + 1 UNTIL tab_line > 10
    ( mandt = cl_abap_syst=>get_client( )
      partner = ( tab_line * tab_line + 2 )
      partner_type = COND char2( WHEN ( tab_line MOD 2 ) EQ 0 THEN '02'
                                    ELSE '03' )

      rating_pay_euro = lo_number->get_any_num0_1000( )
      rating_pay_ru  = lo_number->get_any_num0_1000( )
      rating_pay_usa = lo_number->get_any_num0_1000( )
      rating_deliver_euro = lo_number->get_any_num0_1000( )
      rating_deliver_ru = lo_number->get_any_num0_1000( )
      rating_deliver_usa = lo_number->get_any_num0_1000( )
      rating_hr_euro  = lo_number->get_any_num0_1000( )
      rating_hr_ru  = lo_number->get_any_num0_1000( )
      rating_hr_usa  = lo_number->get_any_num0_1000( )
      crdate = sy-datum crtime = sy-uzeit  )
      ).

  ELSE.
    CLEAR ev_changed.
    RETURN.
  ENDIF.

ENDFUNCTION.
