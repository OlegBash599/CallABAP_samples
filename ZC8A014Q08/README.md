# Reference in ABAP Debugger is not shown

Let we have a snippet like the following


```abap
    TYPES: tt_t000 TYPE STANDARD TABLE OF t000 WITH DEFAULT KEY.

    DATA lt_t000 TYPE tt_t000.

    DATA lr_tab_t000 TYPE REF TO tt_t000.

    FIELD-SYMBOLS <fs_tab_t000> TYPE tt_t000.
"""...................
"""...................
"""...................
    ASSIGN lt_t000 TO <fs_tab_t000>.

    lr_tab_t000 = REF #( lt_t000 ).
```

Full code-list is presented [here](https://github.com/OlegBash599/CallABAP_samples/blob/master/ZC8A014Q08/ZREP_C8A014Q08_DEBUGGER_REF.prog.abap)


if we will try to display references to the lt_t000 via special button in the debugger, we will get only information abaout field-symbols <fs_tab_t000>, but not about lr_tab_t000.

![Screen from Debugger](https://github.com/OlegBash599/CallABAP_samples/blob/master/ZC8A014Q08/pict1_no_ref.png)
