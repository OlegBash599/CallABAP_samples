@AbapCatalog.sqlViewName: 'ZQMARD_CDS1'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'as main'
define view ZMARD_CDS1_M as select from zmard
{
    key matnr,
    key werks,
    key lgort,
    labst
}
