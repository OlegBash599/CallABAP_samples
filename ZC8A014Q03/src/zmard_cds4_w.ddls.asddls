@AbapCatalog.sqlViewName: 'ZQMARD_CDS4'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'calculation'
define view ZMARD_CDS4_W as select from ZMARD_CDS1_M
{
    key matnr,
    key werks,
    key lgort,
    sum(labst) as stock
} group by matnr, werks, lgort
