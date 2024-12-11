@AbapCatalog.sqlViewName: 'ZQMARD_CDS3'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS wain'
define view ZMARD_CDS3_W as select from ZMARD_CDS1_M
{
    key matnr,
    key lgort,
    sum(labst) as stock
} group by matnr, lgort
