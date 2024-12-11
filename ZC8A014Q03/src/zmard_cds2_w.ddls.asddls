@AbapCatalog.sqlViewName: 'ZQMARD_CDS2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'as wain'
define view ZMARD_CDS2_W as select from ZMARD_CDS1_M
{
    key matnr,
    sum(labst) as stock
} group by matnr
