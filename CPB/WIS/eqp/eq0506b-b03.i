/**********************************************************************/
/***                                                                ***/
/*** DEFINI€ÇO DA TT-IT-PRE-FAT                                     ***/
/***                                                                ***/
/**********************************************************************/

def temp-table tt-it-pre-fat{1} NO-UNDO 
    FIELD cdd-embarq   LIKE it-pre-fat.cdd-embarq
    field nr-resumo    like it-pre-fat.nr-resumo
    field nome-abrev   like it-pre-fat.nome-abrev
    field nr-pedcli    like it-pre-fat.nr-pedcli
    field nr-sequencia like it-pre-fat.nr-sequencia
    field it-codigo    like it-pre-fat.it-codigo
    field cod-refer    like ped-ent.cod-refer
    field qt-alocada   like it-pre-fat.qt-alocada
    field dt-entrega   like it-pre-fat.dt-entrega
    field nr-entrega   like it-pre-fat.nr-entrega
    field cor          as int
    field hr-entrega   like ped-ent.hr-entrega
    &if  defined(bf_comex206b-1) &then
         field nr-proc-exp  like ped-ent.nr-proc-exp
    &endif
    field nat-operacao like it-pre-fat.nat-operacao

    index ch-ent is primary unique
        nome-abrev
        nr-pedcli
        nr-sequencia
        it-codigo
        nr-entrega
        nr-resumo.
