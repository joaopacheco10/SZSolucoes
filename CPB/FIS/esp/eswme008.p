/**************************************************************************
**   Programa: esp/eswme008.p
**   Data    : Junho/2016
**   Autor   : Joao Pacheco
**   Objetivo: Integracao WMS Recebimento Transportadora
**   Versao..: 
**************************************************************************/

{include/i-prgvrs.i eswme008.p 12.01.00.001}

def shared stream s-log.

def var h-api        as handle  no-undo.
def var l-alterou    as logical no-undo.

def buffer bped-venda for ped-venda.
def buffer bpre-fatur for pre-fatur.

def temp-table ti_s_frete_servico_prazo_troca no-undo
    field ped_cd_empresa        as int
    field ped_cd_cliente        as int
    field ped_nu_pedido_origem  as char
    field ped_cd_transportadora as int
    field ped_tipo_entrega      as int
    field ped_tipo_imposto      as char
    field ped_qt_dias_entrega   as int
    field ped_frete_valor       as dec
    field ped_nf_peso_real      as dec
    field ped_fator_cubagem     as int
    field dt_addrow             as date
    field id_processado         as char. 

run esp/eswmapi998.p persistent set h-api.

put stream s-log unformatted
            "- " string(time,"hh:mm:ss") 
            " - Processando interface --> ti_s_frete_servico_prazo_troca" skip.

run pi_select in h-api (temp-table ti_s_frete_servico_prazo_troca:default-buffer-handle).

for each ti_s_frete_servico_prazo_troca,
    first ped-venda no-lock where 
          ped-venda.cod-emitente = ti_s_frete_servico_prazo_troca.ped_cd_cliente and 
          ped-venda.nr-pedcli    = trim(ti_s_frete_servico_prazo_troca.ped_nu_pedido_origem):  

    put stream s-log unformatted
        "# " string(time,"hh:mm:ss") 
        " - Processando Transportadora --> " ti_s_frete_servico_prazo_troca.ped_cd_transportadora " - Pedido: " ped-venda.nr-pedcli skip.

    find first transporte no-lock where 
               transporte.cod-transp = ti_s_frete_servico_prazo_troca.ped_cd_transportadora no-error.

    assign l-alterou = no.
    for each pre-fatur use-index ch-pedido no-lock where 
             pre-fatur.nome-abrev  = ped-venda.nome-abrev and 
             pre-fatur.nr-pedcli   = ped-venda.nr-pedcli:

        if  pre-fatur.cod-sit-pre <> 1 then do:
            for each nota-fiscal exclusive-lock where 
                     nota-fiscal.nome-ab-cli  = ped-venda.nome-abrev and 
                     nota-fiscal.nr-pedcli    = ped-venda.nr-pedcli  and 
                     nota-fiscal.ind-sit-nota = 1                    and 
                     nota-fiscal.dt-cancel    = ?:
                assign nota-fiscal.nome-transp = transporte.nome-abrev when avail transporte
                       l-alterou               = yes.
            end.
        end.
        else do:
            assign l-alterou = yes.

            find first bped-venda exclusive-lock where 
                       rowid (bped-venda) = rowid(ped-venda) no-error.
            if avail bped-venda then
                assign bped-venda.nome-transp = transporte.nome-abrev when avail transporte.

            find first bpre-fatur exclusive-lock where 
                       rowid (bpre-fatur) = rowid (pre-fatur) no-error.
            if avail bpre-fatur then
                assign bpre-fatur.nome-transp = transporte.nome-abrev when avail transporte.
        end.
    end.
    
    run pi_update_status in h-api (temp-table ti_s_frete_servico_prazo_troca:default-buffer-handle).

    if l-alterou then
        put stream s-log unformatted
            "- " string(time,"hh:mm:ss") 
            " @ Processado com sucesso --> Transp: " ti_s_frete_servico_prazo_troca.ped_cd_transportadora " - Pedido: " ped-venda.nr-pedcli skip.
end.

run pi_finalizar in h-api.

delete procedure h-api.

return 'ok'.
