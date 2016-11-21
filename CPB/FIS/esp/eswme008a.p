/**************************************************************************
**   Programa: esp/eswme008a.p
**   Data    : Outubro/2016
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

def temp-table ti_s_frete_servico_prazo no-undo
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
            " - Processando interface --> ti_s_frete_servico_prazo" skip.

run pi_select in h-api (temp-table ti_s_frete_servico_prazo:default-buffer-handle).

for each ti_s_frete_servico_prazo,
    first ped-venda no-lock where 
          ped-venda.cod-emitente = ti_s_frete_servico_prazo.ped_cd_cliente and 
          ped-venda.nr-pedcli    = trim(ti_s_frete_servico_prazo.ped_nu_pedido_origem):  

    put stream s-log unformatted
        "# " string(time,"hh:mm:ss") 
        " - Processando Transportadora --> " ti_s_frete_servico_prazo.ped_cd_transportadora " - Pedido: " ped-venda.nr-pedcli skip.

    find first transporte no-lock where 
               transporte.cod-transp = ti_s_frete_servico_prazo.ped_cd_transportadora no-error.

    for each pre-fatur use-index ch-pedido no-lock where 
             pre-fatur.nome-abrev  = ped-venda.nome-abrev and 
             pre-fatur.nr-pedcli   = ped-venda.nr-pedcli:

        for each it-pre-fat of pre-fatur exclusive-lock:
            assign it-pre-fat.int-2 = 9.
        end.

        find first esp-resumo-wms exclusive-lock where
                   esp-resumo-wms.nr-embarque = int(pre-fat.cdd-embarq) and
                   esp-resumo-wms.nr-resumo   = it-pre-fat.nr-resumo    no-error.
        if avail esp-resumo-wms then
            assign esp-resumo-wms.conf-fis = yes.

    end.
    
    run pi_update_status in h-api (temp-table ti_s_frete_servico_prazo:default-buffer-handle).
    
end.

run pi_finalizar in h-api.

delete procedure h-api.

return 'ok'.
