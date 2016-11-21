/**************************************************************************
**   Programa: esp/eswms018.p
**   Data    : Junho/216
**   Autor   : Joao Pacheco
**   Objetivo: Integra‡Æo Confirma‡Æo dos Pagamentos
**   Versao..: 
**************************************************************************/

{include/i-prgvrs.i eswms018 2.11.00.001}
{utp/ut-glob.i}

define param buffer tit_ap for tit_ap. 

def new global shared var g-integrator as log no-undo.

def temp-table ti_e_confirmacao_pagamento no-undo
    field pag_cd_empresa        as int
    field pag_cd_cnpj_transp    as char
    field pag_nu_cte            as int
    field pag_nu_cte_serie      as char
    field pag_nu_titulo         as char
    field pag_vlr_pago          as dec
    field pag_dt_pagamento      as date
    field dt_addrow             as date
    field id_processado         as char.

def var h-api          as handle      no-undo.

if not g-integrator then do:
    run tools/queuemanager.p ("ConfPagto",tit_ap.cod_estab + "|" + string(tit_ap.num_id_tit_ap)).
    return "ok".
end.


if avail tit_ap then do:

    find emitente no-lock where
         emitente.cod-emitente = tit_ap.cdn_fornecedor no-error.         

    find esp-fatura-frete no-lock where
         esp-fatura-frete.nome-transp = emitente.nome-abrev  and
         esp-fatura-frete.ser-docto   = tit_ap.cod_ser_docto and
         esp-fatura-frete.nr-fatura   = tit_ap.cod_tit_ap    no-error.
    
    find first esp-cte-fatura no-lock where
               esp-cte-fatura.nome-transp = esp-fatura-frete.nome-transp and
               esp-cte-fatura.ser-docto   = esp-fatura-frete.ser-docto   and
               esp-cte-fatura.nr-fatura   = esp-fatura-frete.nr-fatura   no-error.
    if avail esp-cte-fatura then do:           
    
        create ti_e_confirmacao_pagamento.
        assign ti_e_confirmacao_pagamento.pag_cd_empresa     = if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1
               ti_e_confirmacao_pagamento.pag_cd_cnpj_transp = emitente.cgc
               ti_e_confirmacao_pagamento.pag_nu_cte         = int(esp-cte-fatura.nr-cte)
               ti_e_confirmacao_pagamento.pag_nu_cte_serie   = esp-cte-fatura.ser-cte
               ti_e_confirmacao_pagamento.pag_nu_titulo      = tit_ap.cod_tit_ap
               ti_e_confirmacao_pagamento.pag_dt_pagamento   = tit_ap.dat_liquidac_tit_ap.
    
        for each movto_tit_ap no-lock where
                 movto_tit_ap.cod_estab     = tit_ap.cod_estab     and
                 movto_tit_ap.num_id_tit_ap = tit_ap.num_id_tit_ap and
                 movto_tit_ap.ind_trans_ap  = 'baixa':
            assign ti_e_confirmacao_pagamento.pag_vlr_pago = ti_e_confirmacao_pagamento.pag_vlr_pago + movto_tit_ap.val_movto_ap.
        end.
    
        if not valid-handle(h-api) then
            run esp/eswmapi998.p persistent set h-api.
    
        run pi_insert in h-api (temp-table ti_e_confirmacao_pagamento:default-buffer-handle).
    
        if return-value <> "ok" then
            return "nok".
    
        run pi_finalizar in h-api.
    
        delete procedure h-api.
    end.
end.

return "ok".
