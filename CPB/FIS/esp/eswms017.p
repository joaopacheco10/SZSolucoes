/**************************************************************************
**   Programa: esp/eswms017.p
**   Data    : Junho/216
**   Autor   : Joao Pacheco
**   Objetivo: Integra‡Æo Cancelamento do Documento de Frete
**   Versao..: 
**************************************************************************/

{include/i-prgvrs.i eswms017 2.11.00.001}
{utp/ut-glob.i}

define param buffer nota-fiscal for nota-fiscal. 

def new global shared var g-integrator as log no-undo.

def temp-table ti_e_cancela_documento no-undo
    field ped_cd_empresa                as int
    field ped_cd_cliente                as int
    field ped_nu_pedido_origem          as char
    field ped_ds_motivo_cancelamento    as char
    field dt_addrow                     as date
    field id_processado                 as char.

def var h-api          as handle      no-undo.

/*
if not g-integrator then do:
    run tools/queuemanager.p ("embarque",cdd-embarqres-cli + "|" + p-acao).        
    return "ok".
end.*/

find ped-venda no-lock where
     ped-venda.nome-abrev = nota-fiscal.nome-abrev and
     ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli  no-error.
if avail ped-venda then do:

    create ti_e_cancela_documento.
    assign ti_e_cancela_documento.ped_cd_empresa             = if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1
           ti_e_cancela_documento.ped_cd_cliente             = ped-venda.cod-emitente
           ti_e_cancela_documento.ped_nu_pedido_origem       = ped-venda.nr-pedcli
           ti_e_cancela_documento.ped_ds_motivo_cancelamento = nota-fiscal.desc-cancela.

    if not valid-handle(h-api) then
        run esp/eswmapi998.p persistent set h-api.

    run pi_insert in h-api (temp-table ti_e_cancela_documento:default-buffer-handle).

    if return-value <> "ok" then
        return "nok".

    run pi_finalizar in h-api.

    delete procedure h-api.

end.

return "ok".
