/**************************************************************************
**   Programa: esp/eswms007a.p
**   Data    : maio de 2015
**   Autor   : eder
**   Pbjetivo: integra‡Æo wms embarques/nr nota fiscal
**   Versao..: 
**************************************************************************/

/*{include/i-bfems2.i}*/
{include/i-prgvrs.i eswms007a 2.11.00.001}
{utp/ut-glob.i}

define param buffer nota-fiscal for nota-fiscal. 

def buffer bfnf for nota-fiscal.

def temp-table int_e_cab_pedido_saida no-undo
    fields nu_pedido_origem     as char format "x(20)"
    fields cd_carga             as integer
    fields cd_cliente           as integer
    fields cd_empresa           as integer
    fields cd_deposito          as char format "x(3)"
    fields cd_situacao          as integer
    fields ds_cliente           as char format "x(100)"
    fields ds_cliente_entrega   as char format "x(100)"
    fields cd_transportadora    as integer
    fields ds_municipio_entrega as char format "x(72)"
    fields cd_uf_entrega        as char format "x(2)" 
    fields cd_cep_entrega       as char format "x(10)" 
    fields dt_addrow            as date.

def temp-table int_e_det_pedido_saida no-undo
    fields nu_pedido_origem as char format "x(20)"
    fields cd_cliente       as integer
    fields cd_empresa       as integer
    fields cd_deposito      as char format "x(3)"
    fields cd_situacao      as integer
    fields cd_produto       as char format "x(40)"
    fields nu_item_corp     as integer
    fields qt_separar       as decimal decimals 3
    fields nu_lote          as character
    fields nu_nota          as character
    fields nu_serie_nota    as character
    fields dt_addrow        as date.

def temp-table ti_e_documento_frete_nf no-undo
    field ped_cd_empresa        as int
    field ped_cd_cliente        as int
    field ped_nu_pedido_origem  as char
    field ped_nu_nf             as int
    field ped_nu_serie_nf       as char
    field ped_dt_emissao_nf     as date
    field ped_chave_acesso      as char
    field ped_qt_volume         as int
    field ped_cfop              as char
    field dt_addrow             as date /*n*/
    field id_processado         as char /*n*/.

def var i-cod-emitente as integer     no-undo.
def var c-not-fiscal   as character   no-undo.
def var c-nota-aux     as character   no-undo.
def var i-resumo       as integer     no-undo.
def var h-api-wis      as handle      no-undo.
def var h-api-fis      as handle      no-undo.


run esp/eswmapi999.p persistent set h-api-wis.
run esp/eswmapi998.p persistent set h-api-fis.

assign i-resumo = -1.

for each it-nota-fisc of nota-fiscal no-lock,
    each fat-ser-lote of it-nota-fisc no-lock
    on error undo , return "nok":
    
    assign i-cod-emitente = nota-fiscal.cod-emitente
           c-not-fiscal   = nota-fiscal.nr-nota-fis .
           c-nota-aux     = string(int(nota-fiscal.nr-nota-fis) + 1,"9999999").

    find first bfnf no-lock where 
               bfnf.cdd-embarq  = nota-fiscal.cdd-embarq and 
               bfnf.nome-abrev  = nota-fiscal.nome-abrev and 
               bfnf.nr-nota-fis = c-nota-aux             and 
               bfnf.nr-pedcli   = '' no-error.
    if avail bfnf then       
       assign i-cod-emitente = bfnf.cod-emitente 
              c-not-fiscal   = bfnf.nr-nota-fis.

    for first embarque no-lock
        where embarque.cdd-embarq = nota-fiscal.cdd-embarq:
    end.

    for first emitente no-lock
        where emitente.cod-emitente = i-cod-emitente:
    end.

    for first transporte no-lock
        where transporte.nome-abrev = nota-fiscal.nome-transp:
    end.

    create int_e_det_pedido_saida.
    assign int_e_det_pedido_saida.nu_pedido_origem = it-nota-fisc.nr-pedcli
           int_e_det_pedido_saida.cd_cliente       = nota-fiscal.cod-emitente
           int_e_det_pedido_saida.cd_empresa       = if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1
           int_e_det_pedido_saida.cd_deposito      = nota-fiscal.cod-estabel
           int_e_det_pedido_saida.cd_situacao      = 3 /** para nota fiscal **/
           int_e_det_pedido_saida.cd_produto       = it-nota-fisc.it-codigo 
           int_e_det_pedido_saida.nu_item_corp     = it-nota-fisc.nr-seq-ped
           int_e_det_pedido_saida.qt_separar       = fat-ser-lote.qt-baixada[1]
           int_e_det_pedido_saida.nu_lote          = fat-ser-lote.nr-serlote
           int_e_det_pedido_saida.nu_nota          = nota-fiscal.nr-nota-fis
           int_e_det_pedido_saida.nu_serie_nota    = nota-fiscal.serie
           int_e_det_pedido_saida.dt_addrow        = today.

    run pi_insert in h-api-wis (temp-table int_e_det_pedido_saida:default-buffer-handle).

    if return-value <> "ok" then
        return "nok".

    put unformat 
        skip 'notalogret>> ' return-value skip .

    if i-resumo <> nota-fiscal.nr-resumo then do:
            
        create int_e_cab_pedido_saida.
        assign int_e_cab_pedido_saida.nu_pedido_origem      = it-nota-fisc.nr-pedcli
               int_e_cab_pedido_saida.cd_carga              = int(string(nota-fiscal.cdd-embarq,"9999999") + string(nota-fiscal.nr-resumo,"999"))
               int_e_cab_pedido_saida.cd_cliente            = nota-fiscal.cod-emitente
               int_e_cab_pedido_saida.cd_empresa            = if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1
               int_e_cab_pedido_saida.cd_deposito           = nota-fiscal.cod-estabel
               int_e_cab_pedido_saida.cd_situacao           = 3 /** para nota fiscal **/
               int_e_cab_pedido_saida.ds_cliente            = emitente.nome-emit
               int_e_cab_pedido_saida.ds_cliente_entrega    = emitente.nome-emit
               int_e_cab_pedido_saida.cd_transportador      = if avail transporte then transporte.cod-transp else 0
               int_e_cab_pedido_saida.ds_municipio_entrega  = nota-fiscal.cidade
               int_e_cab_pedido_saida.cd_uf_entrega         = nota-fiscal.estado
               int_e_cab_pedido_saida.cd_cep_entrega        = trim(emitente.cep)
               int_e_cab_pedido_saida.dt_addrow             = today.

        put unformat 
            'notalog>> ' nota-fiscal.nr-pedcli ' - ' nota-fiscal.cdd-embarq skip.
    
        run pi_insert in h-api-wis (temp-table int_e_cab_pedido_saida:default-buffer-handle).

        put unformat 
            'notalogret>> ' return-value skip.

        assign i-resumo = nota-fiscal.nr-resumo.

        find ped-venda no-lock where 
             ped-venda.nome-abrev = nota-fiscal.nome-ab-cli and 
             ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli   no-error.
        if avail ped-venda then do:
            create ti_e_documento_frete_nf.
            assign ti_e_documento_frete_nf.ped_cd_empresa       = 1001 /*if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1*/
                   ti_e_documento_frete_nf.ped_cd_cliente       = ped-venda.cod-emitente
                   ti_e_documento_frete_nf.ped_nu_pedido_origem = ped-venda.nr-pedcli
                   ti_e_documento_frete_nf.ped_nu_nf            = int(c-not-fiscal)
                   ti_e_documento_frete_nf.ped_nu_serie_nf      = nota-fiscal.serie
                   ti_e_documento_frete_nf.ped_dt_emissao_nf    = nota-fiscal.dt-emis
                   ti_e_documento_frete_nf.ped_chave_acesso     = substring(nota-fiscal.cod-chave-aces-nf-eletro,1,44)
                   ti_e_documento_frete_nf.dt_addrow            = today.
    
            find natur-oper no-lock where
                 natur-oper.nat-operacao = nota-fiscal.nat-operacao no-error.
            if avail natur-oper then
                assign ti_e_documento_frete_nf.ped_cfop = natur-oper.cod-cfop.
    
            for each res-emb no-lock
               where res-emb.cdd-embarq = nota-fiscal.cdd-embarq
                 and res-emb.nr-resumo  = nota-fiscal.nr-resumo:
    
                assign ti_e_documento_frete_nf.ped_qt_volume = ti_e_documento_frete_nf.ped_qt_volume + res-emb.qt-volumes.
            end.
        end.

        run pi_insert in h-api-fis (temp-table ti_e_documento_frete_nf:default-buffer-handle).

        put unformat 'FreteLogRet>> ' return-value skip .
        
        if return-value <> "OK" then
            return "NOK".
    
    end.
end.

run pi_finalizar in h-api-wis.
run pi_finalizar in h-api-fis.


delete procedure h-api-wis.
delete procedure h-api-fis.


return "ok".

