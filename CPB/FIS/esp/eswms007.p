/**************************************************************************
**   programa: esp/eswms007.p
**   data    : maio 2015
**   autor   : eder
**   objetivo: integraá∆o wms embarques
**   versao..: 
**************************************************************************/

def buffer prog_dtsul   for emsfnd.prog_dtsul.
def buffer usuar_mestre for emsfnd.usuar_mestre.
def buffer b-transporte for transporte.

{include/i-prgvrs.i eswms007.p 2.11.00.001}
{utp/ut-glob.i}

def input param cdd-embarqres-cli as char no-undo.
def input param p-acao            as char no-undo.

def new global shared var g-integrator as log no-undo.

def var c-rua      as character   no-undo.
def var c-nro      as character   no-undo.
def var c-comp     as character   no-undo.
def var h-cdapi704 as handle      no-undo.
def var l-item     as log         no-undo.    
def var l-ult      as log         no-undo.
def var h-api-wis  as handle      no-undo.
def var h-api-fis  as handle      no-undo.
def var c-cgc-ped  as char        no-undo.

if not g-integrator then do:
    run tools/queuemanager.p ("embarque",cdd-embarqres-cli + "|" + p-acao).        
    return "ok".
end.

function fn_only_numbers returns character
    (input cvaluestring as character):

    define variable ipos     as integer   no-undo.
    define variable ilength  as integer   no-undo.
    define variable cdata    as character no-undo.
    define variable cdatanew as character no-undo.


    assign ilength = length(cvaluestring).

    do ipos = 1 to ilength:
        assign cdata = substring(cvaluestring,ipos,1).
        
        if cdata < '0' or cdata > '9' then
            next.

        assign cdatanew = cdatanew + cdata.
    end.

    return cdatanew.
end function.

def temp-table int_e_cab_pedido_saida no-undo
    fields nu_pedido_origem            as char format "x(20)"
    fields cd_carga             as integer
    fields cd_cliente           as integer
    fields cd_empresa           as integer
    fields cd_deposito          as char format "x(3)"
    fields dt_entrega           as date
    fields cd_situacao          as integer
    fields tp_pedido            as char format "x"
    fields ds_cliente_entrega         as char format "x(100)"
    fields ds_endereco_entrega  as char format "x(100)"
    fields nu_seq_entrega       as integer
    fields cd_transportadora    as integer
    fields cd_cnpj_transportadora as dec
    fields nu_doc_erp         as char format "x(30)"
    fields ds_municipio_entrega as char format "x(72)"
    fields ds_bairro_entrega    as char format "x(72)"
    fields cd_uf_entrega        as char format "x(2)" 
    fields cd_cep_entrega       as char format "x(10)" 
    fields nu_endereco_entrega  as char format "x(10)"
    fields ds_observacao        as char format "x(500)".

def temp-table int_e_det_pedido_saida no-undo
    fields nu_pedido_origem       as char format "x(20)"
    fields cd_cliente             as integer
    fields cd_empresa             as integer
    fields cd_deposito            as char format "x(3)"
    fields cd_produto             as char format "x(40)"
    fields nu_item_corp           as integer
    fields qt_separar        as decimal decimals 3
    fields nu_lote                as character
    fields cd_situacao         as integer format ">>9" 
    fields dt_addrow              as date.


/*projeto fis*/
def temp-table ti_e_documento_frete no-undo
    field ped_cd_empresa                as int
    field ped_cd_cliente                as int
    field ped_nu_pedido_origem          as char
    field ped_vlr_nf                    as int
    field ped_ps_real                   as int
    field ped_ps_cubado                 as int  /*n*/
    field ped_tp_pedido                 as char
    field cli_nm_cliente                as char
    field loc_cep                       as char
    field loc_endereco                  as char
    field loc_endereco_numero           as char
    field loc_bairro                    as char
    field loc_cidade                    as char
    field loc_cidade_cod_ibge           as char
    field loc_sg_uf                     as char
    field tp_frete                      as char
    field cli_id_contribuinte_icms      as char
    field ped_cd_transp_origem          as char.
    
def temp-table ti_e_cancela_documento no-undo
    field ped_cd_empresa                as int
    field ped_cd_cliente                as int
    field ped_nu_pedido_origem          as char
    field ped_ds_motivo_cancelamento    as char
    field dt_addrow                     as date
    field id_processado                 as char.    

run esp/eswmapi999.p persistent set h-api-wis.
run esp/eswmapi998.p persistent set h-api-fis.

for first embarque where
          embarque.cdd-embarq = int(entry(1,cdd-embarqres-cli,"|")) no-lock: end.

for first res-cli where
          res-cli.cdd-embarq = int(entry(1,cdd-embarqres-cli,"|")) and
          res-cli.nr-resumo    = int(entry(2,cdd-embarqres-cli,"|")) no-lock: end.

put unformatted
    "- " string(time,"hh:mm:ss") 
    " - inicio --> eswms007 - " cdd-embarqres-cli skip.
    
assign l-item = no.            
    
for each it-pre-fat where
         it-pre-fat.cdd-embarq  = embarque.cdd-embarq and
         it-pre-fat.nr-embarque = 0                   and
         it-pre-fat.nr-resumo   = res-cli.nr-resumo   no-lock,
    first ped-venda where 
          ped-venda.nome-abrev = it-pre-fat.nome-abrev and 
          ped-venda.nr-pedcli  = it-pre-fat.nr-pedcli no-lock,
    first item where
          item.it-codigo = it-pre-fat.it-codigo no-lock
    break by it-pre-fat.cdd-embarq on error undo , return "nok":
    
    if first-of(it-pre-fat.cdd-embarq) then
        assign l-item = no.       
    
    if item.politica = 6 then do:
        create int_e_det_pedido_saida.
        assign int_e_det_pedido_saida.nu_pedido_origem  = ped-venda.nr-pedcli       
               int_e_det_pedido_saida.cd_cliente        = ped-venda.cod-emitente
               int_e_det_pedido_saida.cd_empresa        = if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1
               int_e_det_pedido_saida.cd_deposito       = ped-venda.cod-estabel
               int_e_det_pedido_saida.cd_produto        = it-pre-fat.it-codigo 
               int_e_det_pedido_saida.nu_item_corp      = it-pre-fat.nr-sequencia
               int_e_det_pedido_saida.qt_separar        = it-pre-fat.qt-alocada
               int_e_det_pedido_saida.dt_addrow         = today
               int_e_det_pedido_saida.cd_situacao       = if p-acao = "cancelar" then 2 else 1 .
                                   
        put  unformat "- " string(time,"hh:mm:ss") ' pedlogitem>> ' ped-venda.nr-pedcli ' - ' it-pre-fat.it-codigo  skip .
   
        run pi_insert in h-api-wis (temp-table int_e_det_pedido_saida:default-buffer-handle).
        
        put  unformat "- " string(time,"hh:mm:ss") ' pedlogitemret>> ' return-value skip .
        
        if return-value <> "ok" then do:
            for each esp-fila-wms exclusive-lock
               where esp-fila-wms.cod-trans = "embarque"
                 and esp-fila-wms.chave     = cdd-embarqres-cli + "|" + p-acao:
                 
                 create esp-erro-wms.
                 assign esp-erro-wms.id             = esp-fila-wms.id
                        esp-erro-wms.data-hora-erro = now
                        esp-erro-wms.mensagem       = "item: " + it-pre-fat.it-codigo + ", pedido: " + it-pre-fat.nr-pedcli + ": " +
                                                       return-value.   
                 return "nok".                                   
            end.
        end.
        else
            assign l-item = yes.
                   
                 
    end.
    else do:
    
        put unformat 
            "it-pre-fat.cdd-embarq   - " it-pre-fat.cdd-embarq   skip
            "it-pre-fat.nr-resumo    - " it-pre-fat.nr-resumo    skip
            "it-pre-fat.nome-abrev   - " it-pre-fat.nome-abrev   skip
            "it-pre-fat.nr-pedcli    - " it-pre-fat.nr-pedcli    skip  
            "it-pre-fat.nr-sequencia - " it-pre-fat.nr-sequencia skip.              
                        
        for each it-dep-fat where
                 it-dep-fat.cdd-embarq   = it-pre-fat.cdd-embarq   and
                 it-dep-fat.nr-resumo    = it-pre-fat.nr-resumo    and 
                 it-dep-fat.nome-abrev   = it-pre-fat.nome-abrev   and
                 it-dep-fat.nr-pedcli    = it-pre-fat.nr-pedcli    and 
                 it-dep-fat.nr-sequencia = it-pre-fat.nr-sequencia no-lock:
    
            create int_e_det_pedido_saida.
            assign int_e_det_pedido_saida.nu_pedido_origem  = ped-venda.nr-pedcli           
                   int_e_det_pedido_saida.cd_cliente        = ped-venda.cod-emitente
                   int_e_det_pedido_saida.cd_empresa        = if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1
                   int_e_det_pedido_saida.cd_deposito       = ped-venda.cod-estabel
                   int_e_det_pedido_saida.cd_produto        = it-pre-fat.it-codigo 
                   int_e_det_pedido_saida.nu_item_corp      = it-pre-fat.nr-sequencia
                   int_e_det_pedido_saida.qt_separar        = it-dep-fat.qt-alocada
                   int_e_det_pedido_saida.dt_addrow         = today
                   int_e_det_pedido_saida.cd_situacao       = if p-acao = "cancelar" then 2 else 1.
                                       
            put unformat 
                "- " string(time,"hh:mm:ss") ' pedlogitem>> ' ped-venda.nr-pedcli ' - ' it-pre-fat.it-codigo  skip .
       
            run pi_insert in h-api-wis (temp-table int_e_det_pedido_saida:default-buffer-handle).

            put unformat 
                "- " string(time,"hh:mm:ss") ' pedlogitemret>> ' return-value skip .
            
            if return-value <> "ok" then do:
                for each esp-fila-wms exclusive-lock
                   where esp-fila-wms.cod-trans = "embarque"
                     and esp-fila-wms.chave     = cdd-embarqres-cli + "|" + p-acao:
                     
                     create esp-erro-wms.
                     assign esp-erro-wms.id             = esp-fila-wms.id
                            esp-erro-wms.data-hora-erro = now
                            esp-erro-wms.mensagem       = "item: " + it-pre-fat.it-codigo + ", pedido: " + it-pre-fat.nr-pedcli + ": " +
                                                           return-value.   
                     run pi_finalizar in h-api-wis.
                     delete procedure h-api-wis.  
                     run pi_finalizar in h-api-fis.
                     delete procedure h-api-fis.  
                     return "nok".                                   
                end.
            end.
            else
                assign l-item = yes.
                
        end.
    end.
    
    if last-of(it-pre-fat.cdd-embarq) then do on error undo, return "nok":
       
        for first emitente no-lock
            where emitente.cod-emitente = ped-venda.cod-emitente:
        end.

        for first transporte no-lock
            where transporte.nome-abrev = ped-venda.nome-transp:
        end.

        if l-item then do:

            create int_e_cab_pedido_saida.
            assign int_e_cab_pedido_saida.nu_pedido_origem     = ped-venda.nr-pedcli
                   int_e_cab_pedido_saida.cd_carga             = int(string(res-cli.cdd-embarq,"9999999") + string(res-cli.nr-resumo,"999"))
                   int_e_cab_pedido_saida.cd_cliente           = ped-venda.cod-emitente
                   int_e_cab_pedido_saida.cd_empresa           = if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1
                   int_e_cab_pedido_saida.cd_deposito          = ped-venda.cod-estabel
                   int_e_cab_pedido_saida.dt_entrega           = embarque.dt-embarque
                   int_e_cab_pedido_saida.cd_situacao          = if p-acao = "cancelar" then 2 else 1
                   int_e_cab_pedido_saida.tp_pedido            = caps(ped-venda.tp-pedido)
                   int_e_cab_pedido_saida.ds_cliente           = emitente.nome-emit
                   int_e_cab_pedido_saida.ds_cliente_entrega   = emitente.nome-emit
                   int_e_cab_pedido_saida.ds_endereco          = substring(emitente.endereco + " " + emitente.bairro + " " + " " + 
                                                                         emitente.cidade   + " " + emitente.estado + " " + emitente.cep,1,100)
                   int_e_cab_pedido_saida.nu_seq_entrega       = 1
                   int_e_cab_pedido_saida.cd_transportador     = if avail transporte then transporte.cod-transp else 0
                   int_e_cab_pedido_saida.nu_doc_erp           = substring(string(i-ep-codigo-usuario,"9999") + string(ped-venda.nr-pedido,"999999999999") +
                                                                 ped-venda.tp-pedido + string(emitente.cod-emitente,"9999999999") + string(res-cli.cdd-embarq,"9999999") + string(res-cli.nr-resumo,"999"),1,30)
                   int_e_cab_pedido_saida.ds_municipio_entrega = ped-venda.cidade
                   int_e_cab_pedido_saida.cd_uf_entrega        = ped-venda.estado
                   int_e_cab_pedido_saida.cd_cep_entrega       = trim(emitente.cep).
                   
            find imp_pedidos no-lock where
                 imp_pedidos.nr-pedido = ped-venda.nr-pedido no-error.
            if avail imp_pedidos then do:
    
                if mgcpb.imp_pedidos.l-3 = true then
                    assign int_e_cab_pedido_saida.ds_observacao = "urgente ".
    
                case mgcpb.imp_pedidos.c-2:
                    when "transportadora c/frete":u then
                        assign int_e_cab_pedido_saida.ds_observacao          = int_e_cab_pedido_saida.ds_observacao + " transportadora c/frete ":u.
                    when "malote" then
                        assign int_e_cab_pedido_saida.cd_transportadora      = 6
                               int_e_cab_pedido_saida.cd_cnpj_transportadora = 34028316710151.
                    when "sedex" then
                        assign int_e_cab_pedido_saida.cd_transportadora      = 13
                               int_e_cab_pedido_saida.cd_cnpj_transportadora = 34028316710151.
                    when "sedex s/custo":u then
                        assign int_e_cab_pedido_saida.cd_transportadora      = 12
                               int_e_cab_pedido_saida.cd_cnpj_transportadora = 34028316710151.
                    when "aÇreo":u then
                        assign int_e_cab_pedido_saida.ds_observacao          = int_e_cab_pedido_saida.ds_observacao + " aÇreo ":u
                               int_e_cab_pedido_saida.cd_transportadora      = 0
                               int_e_cab_pedido_saida.cd_cnpj_transportadora = 0.
                    when "aÇreo s/custo":u then
                        assign int_e_cab_pedido_saida.ds_observacao          = int_e_cab_pedido_saida.ds_observacao + " aÇreo s/custo ":u
                               int_e_cab_pedido_saida.cd_transportadora      = 0
                               int_e_cab_pedido_saida.cd_cnpj_transportadora = 0.
                    when "vem retirar" then
                        assign int_e_cab_pedido_saida.cd_transportadora      = 999.
                    when "correios" then
                        assign int_e_cab_pedido_saida.cd_transportadora      = 10
                               int_e_cab_pedido_saida.cd_cnpj_transportadora = 34028316710151.                           
                end case.
                
                assign int_e_cab_pedido_saida.ds_observacao = int_e_cab_pedido_saida.ds_observacao + imp_pedidos.obs-extra[1].
                
            end.        
    
            put unformat 
                "- " string(time,"hh:mm:ss") ' pedlog>> ' ped-venda.nr-pedcli ' - ' embarque.cdd-embarq skip .
        
            run pi_insert in h-api-wis (temp-table int_e_cab_pedido_saida:default-buffer-handle).
            
            put unformat 
                "- " string(time,"hh:mm:ss") ' pedlogret>> ' return-value skip . 
                
            if return-value <> "ok" then do:
                for each esp-fila-wms exclusive-lock
                   where esp-fila-wms.cod-trans = "embarque"
                     and esp-fila-wms.chave     = cdd-embarqres-cli:
                     
                     create esp-erro-wms.
                     assign esp-erro-wms.id             = esp-fila-wms.id
                            esp-erro-wms.data-hora-erro = now
                            esp-erro-wms.mensagem       = "pedido: " + ped-venda.nr-pedcli + ": " + return-value.
                            
                     return "nok".        
                     
                end.
            end.
        end.

        find first loc-entr no-lock
             where loc-entr.cod-entr   = ped-venda.cod-entrega 
               and loc-entr.nome-abrev = ped-venda.nome-abrev no-error.
                    
             
        if p-acao <> 'Cancelar' then do:     

            /*metodo que busca o logradouro e o numero*/
            if not valid-handle(h-cdapi704) then
                run cdp/cdapi704.p persistent set h-cdapi704.
            
            run pi-trata-endereco in h-cdapi704 (input loc-entr.endereco, 
                                                 output c-rua, 
                                                 output c-nro, 
                                                 output c-comp).
    
            find first mgcad.cidade no-lock where
                       cidade.cidade = loc-entr.cidade no-error.
                       
          /*Empresa: 1001
            C¢digo do cliente: C¢digo de cadastro do cliente no ERP.
            Pedido: N£mero do pedido.
            Valor nf: Valor do produto para c†lculo do frete.
            Peso real: Peso da mercadoria para c†lculo do frete (n∆o veio nos pedidos teste, Ç necess†rio definir agora, quem vai alimentar esse peso, se o ERP ou o WIS e quando).
            Transportadora origem: C¢digo da transportadora, cadastrado no ERP, WMS e FIS (mesmo c¢digo), pode vir c¢digo definido ou nulo para que o FIS verifique a transportadora que atende o frete com menor valor.
            Tipo de pedido: E (tem de ser fixo como ?E?, v†rios tipos vieram nos primeiros testes, mas o FIS est† configurado e cadastrado para calcular fretes com tipo de pedido E, caso necess†rio, posteriormente podemos inserir novos tipos)
            CEP: Cep do endereáo de entrega.
            Endereáo: Endereáo de entrega.
            N£mero: N£mero de entrega.
            Bairro: Bairro do endereáo de entrega.
            Cidade: Cidade da entrega.
            C¢digo IBGE: C¢digo de IBGE da cidade de entrega.
            UF: UF do estado destino.
            Tipo de frete: Tem obrigatoriamente que ser CIF.*/
                    
            create ti_e_documento_frete.
            assign ti_e_documento_frete.ped_cd_empresa          = 1001 /*if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1*/
                   ti_e_documento_frete.ped_cd_cliente          = ped-venda.cod-emitente
                   ti_e_documento_frete.ped_nu_pedido_origem    = ped-venda.nr-pedcli
                   ti_e_documento_frete.ped_vlr_nf              = ped-venda.vl-tot-ped
                   ti_e_documento_frete.ped_cd_transp_origem    = if avail transporte then string(transporte.cod-transp) else ''
                   ti_e_documento_frete.ped_tp_pedido           = 'E' /*ped-venda.tp-pedido*/
                   ti_e_documento_frete.cli_nm_cliente          = emitente.nome-emit
                   ti_e_documento_frete.loc_cep                 = trim(loc-entr.cep)
                   ti_e_documento_frete.loc_endereco            = c-rua
                   ti_e_documento_frete.loc_endereco_numero     = c-nro
                   ti_e_documento_frete.loc_bairro              = loc-entr.bairro
                   ti_e_documento_frete.loc_cidade              = loc-entr.cidade
                   ti_e_documento_frete.loc_cidade_cod_ibge     = if avail cidade then string(cidade.cdn-munpio-ibge) else ''
                   ti_e_documento_frete.loc_sg_uf               = loc-entr.estado
                   ti_e_documento_frete.tp_frete                = 'CIF' /*if ped-venda.cidade-cif <> "" then "cif" else "fob"*/
                   ti_e_documento_frete.cli_id_contribuinte_icms = if emitente.contrib-icms then 'S' else 'N'
                   .
            
            if ti_e_documento_frete.ped_cd_transp_origem = '0' then
                assign ti_e_documento_frete.ped_cd_transp_origem = ''.
            
            /*
            if ped-venda.nr-pedcli begins "6-" then
                assign ti_e_documento_frete.tp_frete = 'cif'.
            */
            
            for each res-cli no-lock
               where res-cli.cdd-embarq  = it-pre-fat.cdd-embarq
                 and res-cli.nr-resumo   = it-pre-fat.nr-resumo:
                 assign ti_e_documento_frete.ped_ps_real    = ti_e_documento_frete.ped_ps_real   + res-cli.peso-liq-tot
                        ti_e_documento_frete.ped_ps_cubado  = ti_e_documento_frete.ped_ps_cubado + res-cli.peso-bru-tot.  
            end.
    
            put unformat 
                'fretelog>> ' ped-venda.nr-pedcli ' - ' embarque.cdd-embarq skip .
    
            run pi_insert in h-api-fis (temp-table ti_e_documento_frete:default-buffer-handle).
            
            put unformat 
                'fretelog>> ' return-value skip .
            
            if return-value <> "ok" then do:
                for each esp-fila-wms exclusive-lock
                   where esp-fila-wms.cod-trans = "embarque"
                     and esp-fila-wms.chave     = cdd-embarqres-cli:
                     
                     create esp-erro-wms.
                     assign esp-erro-wms.id             = esp-fila-wms.id
                            esp-erro-wms.data-hora-erro = now
                            esp-erro-wms.mensagem       = "pedido: " + ped-venda.nr-pedcli + ": " + return-value.
                     
                     return "nok".        
                     
                end.
            end.
            else
                for each esp-fila-wms exclusive-lock
                   where esp-fila-wms.cod-trans = "embarque"
                     and esp-fila-wms.chave     = cdd-embarqres-cli:
                     
                     if esp-fila-wms.data-hora-integracao = ? then
                         assign esp-fila-wms.data-hora-integracao = now.
                end.
        end.        
        else do:
            
            if avail ped-venda then do:

                create ti_e_cancela_documento.
                assign ti_e_cancela_documento.ped_cd_empresa             = 1001 /*if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1*/
                       ti_e_cancela_documento.ped_cd_cliente             = ped-venda.cod-emitente
                       ti_e_cancela_documento.ped_nu_pedido_origem       = ped-venda.nr-pedcli
                       ti_e_cancela_documento.ped_ds_motivo_cancelamento = 'Embarque ' + string(embarque.cdd-embarq) + ' Cancelado'.
            
                run pi_insert in h-api-fis (temp-table ti_e_cancela_documento:default-buffer-handle).
            
                if return-value <> "ok" then do:
                    
                    for each esp-fila-wms exclusive-lock
                       where esp-fila-wms.cod-trans = "embarque"
                         and esp-fila-wms.chave     = cdd-embarqres-cli:
                         
                         create esp-erro-wms.
                         assign esp-erro-wms.id             = esp-fila-wms.id
                                esp-erro-wms.data-hora-erro = now
                                esp-erro-wms.mensagem       = "pedido: " + ped-venda.nr-pedcli + ": " + return-value.
                          
                         return "nok".        
                         
                    end.
                
                end.
                else
                    for each esp-fila-wms exclusive-lock
                       where esp-fila-wms.cod-trans = "embarque"
                         and esp-fila-wms.chave     = cdd-embarqres-cli:
                          
                         if esp-fila-wms.data-hora-integracao = ? then
                             assign esp-fila-wms.data-hora-integracao = now.
                    end.
            end.        
        end.
    end.
end.

run pi_finalizar in h-api-wis.
run pi_finalizar in h-api-fis.

delete procedure h-api-wis.
delete procedure h-api-fis.
delete procedure h-cdapi704.

return "ok".


