/**************************************************************************
**   Programa: esp/eswms007.p
**   Data    : Maio 2015
**   Autor   : Eder
**   Objetivo: Integra‡Æo WMS Embarques
**   Versao..: 
**************************************************************************/

def buffer prog_dtsul   for emsfnd.prog_dtsul.
def buffer usuar_mestre for emsfnd.usuar_mestre.

{include/i-prgvrs.i eswms007.p 2.11.00.001}
{utp/ut-glob.i}

DEFINE INPUT PARAM cdd-embarqRes-cli AS CHAR  NO-UNDO.
DEFINE INPUT PARAM p-acao            AS CHAR NO-UNDO.

def new global shared var g-integrator as LOG no-undo.

if not g-integrator then do:
    run tools/QueueManager.p ("Embarque",cdd-embarqRes-cli + "|" + p-acao).        
    return "OK".
end.

function fn_only_numbers returns character
    (input cValueString as character):

    define variable iPos     as integer   no-undo.
    define variable iLength  as integer   no-undo.
    define variable cData    as character no-undo.
    define variable cDataNew as character no-undo.


    assign iLength = length(cValueString).

    do iPos = 1 to iLength:
        assign cData = substring(cValueString,iPos,1).
        
        if cData < '0' or cData > '9' then
            next.

        assign cDataNew = cDataNew + cData.
    end.

    return cDataNew.
end function.

def temp-table INT_E_CAB_PEDIDO_SAIDA no-undo
    fields NU_PEDIDO_ORIGEM            as CHAR FORMAT "x(20)"
    fields CD_CARGA	            as integer
    fields CD_CLIENTE           as integer
    fields CD_EMPRESA           as integer
    fields CD_DEPOSITO          as char format "x(3)"
    fields DT_ENTREGA           as date
    fields CD_SITUACAO	        as integer
    fields TP_PEDIDO            as char format "x"
/*    fields DS_CLIENTE	        as char format "x(100)"*/
    fields DS_CLIENTE_ENTREGA	        as char format "x(100)"
    fields DS_ENDERECO_ENTREGA  as char format "x(100)"
    fields NU_SEQ_ENTREGA       as integer
    fields CD_TRANSPORTADORA    as integer
    fields CD_CNPJ_TRANSPORTADORA as dec
    fields NU_DOC_ERP	        as char format "x(30)"
    FIELDS DS_MUNICIPIO_ENTREGA AS CHAR FORMAT "x(72)"
    FIELDS DS_BAIRRO_ENTREGA    AS CHAR FORMAT "X(72)"
    FIELDS CD_UF_ENTREGA        AS CHAR FORMAT "x(2)" 
    FIELDS CD_CEP_ENTREGA       AS CHAR FORMAT "x(10)" 
    FIELDS NU_ENDERECO_ENTREGA  AS CHAR FORMAT "x(10)"
    FIELDS DS_OBSERVACAO        AS CHAR FORMAT "x(500)".

def temp-table INT_E_DET_PEDIDO_SAIDA no-undo
    fields NU_PEDIDO_ORIGEM       AS CHAR FORMAT "x(20)"
    fields CD_CLIENTE             as integer
    fields CD_EMPRESA             as integer
    fields CD_DEPOSITO            as char format "x(3)"
    fields CD_PRODUTO             as char format "x(40)"
    fields NU_ITEM_CORP           as integer
    fields QT_SEPARAR	       as decimal decimals 3
    fields NU_LOTE                AS CHARACTER
    /*fields CD_SIMILAR             as char format "x(50)"
    fields NU_VOLUME              as integer format ">>>>>>>9"*/
    fields CD_SITUACAO	       as integer FORMAT ">>9" 
    /*fields ID_EMBALAGEM_PRESENTE  AS CHAR FORMAT "x(1)"*/
    FIELDS DT_ADDROW              AS date.


def var l-item    as log    no-undo.    
def var l-ult     as log    no-undo.
def var h-api     as handle no-undo.
DEF VAR c-cgc-ped AS CHAR   NO-UNDO.
run esp/eswmapi999.p persistent set h-api.

FOR FIRST embarque WHERE
    embarque.cdd-embarq = INT(ENTRY(1,cdd-embarqRes-cli,"|")) NO-LOCK: END.

FOR FIRST res-cli WHERE
    res-cli.cdd-embarq = INT(ENTRY(1,cdd-embarqRes-cli,"|")) AND
    res-cli.nr-resumo    = INT(ENTRY(2,cdd-embarqRes-cli,"|")) NO-LOCK: END.

PUT UNFORMATTED
    "- " STRING(TIME,"hh:mm:ss") 
    " - Inicio --> ESWMS007 - " cdd-embarqRes-cli SKIP.
    
assign l-item = no.            
    
FOR EACH it-pre-fat WHERE
    it-pre-fat.cdd-embarq  = embarque.cdd-embarq AND
    it-pre-fat.nr-embarque = 0                   and
    it-pre-fat.nr-resumo   = res-cli.nr-resumo   NO-LOCK,
    FIRST ped-venda WHERE 
    ped-venda.nome-abrev = it-pre-fat.nome-abrev AND 
    ped-venda.nr-pedcli  = it-pre-fat.nr-pedcli NO-LOCK,
    FIRST item WHERE
    item.it-codigo = it-pre-fat.it-codigo NO-LOCK
    BREAK BY it-pre-fat.cdd-embarq ON ERROR UNDO , RETURN "NOK":
    
/*    PUT  UNFORMATTED
        "- " STRING(TIME,"hh:mm:ss") 
        " - Item --> Antes" SKIP.
    
    RUN esp/eswms001.p (BUFFER item).
    
    PUT  UNFORMATTED
        "- " STRING(TIME,"hh:mm:ss") 
        " - Item --> Depois" SKIP
        "Politica --> " item.politica skip.   */ 
    
    if item.politica = 6 then do:
        CREATE INT_E_DET_PEDIDO_SAIDA.
        ASSIGN INT_E_DET_PEDIDO_SAIDA.NU_PEDIDO_ORIGEM  = ped-venda.nr-pedcli       
               INT_E_DET_PEDIDO_SAIDA.CD_CLIENTE        = ped-venda.cod-emitente
               INT_E_DET_PEDIDO_SAIDA.CD_EMPRESA        = if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1
               INT_E_DET_PEDIDO_SAIDA.CD_DEPOSITO       = ped-venda.cod-estabel
               INT_E_DET_PEDIDO_SAIDA.CD_PRODUTO        = it-pre-fat.it-codigo 
               INT_E_DET_PEDIDO_SAIDA.NU_ITEM_CORP      = it-pre-fat.nr-sequencia
               INT_E_DET_PEDIDO_SAIDA.QT_SEPARAR        = it-pre-fat.qt-alocada
               INT_E_DET_PEDIDO_SAIDA.DT_ADDROW         = today
               INT_E_DET_PEDIDO_SAIDA.CD_SITUACAO       = if p-acao = "Cancelar" then 2 else 1 .
                                   
        PUT  UNFORMAT "- " STRING(TIME,"hh:mm:ss") ' PedLogitem>> ' ped-venda.nr-pedcli ' - ' it-pre-fat.it-codigo  SKIP .
   
        run pi_insert in h-api (temp-table INT_E_DET_PEDIDO_SAIDA:default-buffer-handle).
        
        PUT  UNFORMAT "- " STRING(TIME,"hh:mm:ss") ' PedLogItemRet>> ' return-value SKIP .
        
        if return-value <> "OK" then do:
            for each esp-fila-wms exclusive-lock
               where esp-fila-wms.cod-trans = "Embarque"
                 and esp-fila-wms.chave     = cdd-embarqRes-cli + "|" + p-acao:
                 
                 create esp-erro-wms.
                 assign esp-erro-wms.id             = esp-fila-wms.id
                        esp-erro-wms.data-hora-erro = now
                        esp-erro-wms.mensagem       = "Item: " + it-pre-fat.it-codigo + ", Pedido: " + it-pre-fat.nr-pedcli + ": " +
                                                       return-value.   
                 run pi_finalizar in h-api.
                 delete procedure h-api.   
                 return "NOK".                                   
            end.
        end.
        else
            assign l-item = yes.
                   
                 
    end.
    else do:
    
        PUT  UNFORMAT "it-pre-fat.cdd-embarq - " it-pre-fat.cdd-embarq skip
                      "it-pre-fat.nr-resumo - " it-pre-fat.nr-resumo skip
                      "it-pre-fat.nome-abrev - " it-pre-fat.nome-abrev skip
                      "it-pre-fat.nr-pedcli - " it-pre-fat.nr-pedcli skip  
                      "it-pre-fat.nr-sequencia - " it-pre-fat.nr-sequencia skip.
                        
        FOR EACH it-dep-fat WHERE
            it-dep-fat.cdd-embarq   = it-pre-fat.cdd-embarq   AND
            it-dep-fat.nr-resumo    = it-pre-fat.nr-resumo    AND 
            it-dep-fat.nome-abrev   = it-pre-fat.nome-abrev   AND
            it-dep-fat.nr-pedcli    = it-pre-fat.nr-pedcli    AND 
            it-dep-fat.nr-sequencia = it-pre-fat.nr-sequencia /*AND
            it-dep-fat.it-codigo    = it-pre-fat.it-codigo    AND
            it-dep-fat.nr-entrega   = it-pre-fat.nr-entrega*/ NO-LOCK:
    
            CREATE INT_E_DET_PEDIDO_SAIDA.
            ASSIGN INT_E_DET_PEDIDO_SAIDA.NU_PEDIDO_ORIGEM  = ped-venda.nr-pedcli           
                   INT_E_DET_PEDIDO_SAIDA.CD_CLIENTE        = ped-venda.cod-emitente
                   INT_E_DET_PEDIDO_SAIDA.CD_EMPRESA        = if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1
                   INT_E_DET_PEDIDO_SAIDA.CD_DEPOSITO       = ped-venda.cod-estabel
                   INT_E_DET_PEDIDO_SAIDA.CD_PRODUTO        = it-pre-fat.it-codigo 
                   INT_E_DET_PEDIDO_SAIDA.NU_ITEM_CORP      = it-pre-fat.nr-sequencia
                   INT_E_DET_PEDIDO_SAIDA.QT_SEPARAR        = it-dep-fat.qt-alocada
                   INT_E_DET_PEDIDO_SAIDA.DT_ADDROW         = today
                   INT_E_DET_PEDIDO_SAIDA.CD_SITUACAO       = if p-acao = "Cancelar" then 2 else 1
      /*           INT_E_DET_PEDIDO_SAIDA.NU_VOLUME         = 2
                  INT_E_DET_PEDIDO_SAIDA.CD_SIMILAR         = if avail es_ped_item then es_ped_item.it-similar else "999"*/ .
                                       
            PUT  UNFORMAT "- " STRING(TIME,"hh:mm:ss") ' PedLogitem>> ' ped-venda.nr-pedcli ' - ' it-pre-fat.it-codigo  SKIP .
       
            run pi_insert in h-api (temp-table INT_E_DET_PEDIDO_SAIDA:default-buffer-handle).
            PUT  UNFORMAT "- " STRING(TIME,"hh:mm:ss") ' PedLogItemRet>> ' return-value SKIP .
            
            if return-value <> "OK" then do:
                for each esp-fila-wms exclusive-lock
                   where esp-fila-wms.cod-trans = "Embarque"
                     and esp-fila-wms.chave     = cdd-embarqRes-cli + "|" + p-acao:
                     
                     create esp-erro-wms.
                     assign esp-erro-wms.id             = esp-fila-wms.id
                            esp-erro-wms.data-hora-erro = now
                            esp-erro-wms.mensagem       = "Item: " + it-pre-fat.it-codigo + ", Pedido: " + it-pre-fat.nr-pedcli + ": " +
                                                           return-value.   
                     run pi_finalizar in h-api.
                     delete procedure h-api.   
                     return "NOK".                                   
                end.
            end.
            else
                assign l-item = yes.
                
            end.
    end.
    
    if LAST-OF(it-pre-fat.cdd-embarq) and l-item then do on error undo, return "NOK":
        for first emitente no-lock
            where emitente.cod-emitente = ped-venda.cod-emitente:
        end.

        for first transporte no-lock
            where transporte.nome-abrev = ped-venda.nome-transp:
        end.

        CREATE INT_E_CAB_PEDIDO_SAIDA.
        ASSIGN INT_E_CAB_PEDIDO_SAIDA.NU_PEDIDO_ORIGEM     = ped-venda.nr-pedcli
               INT_E_CAB_PEDIDO_SAIDA.CD_CARGA             = INT(STRING(res-cli.cdd-embarq,"9999999") + string(res-cli.nr-resumo,"999"))
               INT_E_CAB_PEDIDO_SAIDA.CD_CLIENTE           = ped-venda.cod-emitente
               INT_E_CAB_PEDIDO_SAIDA.CD_EMPRESA           = if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1
               INT_E_CAB_PEDIDO_SAIDA.CD_DEPOSITO          = ped-venda.cod-estabel
               INT_E_CAB_PEDIDO_SAIDA.DT_ENTREGA           = embarque.dt-embarque
               INT_E_CAB_PEDIDO_SAIDA.CD_SITUACAO          = if p-acao = "Cancelar" then 2 else 1
               INT_E_CAB_PEDIDO_SAIDA.TP_PEDIDO            = CAPS(ped-venda.tp-pedido)
               INT_E_CAB_PEDIDO_SAIDA.DS_CLIENTE           = emitente.nome-emit
               INT_E_CAB_PEDIDO_SAIDA.DS_CLIENTE_ENTREGA   = emitente.nome-emit
               INT_E_CAB_PEDIDO_SAIDA.DS_ENDERECO          = substring(emitente.endereco + " " + emitente.bairro + " " + " " + 
                                                                     emitente.cidade   + " " + emitente.estado + " " + emitente.cep,1,100)
               INT_E_CAB_PEDIDO_SAIDA.NU_SEQ_ENTREGA       = 1
               INT_E_CAB_PEDIDO_SAIDA.CD_TRANSPORTADOR     = if avail transporte then transporte.cod-transp else 0
               INT_E_CAB_PEDIDO_SAIDA.NU_DOC_ERP           = substring(string(i-ep-codigo-usuario,"9999") + string(ped-venda.nr-pedido,"999999999999") +
                                                             ped-venda.tp-pedido + string(emitente.cod-emitente,"9999999999") + STRING(res-cli.cdd-embarq,"9999999") + string(res-cli.nr-resumo,"999"),1,30)
               INT_E_CAB_PEDIDO_SAIDA.DS_MUNICIPIO_ENTREGA = ped-venda.cidade
               INT_E_CAB_PEDIDO_SAIDA.cd_UF_ENTREGA        = ped-venda.estado
               INT_E_CAB_PEDIDO_SAIDA.CD_CEP_ENTREGA       = trim(emitente.cep).
               
        find imp_pedidos no-lock where
             imp_pedidos.nr-pedido = ped-venda.nr-pedido no-error.
        if avail imp_pedidos then do:

            if mgcpb.Imp_pedidos.l-3 = true then
                assign INT_E_CAB_PEDIDO_SAIDA.DS_OBSERVACAO = "Urgente ".

            case mgcpb.Imp_pedidos.c-2:
                when "Transportadora C/Frete":U then
                    assign INT_E_CAB_PEDIDO_SAIDA.DS_OBSERVACAO          = INT_E_CAB_PEDIDO_SAIDA.DS_OBSERVACAO + " Transportadora C/Frete ":U.
                           /*INT_E_CAB_PEDIDO_SAIDA.CD_TRANSPORTADORA      = 900
                           INT_E_CAB_PEDIDO_SAIDA.CD_CNPJ_TRANSPORTADORA = 11111111111180.*/
                when "Malote" then
                    assign INT_E_CAB_PEDIDO_SAIDA.CD_TRANSPORTADORA      = 6
                           INT_E_CAB_PEDIDO_SAIDA.CD_CNPJ_TRANSPORTADORA = 34028316710151.
                when "Sedex" then
                    assign INT_E_CAB_PEDIDO_SAIDA.CD_TRANSPORTADORA      = 13
                           INT_E_CAB_PEDIDO_SAIDA.CD_CNPJ_TRANSPORTADORA = 34028316710151.
                when "Sedex S/Custo":U then
                    assign INT_E_CAB_PEDIDO_SAIDA.CD_TRANSPORTADORA      = 12
                           INT_E_CAB_PEDIDO_SAIDA.CD_CNPJ_TRANSPORTADORA = 34028316710151.
                when "A‚reo":U then
                    assign INT_E_CAB_PEDIDO_SAIDA.DS_OBSERVACAO          = INT_E_CAB_PEDIDO_SAIDA.DS_OBSERVACAO + " A‚reo ":U
                           INT_E_CAB_PEDIDO_SAIDA.CD_TRANSPORTADORA      = 0
                           INT_E_CAB_PEDIDO_SAIDA.CD_CNPJ_TRANSPORTADORA = 0.
                when "A‚reo S/Custo":U then
                    assign INT_E_CAB_PEDIDO_SAIDA.DS_OBSERVACAO          = INT_E_CAB_PEDIDO_SAIDA.DS_OBSERVACAO + " A‚reo S/Custo ":U
                           INT_E_CAB_PEDIDO_SAIDA.CD_TRANSPORTADORA      = 0
                           INT_E_CAB_PEDIDO_SAIDA.CD_CNPJ_TRANSPORTADORA = 0.
                when "Vem Retirar" then
                    assign INT_E_CAB_PEDIDO_SAIDA.CD_TRANSPORTADORA      = 999.
                           /*INT_E_CAB_PEDIDO_SAIDA.CD_CNPJ_TRANSPORTADORA = 77777777777714.*/
                when "Correios" then
                    assign INT_E_CAB_PEDIDO_SAIDA.CD_TRANSPORTADORA      = 10
                           INT_E_CAB_PEDIDO_SAIDA.CD_CNPJ_TRANSPORTADORA = 34028316710151.                           
            end case.
            
            assign INT_E_CAB_PEDIDO_SAIDA.DS_OBSERVACAO = INT_E_CAB_PEDIDO_SAIDA.DS_OBSERVACAO + imp_pedidos.obs-extra[1].
            
        end.

        PUT  UNFORMAT "- " STRING(TIME,"hh:mm:ss") ' PedLog>> ' ped-venda.nr-pedcli ' - ' embarque.cdd-embarq SKIP .
    
        run pi_insert in h-api (temp-table INT_E_CAB_PEDIDO_SAIDA:default-buffer-handle).
        PUT  UNFORMAT "- " STRING(TIME,"hh:mm:ss") ' PedLogRet>> ' return-value SKIP .
                
        
        if return-value <> "OK" then do:
            for each esp-fila-wms exclusive-lock
               where esp-fila-wms.cod-trans = "Embarque"
                 and esp-fila-wms.chave     = cdd-embarqRes-cli:
                 
                 create esp-erro-wms.
                 assign esp-erro-wms.id             = esp-fila-wms.id
                        esp-erro-wms.data-hora-erro = now
                        esp-erro-wms.mensagem       = "Pedido: " + ped-venda.nr-pedcli + ": " + return-value.
                        
                 run pi_finalizar in h-api.
                 delete procedure h-api.   
                 return "NOK".        
                 
            end.
        end.
        else
            for each esp-fila-wms exclusive-lock
               where esp-fila-wms.cod-trans = "Embarque"
                 and esp-fila-wms.chave     = cdd-embarqRes-cli:
                 
                 if esp-fila-wms.data-hora-integracao = ? then
                     assign esp-fila-wms.data-hora-integracao = now.
            end.
    end.
end.

run pi_finalizar in h-api.

delete procedure h-api.

return "ok".


