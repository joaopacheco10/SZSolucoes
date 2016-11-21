/**************************************************************************
**   Programa: esp/eswms007a.p
**   Data    : Maio de 2015
**   Autor   : Eder
**   Objetivo: Integra‡Æo WMS Embarques/Nr Nota Fiscal
**   Versao..: 
**************************************************************************/
{include/i-bfems2.i}
{include/i-prgvrs.i eswms007a 2.11.00.001}
{utp/ut-glob.i}

define param buffer nota-fiscal for nota-fiscal. 

def buffer bfnf for nota-fiscal.
DEFINE VARIABLE i-cod-emitente AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-not-fiscal   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-nota-aux     AS char     NO-UNDO.
DEFINE VARIABLE i-resumo       AS INTEGER     NO-UNDO.

def temp-table INT_E_CAB_PEDIDO_SAIDA no-undo
    fields NU_PEDIDO_ORIGEM            as CHAR FORMAT "x(20)"
    fields CD_CARGA	            as integer
    fields CD_CLIENTE           as integer
    fields CD_EMPRESA           as integer
    fields CD_DEPOSITO          as char format "x(3)"
    fields CD_SITUACAO	        as integer
    fields DS_CLIENTE	        as char format "x(100)"
    fields DS_CLIENTE_ENTREGA	as char format "x(100)"
    fields CD_TRANSPORTADORA    as integer
    FIELDS DS_MUNICIPIO_ENTREGA AS CHAR FORMAT "x(72)"
    FIELDS CD_UF_ENTREGA        AS CHAR FORMAT "x(2)" 
    FIELDS CD_CEP_ENTREGA       AS CHAR FORMAT "x(10)" 
    fields DT_ADDROW            as date.

def temp-table INT_E_DET_PEDIDO_SAIDA no-undo
    fields NU_PEDIDO_ORIGEM     AS CHAR FORMAT "x(20)"
    fields CD_CLIENTE    as integer
    fields CD_EMPRESA    as integer
    fields CD_DEPOSITO   as char format "x(3)"
    fields CD_SITUACAO	 as integer
    fields CD_PRODUTO    as char format "x(40)"
    fields NU_ITEM_CORP  as integer
    fields QT_SEPARAR	 as decimal decimals 3
    fields NU_LOTE       AS CHARACTER
    fields NU_NOTA       as character
    fields NU_SERIE_NOTA as character
    fields DT_ADDROW     as date
    /*fields CD_SIMILAR    as char format "x(50)"
    fields NU_VOLUME     as integer format ">>>>>>>9" */ .


def var h-api     as handle no-undo.

run esp/eswmapi999.p persistent set h-api.

ASSIGN i-resumo = -1.

for each it-nota-fisc of nota-fiscal no-lock,
    each fat-ser-lote of it-nota-fisc no-lock
    on error undo , return "NOK":
    
    ASSIGN i-cod-emitente = nota-fiscal.cod-emitente
           c-not-fiscal   = nota-fiscal.nr-nota-fis .
           c-nota-aux     = string(int(nota-fiscal.nr-nota-fis) + 1,"9999999").

    find first bfnf 
         where bfnf.cdd-embarq  = nota-fiscal.cdd-embarq                 
           and bfnf.nome-abrev  = nota-fiscal.nome-abrev
           and bfnf.nr-nota-fis = c-nota-aux
           and bfnf.nr-pedcli   = '' no-lock no-error .
    if avail bfnf then       
       ASSIGN i-cod-emitente = bfnf.cod-emitente 
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

    create INT_E_DET_PEDIDO_SAIDA.
    assign INT_E_DET_PEDIDO_SAIDA.NU_PEDIDO_ORIGEM = it-nota-fisc.nr-pedcli
           INT_E_DET_PEDIDO_SAIDA.CD_CLIENTE    = nota-fiscal.cod-emitente
           INT_E_DET_PEDIDO_SAIDA.CD_EMPRESA    = if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1
           INT_E_DET_PEDIDO_SAIDA.CD_DEPOSITO   = nota-fiscal.cod-estabel
           INT_E_DET_PEDIDO_SAIDA.CD_SITUACAO   = 3 /** Para nota fiscal **/
           INT_E_DET_PEDIDO_SAIDA.CD_PRODUTO    = it-nota-fisc.it-codigo 
           INT_E_DET_PEDIDO_SAIDA.NU_ITEM_CORP  = it-nota-fisc.nr-seq-ped
           INT_E_DET_PEDIDO_SAIDA.QT_SEPARAR    = fat-ser-lote.qt-baixada[1]
           INT_E_DET_PEDIDO_SAIDA.NU_LOTE       = fat-ser-lote.nr-serlote
           INT_E_DET_PEDIDO_SAIDA.NU_NOTA       = nota-fiscal.nr-nota-fis
           INT_E_DET_PEDIDO_SAIDA.NU_SERIE_NOTA = nota-fiscal.serie
           INT_E_DET_PEDIDO_SAIDA.DT_ADDROW     = today
           /*INT_E_DET_PEDIDO_SAIDA.CD_SIMILAR    = if avail es_ped_item then es_ped_item.it-similar else ""
           INT_E_DET_PEDIDO_SAIDA.NU_VOLUME     = int(nota-fiscal.nr-volumes)*/ . /*verificar*/.

    run pi_insert in h-api (temp-table INT_E_DET_PEDIDO_SAIDA:default-buffer-handle).
    if return-value <> "OK" then
        return "NOK".

    PUT  UNFORMAT skip 'NotaLogRet>> ' return-value SKIP .

    IF i-resumo <> nota-fiscal.nr-resumo THEN DO:
            
        create INT_E_CAB_PEDIDO_SAIDA.
        assign INT_E_CAB_PEDIDO_SAIDA.NU_PEDIDO_ORIGEM      = it-nota-fisc.nr-pedcli
               INT_E_CAB_PEDIDO_SAIDA.CD_CARGA              = INT(STRING(nota-fiscal.cdd-embarq,"9999999") + string(nota-fiscal.nr-resumo,"999"))
               INT_E_CAB_PEDIDO_SAIDA.CD_CLIENTE            = nota-fiscal.cod-emitente
               INT_E_CAB_PEDIDO_SAIDA.CD_EMPRESA            = if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1
               INT_E_CAB_PEDIDO_SAIDA.CD_DEPOSITO           = nota-fiscal.cod-estabel
               INT_E_CAB_PEDIDO_SAIDA.CD_SITUACAO           = 3 /** Para nota fiscal **/
               INT_E_CAB_PEDIDO_SAIDA.DS_CLIENTE            = emitente.nome-emit
               INT_E_CAB_PEDIDO_SAIDA.DS_CLIENTE_ENTREGA    = emitente.nome-emit
               INT_E_CAB_PEDIDO_SAIDA.CD_TRANSPORTADOR      = if avail transporte then transporte.cod-transp else 0
               INT_E_CAB_PEDIDO_SAIDA.DS_MUNICIPIO_ENTREGA  = nota-fiscal.cidade
               INT_E_CAB_PEDIDO_SAIDA.cd_UF_ENTREGA         = nota-fiscal.estado
               INT_E_CAB_PEDIDO_SAIDA.CD_CEP_ENTREGA        = trim(emitente.cep)
               INT_E_CAB_PEDIDO_SAIDA.DT_ADDROW             = today.
    
    
        PUT  UNFORMAT 'NotaLog>> ' nota-fiscal.nr-pedcli ' - ' nota-fiscal.cdd-embarq SKIP .
    
        run pi_insert in h-api (temp-table INT_E_CAB_PEDIDO_SAIDA:default-buffer-handle).
        PUT  UNFORMAT 'NotaLogRet>> ' return-value SKIP .

        ASSIGN i-resumo = nota-fiscal.nr-resumo.
    
    END.
end.

run pi_finalizar in h-api.

delete procedure h-api.

return "ok".
