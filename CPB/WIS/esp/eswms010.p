 /**************************************************************************
**   Programa: esp/eswms010.p
**   Data    : Junho de 2010
**   Autor   : Eder
**   Objetivo: Integra‡Æo WMS Movimenta‡Æo de estoque (Via Interface NF)
** Transa‡äes: ACA/EAC/TRA
**   Versao..: 
**************************************************************************/
      
def buffer prog_dtsul   for emsfnd.prog_dtsul.
def buffer usuar_mestre for emsfnd.usuar_mestre.

{include/i-prgvrs.i eswms010.p 2.04.00.000}
{utp/ut-glob.i}

define param buffer movto-estoq for movto-estoq. 

def new global shared var g-integrator as LOG no-undo.

if not g-integrator then do:
    run tools/QueueManager.p ("Movimenta‡Æo",string(movto-estoq.nr-trans)).        
    return "OK".
end.

def temp-table INT_E_CAB_NOTA_FISCAL no-undo
    fields CD_EMPRESA               as integer
    fields NU_INTERFACE             AS INT64
    fields CD_DEPOSITO              as char format "x(3)"
    fields NU_NOTA                  as char format "x(12)" 
    fields NU_SERIE_NOTA            as char format "x(3)"
    FIELDS CD_PORTA                 AS INT
    FIELDS CD_TRANSPORTADORA        AS char
    FIELDS CD_CNPJ_TRANSPORTADORA   AS char format "x(20)"
    fields CD_FORNECEDOR	        as integer
    FIELDS CD_CNPJ_FORNECEDOR       AS char format "x(20)"
    fields DT_EMISSAO               as date
    fields CD_SITUACAO              as integer
    fields CD_TIPO_NOTA             as char format "x(3)"
    fields NU_DOC_ERP               as char format "x(30)"
    .

def temp-table INT_E_DET_NOTA_FISCAL no-undo
    fields CD_EMPRESA           as integer
    fields NU_INTERFACE     AS INT64
    fields CD_DEPOSITO          as char format "x(3)"
    fields NU_NOTA              as char format "x(12)" 
    fields NU_SERIE_NOTA        as char format "x(3)"
    fields CD_FORNECEDOR	    as integer
    /*FIELDS CD_CNPJ_FORNECEDOR    AS char format "x(20)"*/
    FIELDS CD_SITUACAO          AS INT
    fields NU_ITEM_CORP         as CHAR FORMAT "x(5)"
    fields CD_PRODUTO           as char format "x(40)"
    fields QT_PRODUTO           as decimal decimals 3
    FIELDS NU_LOTE              AS char format "x(20)"
    FIELDS NU_LOTE_FORNECEDOR   AS char format "x(20)"    .
    
FUNCTION fn_only_numbers RETURNS CHARACTER
    (INPUT cValueString AS CHARACTER):

    DEFINE VARIABLE iPos     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLength  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cData    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDataNew AS CHARACTER NO-UNDO.


    ASSIGN iLength = LENGTH(cValueString).

    DO iPos = 1 TO iLength:
        ASSIGN cData = SUBSTRING(cValueString,iPos,1).
        
        IF cData < '0' OR cData > '9' THEN
            NEXT.

        ASSIGN cDataNew = cDataNew + cData.
    END.

    RETURN cDataNew.
END FUNCTION.    

def var h-api     as handle no-undo.
run esp/eswmapi999.p persistent set h-api.

find first estabelec no-lock 
     where estabelec.cod-estabel = movto-estoq.cod-estabel no-error.
     
/* find item no-lock where */
/*      item.it-codigo = movto-estoq.it-codigo no-error. */
/* if avail item then */
/*     run esp/eswms001.p (buffer item). */

create INT_E_DET_NOTA_FISCAL.
assign INT_E_DET_NOTA_FISCAL.CD_EMPRESA         = if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1
       INT_E_DET_NOTA_FISCAL.CD_DEPOSITO        = movto-estoq.cod-estabel
       INT_E_DET_NOTA_FISCAL.NU_NOTA            = STRING(movto-estoq.nr-trans)
       INT_E_DET_NOTA_FISCAL.NU_SERIE_NOTA      = if movto-estoq.serie-docto = "" then "1" else movto-estoq.serie-docto
       INT_E_DET_NOTA_FISCAL.CD_FORNECEDOR      = estabelec.cod-emitente
       /*INT_E_DET_NOTA_FISCAL.CD_CNPJ_FORNECEDOR  = FN_only_numbers(estabelec.cgc)*/
       INT_E_DET_NOTA_FISCAL.CD_SITUACAO        = movto-estoq.tipo-trans
       INT_E_DET_NOTA_FISCAL.NU_ITEM_CORP       = STRING(movto-estoq.num-sequen)
       INT_E_DET_NOTA_FISCAL.CD_PRODUTO         = movto-estoq.it-codigo
       INT_E_DET_NOTA_FISCAL.QT_PRODUTO         = movto-estoq.quantidade
       INT_E_DET_NOTA_FISCAL.NU_LOTE            = movto-estoq.lote
       INT_E_DET_NOTA_FISCAL.NU_LOTE_FORNECEDOR = movto-estoq.lote.

run pi_insert in h-api (temp-table INT_E_DET_NOTA_FISCAL:default-buffer-handle).
if return-value <> "OK" then do:
    for each esp-fila-wms exclusive-lock
   where esp-fila-wms.cod-trans = "Movimenta‡Æo"
     and esp-fila-wms.chave     = string(movto-estoq.nr-trans):
         
         create esp-erro-wms.
         assign esp-erro-wms.id             = esp-fila-wms.id
                esp-erro-wms.data-hora-erro = now
                esp-erro-wms.mensagem       = return-value.
         
    end.
end.
else
    for each esp-fila-wms exclusive-lock
       where esp-fila-wms.cod-trans = "Movimenta‡Æo"
         and esp-fila-wms.chave     = string(movto-estoq.nr-trans):
         
         if esp-fila-wms.data-hora-integracao = ? then
             assign esp-fila-wms.data-hora-integracao = now.
    end.
    
find transporte no-lock where transporte.cod-transp = 3 no-error.
        

create INT_E_CAB_NOTA_FISCAL.
assign INT_E_CAB_NOTA_FISCAL.CD_EMPRESA             = if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1
       INT_E_CAB_NOTA_FISCAL.CD_DEPOSITO            = movto-estoq.cod-estabel
       INT_E_CAB_NOTA_FISCAL.NU_NOTA                = STRING(movto-estoq.nr-trans)
       INT_E_CAB_NOTA_FISCAL.NU_SERIE_NOTA          = if movto-estoq.serie-docto = "" then "1" else movto-estoq.serie-docto
       INT_E_CAB_NOTA_FISCAL.CD_TRANSPORTADORA      = if movto-estoq.esp-docto = 33 then string(estabelec.cod-emitente) else if avail transporte then "3" else ""
       INT_E_CAB_NOTA_FISCAL.CD_CNPJ_TRANSPORTADORA = if movto-estoq.esp-docto = 33 then FN_only_numbers(estabelec.cgc) else if avail transporte then FN_only_numbers(transporte.cgc) else ""
       INT_E_CAB_NOTA_FISCAL.CD_FORNECEDOR          = estabelec.cod-emitente
       INT_E_CAB_NOTA_FISCAL.CD_CNPJ_FORNECEDOR     = FN_only_numbers(estabelec.cgc)
       INT_E_CAB_NOTA_FISCAL.DT_EMISSAO             = movto-estoq.dt-trans
       INT_E_CAB_NOTA_FISCAL.CD_SITUACAO            = movto-estoq.tipo-trans
       INT_E_CAB_NOTA_FISCAL.CD_TIPO_NOTA           = if movto-estoq.esp-docto = 33 then "TRA" ELSE "ACA"
       INT_E_CAB_NOTA_FISCAL.NU_DOC_ERP             = STRING(i-ep-codigo-usuario) + movto-estoq.cod-estabel + movto-estoq.nro-docto 
                                                                                  + movto-estoq.serie-docto + if movto-estoq.cod-emitente <> ? then STRING(movto-estoq.cod-emitente) else "0".

run pi_insert in h-api (temp-table INT_E_CAB_NOTA_FISCAL:default-buffer-handle).
if return-value <> "OK" then do:
    for each esp-fila-wms exclusive-lock
   where esp-fila-wms.cod-trans = "Movimenta‡Æo"
     and esp-fila-wms.chave     = string(movto-estoq.nr-trans):
         
         create esp-erro-wms.
         assign esp-erro-wms.id             = esp-fila-wms.id
                esp-erro-wms.data-hora-erro = now
                esp-erro-wms.mensagem       = return-value.
         
    end.
end.
else
    for each esp-fila-wms exclusive-lock
       where esp-fila-wms.cod-trans = "Movimenta‡Æo"
         and esp-fila-wms.chave     = string(movto-estoq.nr-trans):
         
         if esp-fila-wms.data-hora-integracao = ? then
             assign esp-fila-wms.data-hora-integracao = now.
    end.

run pi_finalizar in h-api.

delete procedure h-api.
