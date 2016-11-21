/**************************************************************************
**   Programa: esp/eswms003.p
**   Data    : Dezembro de 2009
**   Autor   : Eder
**   Objetivo: Integra‡Æo WMS Transportadoras
**   Versao..: 
**************************************************************************/

DEF BUFFER prog_dtsul   FOR emsfnd.prog_dtsul.
DEF BUFFER usuar_mestre FOR emsfnd.usuar_mestre.
      
{include/i-prgvrs.i eswms003.p 2.06.00.000}
{utp/ut-glob.i}

DEF TEMP-TABLE INT_E_TRANSPORTADORA NO-UNDO
    FIELDS CD_EMPRESA             AS INTEGER
    FIELDS CD_DEPOSITO            AS CHAR
    FIELDS CD_TRANSPORTADORA      AS INTEGER 
    FIELDS DS_TRANSPORTADORA      AS CHAR FORMAT "x(100)"
    FIELDS NU_INSCRICAO           AS INT64
    FIELDS CD_CNPJ_TRANSPORTADORA AS CHAR FORMAT "x(20)"
    FIELDS CD_CEP	              AS INTEGER /*CEP*/
    FIELDS DS_ENDERECO	          AS CHAR FORMAT "x(100)"
    FIELDS NU_ENDERECO            AS CHAR
    FIELDS DS_COMPLEMENTO         AS CHAR
    FIELDS DS_BAIRRO              AS CHAR
    FIELDS DS_MUNICIPIO	          AS CHAR FORMAT "x(30)"
    FIELDS CD_UF	              AS CHAR FORMAT "x(5)"
    FIELDS NU_TELEFONE            AS INT64
    FIELDS NU_FAX                 AS INT64
    FIELDS CD_SITUACAO	          AS INTEGER
    FIELDS DT_ADDROW              AS DATE.
     
    
DEF VAR c-sql AS CHAR   NO-UNDO.
DEF VAR h-api AS HANDLE NO-UNDO.

DEFINE PARAM BUFFER transporte FOR transporte.
DEF NEW GLOBAL SHARED VAR g-integrator AS LOG NO-UNDO.

IF NOT g-integrator THEN DO:
    RUN tools/QueueManager.p ("Transportadora",STRING(transporte.cod-transp)).        
    RETURN "OK".
END.

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

DEFINE VARIABLE c-rua AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-nro AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-comp AS CHARACTER   NO-UNDO.
DEFINE VARIABLE h-cdapi704 AS HANDLE      NO-UNDO.

/*Metodo que busca o logradouro e o numero*/
/*IF NOT VALID-HANDLE(h-cdapi704) THEN
    RUN cdp/cdapi704.p PERSISTENT SET h-cdapi704.
RUN pi-trata-endereco IN h-cdapi704 (INPUT transporte.endereco, OUTPUT c-rua, OUTPUT c-nro, OUTPUT c-comp).*/

CREATE INT_E_TRANSPORTADORA.
ASSIGN INT_E_TRANSPORTADORA.CD_EMPRESA             = if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1
       INT_E_TRANSPORTADORA.CD_DEPOSITO            = "001" /*verificar*/
       INT_E_TRANSPORTADORA.CD_TRANSPORTADORA      = transporte.cod-transp
       INT_E_TRANSPORTADORA.DS_TRANSPORTADORA      = if transporte.nome <> "" then SUBSTRING(transporte.nome,1,100) else "NAO CADASTRADO"
       INT_E_TRANSPORTADORA.NU_INSCRICAO           = INT64(FN_only_numbers(transporte.ins-estadual))
       INT_E_TRANSPORTADORA.CD_CNPJ_TRANSPORTADORA = string(FN_only_numbers(transporte.cgc))
       INT_E_TRANSPORTADORA.CD_CEP                 = INTEGER(fn_only_numbers(transporte.cep))
       INT_E_TRANSPORTADORA.DS_ENDERECO            = c-rua + "," + c-nro
       INT_E_TRANSPORTADORA.NU_ENDERECO            = c-nro
       INT_E_TRANSPORTADORA.DS_COMPLEMENTO         = c-comp
       INT_E_TRANSPORTADORA.DS_BAIRRO              = transporte.bairro
       INT_E_TRANSPORTADORA.DS_MUNICIPIO           = transporte.cidade
       INT_E_TRANSPORTADORA.CD_UF                  = transporte.estado
       INT_E_TRANSPORTADORA.NU_TELEFONE            = INT64(FN_only_numbers(transporte.telefone))
       INT_E_TRANSPORTADORA.NU_FAX                 = INT64(FN_only_numbers(transporte.telefax))    
       INT_E_TRANSPORTADORA.CD_SITUACAO            = 15
       INT_E_TRANSPORTADORA.DT_ADDROW              = today.

RUN esp/eswmapi999.p PERSISTENT SET h-api.

RUN pi_insert IN h-api (TEMP-TABLE INT_E_TRANSPORTADORA:DEFAULT-BUFFER-HANDLE).

IF RETURN-VALUE <> "OK" THEN DO:
    FOR EACH esp-fila-wms EXCLUSIVE-LOCK
       WHERE esp-fila-wms.cod-trans = "Transportadora"
         AND esp-fila-wms.chave     = STRING(transporte.cod-transp):
         
         CREATE esp-erro-wms.
         ASSIGN esp-erro-wms.id             = esp-fila-wms.id
                esp-erro-wms.data-hora-erro = NOW
                esp-erro-wms.mensagem       = RETURN-VALUE.
         
    END.
END.
ELSE
    FOR EACH esp-fila-wms EXCLUSIVE-LOCK
       WHERE esp-fila-wms.cod-trans = "Transportadora"
         AND esp-fila-wms.chave     = STRING(transporte.cod-transp):
         IF esp-fila-wms.data-hora-integracao = ? THEN
             ASSIGN esp-fila-wms.data-hora-integracao = NOW.
    END.

RUN pi_finalizar IN h-api.

DELETE PROCEDURE h-api.
