/**************************************************************************
**   Programa: esp/eswms002.p
**   Data    : Dezembro de 2009
**   Autor   : Eder
**   Objetivo: Integra‡Æo WMS Clientes/Fornecedores
**   Versao..: 
**************************************************************************/
DEF BUFFER prog_dtsul   FOR emsfnd.prog_dtsul.
DEF BUFFER usuar_mestre FOR emsfnd.usuar_mestre.
      
{include/i-prgvrs.i eswms002.p 2.11.00.001}
{utp/ut-glob.i}

DEFINE PARAM BUFFER emitente FOR emitente. 
DEFINE INPUT PARAM p-tipo AS INTEGER NO-UNDO.

DEF NEW GLOBAL SHARED VAR g-integrator AS LOG NO-UNDO.

IF NOT g-integrator THEN DO:
    RUN tools/QueueManager.p ("Emitente",STRING(emitente.cod-emitente) + "|" + STRING(p-tipo)).        
    RETURN "OK".
END.

DEF TEMP-TABLE INT_E_CLIENTE NO-UNDO
    FIELDS CD_EMPRESA         AS INTEGER FORMAT ">>>9"
    FIELDS CD_DEPOSITO        AS CHAR FORMAT "xxx" /*Estabelec*/
    FIELDS CD_CLIENTE         AS INTEGER
    FIELDS DS_CLIENTE         AS CHAR FORMAT "x(100)"
    FIELDS NU_INSCRICAO       AS INT64
    FIELDS CD_CNPJ_CLIENTE    AS INT64
    FIELDS CD_CEP             AS INT
    FIELDS DS_ENDERECO        AS CHAR
    FIELDS NU_ENDERECO        AS CHAR
    FIELDS DS_COMPLEMENTO     AS CHAR
    FIELDS DS_BAIRRO          AS CHAR
    FIELDS DS_MUNICIPIO       AS CHAR
    FIELDS CD_UF              AS CHAR
    FIELDS NU_TELEFONE        AS INT64
    FIELDS NU_FAX             AS INT64
    FIELDS CD_ROTA            AS INT
    FIELDS ID_CLIENTE_FILIAL  AS CHAR
    FIELDS CD_SITUACAO        AS INT
    FIELDS DT_ADDROW          AS date
    FIELDS ID_PROCESSADO      AS CHAR
    FIELDS DT_PROCESSADO      AS DATE.

DEF TEMP-TABLE INT_E_FORNECEDOR NO-UNDO
    FIELDS CD_EMPRESA           AS INT FORMAT ">>>9"
    FIELDS CD_DEPOSITO          AS CHAR FORMAT "xxx" /*Estabelec*/
    FIELDS CD_FORNECEDOR        AS INT
    FIELDS DS_RAZAO_SOCIAL      AS CHAR FORMAT "x(100)"
    FIELDS NM_FANTASIA          AS CHAR
    FIELDS CD_CNPJ_FORNECEDOR   AS INT64
    FIELDS NU_INSCRICAO	        AS INT64
    FIELDS DS_ENDERECO          AS CHAR
    FIELDS DS_BAIRRO            AS CHAR
    FIELDS DS_MUNICIPIO         AS CHAR
    FIELDS CD_UF	            AS CHAR FORMAT "x(5)"
    FIELDS CD_CEP	            AS INT /*CEP*/
    FIELDS NU_TELEFONE          AS INT64
    FIELDS NU_FAX               AS INT64
    FIELDS DT_ADDROW            AS date
    FIELDS ID_PROCESSADO        AS CHAR
    FIELDS DT_PROCESSADO        AS DATE.

DEF VAR c-sql AS CHAR   NO-UNDO.
DEF VAR h-api AS HANDLE NO-UNDO.
DEF VAR h-bf  AS HANDLE NO-UNDO.

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

/*/*Metodo que busca o logradouro e o numero*/
IF NOT VALID-HANDLE(h-cdapi704) THEN
    RUN cdp/cdapi704.p PERSISTENT SET h-cdapi704.
RUN pi-trata-endereco IN h-cdapi704 (INPUT emitente.endereco, OUTPUT c-rua, OUTPUT c-nro, OUTPUT c-comp).

DELETE PROCEDURE h-cdapi704.
ASSIGN h-cdapi704 = ?.*/

for first emit-depos where
    emit-depos.cod-emitente = emitente.cod-emitente no-lock: 
end.

IF p-tipo = 1 THEN DO: /*Cliente*/
    CREATE INT_E_CLIENTE.
    ASSIGN INT_E_CLIENTE.CD_EMPRESA        = if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1
           INT_E_CLIENTE.CD_DEPOSITO       = "001" /*verificar*/
           INT_E_CLIENTE.CD_CLIENTE        = emitente.cod-emitente
           INT_E_CLIENTE.DS_CLIENTE        = SUBSTRING(emitente.nome-emit,1,100)
           INT_E_CLIENTE.NU_INSCRICAO      = INT64(FN_only_numbers(emitente.ins-estadual))
           INT_E_CLIENTE.CD_CNPJ_CLIENTE   = INT64(FN_only_numbers(emitente.cgc))
           INT_E_CLIENTE.CD_CEP            = INTEGER(fn_only_numbers(emitente.cep))
           INT_E_CLIENTE.DS_ENDERECO       = c-rua + "," + c-nro
           INT_E_CLIENTE.NU_ENDERECO       = c-nro
           INT_E_CLIENTE.DS_COMPLEMENTO    = c-comp
           INT_E_CLIENTE.DS_BAIRRO         = emitente.bairro
           INT_E_CLIENTE.DS_MUNICIPIO      = emitente.cidade
           INT_E_CLIENTE.CD_UF             = emitente.estado
           INT_E_CLIENTE.NU_TELEFONE       = INT64(FN_only_numbers(emitente.telefone[1]))
           INT_E_CLIENTE.NU_FAX            = INT64(FN_only_numbers(emitente.telefax))   
           INT_E_CLIENTE.CD_ROTA           = 0
           INT_E_CLIENTE.ID_CLIENTE_FILIAL = "N"
           INT_E_CLIENTE.CD_SITUACAO       = 15
           INT_E_CLIENTE.DT_ADDROW         = today
           INT_E_CLIENTE.ID_PROCESSADO     = "N"
           INT_E_CLIENTE.DT_PROCESSADO     = today.

    ASSIGN h-bf = TEMP-TABLE INT_E_CLIENTE:DEFAULT-BUFFER-HANDLE.
END.
ELSE DO: /*Fornecedor*/
    
    CREATE INT_E_FORNECEDOR.
    ASSIGN INT_E_FORNECEDOR.CD_EMPRESA         = if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1
           INT_E_FORNECEDOR.CD_DEPOSITO        = "001" /*verificar*/
           INT_E_FORNECEDOR.CD_FORNECEDOR      = emitente.cod-emitente
           INT_E_FORNECEDOR.DS_RAZAO_SOCIAL    = SUBSTRING(emitente.nome-emit,1,100)
           INT_E_FORNECEDOR.NM_FANTASIA        = ""
           INT_E_FORNECEDOR.CD_CNPJ_FORNECEDOR = INT64(FN_only_numbers(emitente.cgc))
           INT_E_FORNECEDOR.NU_INSCRICAO       = INT64(FN_only_numbers(emitente.ins-estadual))
           INT_E_FORNECEDOR.DS_ENDERECO        = c-rua + "," + c-nro
           INT_E_FORNECEDOR.DS_BAIRRO          = emitente.bairro
           INT_E_FORNECEDOR.DS_MUNICIPIO       = emitente.cidade
           INT_E_FORNECEDOR.CD_UF              = emitente.estado
           INT_E_FORNECEDOR.CD_CEP             = INTEGER(fn_only_numbers(emitente.cep))
           INT_E_FORNECEDOR.NU_TELEFONE        = INT64(FN_only_numbers(emitente.telefone[1]))
           INT_E_FORNECEDOR.NU_FAX             = INT64(FN_only_numbers(emitente.telefax))    
/*           INT_E_FORNECEDOR.CD_SITUACAO        = 15  */
           INT_E_FORNECEDOR.DT_ADDROW          = today 
           INT_E_FORNECEDOR.ID_PROCESSADO      = "N" 
           INT_E_FORNECEDOR.DT_PROCESSADO      = today.  

    ASSIGN h-bf = TEMP-TABLE INT_E_FORNECEDOR:DEFAULT-BUFFER-HANDLE.
END.

RUN esp/eswmapi999.p PERSISTENT SET h-api.

RUN pi_insert IN h-api (h-bf).
IF RETURN-VALUE <> "OK" THEN DO:
    FOR EACH esp-fila-wms EXCLUSIVE-LOCK
       WHERE esp-fila-wms.cod-trans = "Emitente"
         AND esp-fila-wms.chave     = STRING(emitente.cod-emitente) + "|" + STRING(p-tipo):
         
         CREATE esp-erro-wms.
         ASSIGN esp-erro-wms.id             = esp-fila-wms.id
                esp-erro-wms.data-hora-erro = NOW
                esp-erro-wms.mensagem       = RETURN-VALUE.
         
    END.
END.
ELSE
    FOR EACH esp-fila-wms EXCLUSIVE-LOCK
       WHERE esp-fila-wms.cod-trans = "Emitente"
         AND esp-fila-wms.chave     = STRING(emitente.cod-emitente) + "|" + STRING(p-tipo):
         IF esp-fila-wms.data-hora-integracao = ? THEN
             ASSIGN esp-fila-wms.data-hora-integracao = NOW.
    END.

RUN pi_finalizar IN h-api.

DELETE PROCEDURE h-api.
