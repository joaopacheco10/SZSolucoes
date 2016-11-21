/**************************************************************************
**   Programa: esp/eswms006.p
**   Data    : Maio de 2015
**   Autor   : Eder
**   Objetivo: Integraá∆o WMS Nota Fiscais (RE1001)
**   Versao..: 
**************************************************************************/

DEF BUFFER prog_dtsul   FOR emsfnd.prog_dtsul.
DEF BUFFER usuar_mestre FOR emsfnd.usuar_mestre.
      
{include/i-prgvrs.i eswms006 2.11.00.001}
{utp/ut-glob.i}

DEFINE PARAM BUFFER docum-est FOR docum-est. 
DEFINE INPUT PARAM p-acao     AS CHAR no-undo.

DEFINE VARIABLE c-msg AS CHARACTER   NO-UNDO.
DEF NEW GLOBAL SHARED VAR g-integrator AS LOG NO-UNDO.

IF NOT g-integrator THEN DO:
    RUN tools/QueueManager.p ("NFEntrada",
                              STRING(docum-est.cod-emitente) + "|" +
                              docum-est.serie-docto + "|" +
                              docum-est.nro-docto + "|" +
                              docum-est.nat-operacao + "|" +                              
                              p-acao).        
    RETURN "OK".
END.

DEF TEMP-TABLE INT_E_CAB_NOTA_FISCAL NO-UNDO
    FIELDS CD_EMPRESA              AS INTEGER
    FIELDS CD_DEPOSITO             AS CHAR FORMAT "x(3)"
    FIELDS CD_AGENDA               AS CHAR
    FIELDS NU_NOTA                 AS CHAR FORMAT "x(12)" 
    FIELDS NU_SERIE_NOTA           AS CHAR FORMAT "x(3)"
    FIELDS CD_PORTA                AS INT
    FIELDS CD_TRANSPORTADORA       AS CHAR
    /*FIELDS DS_TRANSPORTADORA       AS CHAR*/
    FIELDS CD_CNPJ_TRANSPORTADORA  AS CHAR
    FIELDS CD_FORNECEDOR	       AS INTEGER
    FIELDS CD_CNPJ_FORNECEDOR      AS CHAR FORMAT "x(20)"
    FIELDS DT_EMISSAO              AS DATE
    FIELDS DS_PLACA                AS CHAR
    FIELDS DT_AGENDAMENTO          AS DATE
    FIELDS CD_SITUACAO             AS INTEGER
    FIELDS CD_TIPO_NOTA            AS CHAR FORMAT "x(3)"
    FIELDS NU_DOC_ERP              AS CHAR FORMAT "x(30)"
    /*FIELDS CD_RAV                  AS CHAR*/
    FIELDS DT_ADDROW               AS DATE.

DEF TEMP-TABLE INT_E_DET_NOTA_FISCAL NO-UNDO
    FIELDS CD_EMPRESA          AS INTEGER
    FIELDS CD_DEPOSITO         AS CHAR FORMAT "x(3)"
    FIELDS CD_CLIENTE          AS CHAR
    FIELDS CD_AGENDA           AS CHAR
    FIELDS NU_NOTA             AS CHAR FORMAT "x(12)" 
    FIELDS NU_SERIE_NOTA       AS CHAR format "x(3)"
    FIELDS CD_FORNECEDOR	   AS INTEGER
    /*FIELDS CD_CGC_FORNECEDOR   AS CHAR*/
    FIELDS CD_SITUACAO         AS INTEGER
    FIELDS NU_ITEM_CORP        AS INTEGER
    FIELDS CD_PRODUTO          AS CHAR FORMAT "x(40)"
    FIELDS QT_PRODUTO          AS DECIMAL DECIMALS 3
    FIELDS NU_LOTE             AS CHAR
    FIELDS NU_LOTE_FORNECEDOR  AS CHAR
    FIELDS DT_FABRICACAO       AS DATE
    FIELDS DT_ADDROW           AS DATE.

DEF VAR h-api     AS HANDLE NO-UNDO.
DEF VAR c-tp-nota AS CHAR NO-UNDO.

FOR FIRST emitente NO-LOCK
   WHERE emitente.cod-emitente = docum-est.cod-emitente:
END.

FOR FIRST natur-oper NO-LOCK
    where natur-oper.nat-operacao = docum-est.nat-operacao:
END.

/*

Notas fiscais de transferància (NFT no EMS) devem ser classificadas com o tipo de nota fiscal no WIS igual a 'TRA'
Notas fiscais de devoluá∆o (NFD no EMS) e que tàm o dep¢sito de entrada =CEP devem ser classificadas com o tipo de nota fiscal no WIS igual a 'DEV'.
 Importante: Todas as outras NFD n∆o devem vir para o WIS.
Notas fiscais de entrada normal (NFE no EMS) devem ser classificadas com o tipo de nota fiscal no WIS igual a 'FOR'
*/

IF natur-oper.tipo-compra = 3 THEN DO:

    ASSIGN c-tp-nota = "DEV".
END.
ELSE
    IF natur-oper.transf THEN
        ASSIGN c-tp-nota = "TRA".
    ELSE 
        ASSIGN c-tp-nota = "FOR".    
        
find first transporte where
    transporte.nome-abrev = docum-est.nome-transp no-lock no-error.                  
       
RUN esp/eswmapi999.p PERSISTENT SET h-api.

FIND esp-param-integr-wms NO-LOCK NO-ERROR.    

FOR EACH item-doc-est NO-LOCK OF docum-est,
   FIRST ITEM NO-LOCK 
   WHERE ITEM.it-codigo = item-doc-est.it-codigo:
   
    if (item-doc-est.nat-operacao = "1102"  or
        item-doc-est.nat-operacao = "2102"  or
        item-doc-est.nat-operacao = "3102") and
        item.fm-cod-com           = "711"   then
        next.   
      
    if item.politica = 6 then do:
    
        IF NOT CAN-DO(esp-param-integr-wms.lista-depos-integr,item-doc-est.cod-depos) THEN NEXT .
              
        CREATE INT_E_DET_NOTA_FISCAL.
        ASSIGN INT_E_DET_NOTA_FISCAL.CD_EMPRESA          = if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1
               INT_E_DET_NOTA_FISCAL.CD_DEPOSITO         = docum-est.cod-estabel
               INT_E_DET_NOTA_FISCAL.CD_CLIENTE          = ""
               INT_E_DET_NOTA_FISCAL.CD_AGENDA           = ""
               INT_E_DET_NOTA_FISCAL.NU_NOTA             = docum-est.nro-docto
               INT_E_DET_NOTA_FISCAL.NU_SERIE_NOTA       = if docum-est.serie-docto <> "" then docum-est.serie-docto else "0"
               INT_E_DET_NOTA_FISCAL.CD_FORNECEDOR       = docum-est.cod-emitente 
               /*INT_E_DET_NOTA_FISCAL.CD_CGC_FORNECEDOR   = emitente.cgc*/
               INT_E_DET_NOTA_FISCAL.CD_SITUACAO         = IF p-acao = "atualiza" THEN 1 ELSE 2
               INT_E_DET_NOTA_FISCAL.NU_ITEM_CORP        = item-doc-est.sequencia
               INT_E_DET_NOTA_FISCAL.CD_PRODUTO          = item-doc-est.it-codigo
               INT_E_DET_NOTA_FISCAL.QT_PRODUTO          = item-doc-est.quantidade
               INT_E_DET_NOTA_FISCAL.NU_LOTE             = item-doc-est.lote
               INT_E_DET_NOTA_FISCAL.NU_LOTE_FORNECEDOR  = ""
               INT_E_DET_NOTA_FISCAL.DT_FABRICACAO       = today /*verificar*/
               INT_E_DET_NOTA_FISCAL.DT_ADDROW           = today.                                 
                        
    
        /*RUN esp/eswms001.p (BUFFER ITEM).  */              
    
        RUN pi_insert IN h-api (TEMP-TABLE INT_E_DET_NOTA_FISCAL:DEFAULT-BUFFER-HANDLE).
        IF RETURN-VALUE <> "OK" THEN DO:
            FOR EACH esp-fila-wms EXCLUSIVE-LOCK
               WHERE esp-fila-wms.cod-trans = "NFEntrada"
                 AND esp-fila-wms.chave     = STRING(docum-est.cod-emitente) + "|" +
                                              docum-est.serie-docto + "|" +
                                              docum-est.nro-docto + "|" +
                                              docum-est.nat-operacao + "|" +                              
                                              p-acao:
                                                           
                CREATE esp-erro-wms.
                ASSIGN esp-erro-wms.id             = esp-fila-wms.id
                       esp-erro-wms.data-hora-erro = NOW
                       esp-erro-wms.mensagem       = RETURN-VALUE.
                       
                RUN pi_finalizar IN h-api.
                DELETE PROCEDURE h-api.                  
                        
                RETURN.                    
                   
            END.
        END.
        ELSE
            FOR EACH esp-fila-wms EXCLUSIVE-LOCK
               WHERE esp-fila-wms.cod-trans = "NFEntrada"
                 AND esp-fila-wms.chave     = STRING(docum-est.cod-emitente) + "|" +
                                              docum-est.serie-docto + "|" +
                                              docum-est.nro-docto + "|" +
                                              docum-est.nat-operacao + "|" +                              
                                              p-acao:
                 IF esp-fila-wms.data-hora-integracao = ? THEN
                     ASSIGN esp-fila-wms.data-hora-integracao = NOW.
            END.    

       
    end.
    else do:    
   
        for EACH rat-lote OF item-doc-est NO-LOCK:
        
            IF NOT CAN-DO(esp-param-integr-wms.lista-depos-integr,rat-lote.cod-depos) THEN NEXT .
              
            CREATE INT_E_DET_NOTA_FISCAL.
            ASSIGN INT_E_DET_NOTA_FISCAL.CD_EMPRESA          = if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1
                   INT_E_DET_NOTA_FISCAL.CD_DEPOSITO         = docum-est.cod-estabel
                   INT_E_DET_NOTA_FISCAL.CD_CLIENTE          = ""
                   INT_E_DET_NOTA_FISCAL.CD_AGENDA           = ""
                   INT_E_DET_NOTA_FISCAL.NU_NOTA             = docum-est.nro-docto
                   INT_E_DET_NOTA_FISCAL.NU_SERIE_NOTA       = if docum-est.serie-docto <> "" then docum-est.serie-docto else "0"
                   INT_E_DET_NOTA_FISCAL.CD_FORNECEDOR       = docum-est.cod-emitente 
                   /*INT_E_DET_NOTA_FISCAL.CD_CGC_FORNECEDOR   = emitente.cgc*/
                   INT_E_DET_NOTA_FISCAL.CD_SITUACAO         = IF p-acao = "atualiza" THEN 1 ELSE 2
                   INT_E_DET_NOTA_FISCAL.NU_ITEM_CORP        = item-doc-est.sequencia
                   INT_E_DET_NOTA_FISCAL.CD_PRODUTO          = item-doc-est.it-codigo
                   INT_E_DET_NOTA_FISCAL.QT_PRODUTO          = rat-lote.quantidade
                   INT_E_DET_NOTA_FISCAL.NU_LOTE             = rat-lote.lote
                   INT_E_DET_NOTA_FISCAL.NU_LOTE_FORNECEDOR  = ""
                   INT_E_DET_NOTA_FISCAL.DT_FABRICACAO       = today /*verificar*/
                   INT_E_DET_NOTA_FISCAL.DT_ADDROW           = today.                                 
                            
        
/*            RUN esp/eswms001.p (BUFFER ITEM).                   */
        
            RUN pi_insert IN h-api (TEMP-TABLE INT_E_DET_NOTA_FISCAL:DEFAULT-BUFFER-HANDLE).
            IF RETURN-VALUE <> "OK" THEN DO:
                FOR EACH esp-fila-wms EXCLUSIVE-LOCK
                   WHERE esp-fila-wms.cod-trans = "NFEntrada"
                     AND esp-fila-wms.chave     = STRING(docum-est.cod-emitente) + "|" +
                                                  docum-est.serie-docto + "|" +
                                                  docum-est.nro-docto + "|" +
                                                  docum-est.nat-operacao + "|" +                              
                                                  p-acao:
                                                               
                    CREATE esp-erro-wms.
                    ASSIGN esp-erro-wms.id             = esp-fila-wms.id
                           esp-erro-wms.data-hora-erro = NOW
                           esp-erro-wms.mensagem       = RETURN-VALUE.
                           
                    RUN pi_finalizar IN h-api.
                    DELETE PROCEDURE h-api.                  
                            
                    RETURN.                    
                       
                END.
            END.
            ELSE
                FOR EACH esp-fila-wms EXCLUSIVE-LOCK
                   WHERE esp-fila-wms.cod-trans = "NFEntrada"
                     AND esp-fila-wms.chave     = STRING(docum-est.cod-emitente) + "|" +
                                                  docum-est.serie-docto + "|" +
                                                  docum-est.nro-docto + "|" +
                                                  docum-est.nat-operacao + "|" +                              
                                                  p-acao:
                     IF esp-fila-wms.data-hora-integracao = ? THEN
                         ASSIGN esp-fila-wms.data-hora-integracao = NOW.
                END.    
        end.        
    end.        
END.

if can-find(first INT_E_DET_NOTA_FISCAL) then do:

    RUN esp/eswms002.p (BUFFER emitente, 2).

    CREATE INT_E_CAB_NOTA_FISCAL.
    ASSIGN INT_E_CAB_NOTA_FISCAL.CD_EMPRESA             = if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1
           INT_E_CAB_NOTA_FISCAL.CD_DEPOSITO            = docum-est.cod-estabel
           INT_E_CAB_NOTA_FISCAL.CD_AGENDA              = ""
           INT_E_CAB_NOTA_FISCAL.NU_NOTA                = docum-est.nro-docto
           INT_E_CAB_NOTA_FISCAL.NU_SERIE_NOTA          = if docum-est.serie-docto <> "" then docum-est.serie-docto else "0"
           INT_E_CAB_NOTA_FISCAL.CD_PORTA               = 0
           INT_E_CAB_NOTA_FISCAL.CD_TRANSPORTADORA      = if avail transporte then string(transporte.cod-transp) else "NC"
           /*INT_E_CAB_NOTA_FISCAL.DS_TRANSPORTADORA      = docum-est.nome-transp*/
           INT_E_CAB_NOTA_FISCAL.CD_CNPJ_TRANSPORTADORA = ""
           INT_E_CAB_NOTA_FISCAL.CD_FORNECEDOR          = docum-est.cod-emitente 
           INT_E_CAB_NOTA_FISCAL.CD_CNPJ_FORNECEDOR     = emitente.cgc
           INT_E_CAB_NOTA_FISCAL.DT_EMISSAO             = docum-est.dt-emissao
           INT_E_CAB_NOTA_FISCAL.DS_PLACA               = ""
           INT_E_CAB_NOTA_FISCAL.DT_AGENDAMENTO         = today /*verificar*/
           INT_E_CAB_NOTA_FISCAL.CD_SITUACAO            = IF p-acao = "atualiza" THEN 1 ELSE 2
           INT_E_CAB_NOTA_FISCAL.CD_TIPO_NOTA           = c-tp-nota
           INT_E_CAB_NOTA_FISCAL.NU_DOC_ERP             = STRING(i-ep-codigo-usuario) + docum-est.cod-estabel + docum-est.nro-docto + docum-est.serie-docto
           /*INT_E_CAB_NOTA_FISCAL.CD_RAV                 = ""*/
           INT_E_CAB_NOTA_FISCAL.DT_ADDROW              = today.   
           
    RUN pi_insert IN h-api (TEMP-TABLE INT_E_CAB_NOTA_FISCAL:DEFAULT-BUFFER-HANDLE).
    c-msg = RETURN-VALUE.
    IF c-msg <> "OK" THEN DO:
    
        FOR EACH esp-fila-wms EXCLUSIVE-LOCK
           WHERE esp-fila-wms.cod-trans = "NFEntrada"
             AND esp-fila-wms.chave     = STRING(docum-est.cod-emitente) + "|" +
                                          docum-est.serie-docto + "|" +
                                          docum-est.nro-docto + "|" +
                                          docum-est.nat-operacao + "|" +                              
                                          p-acao:
             
             CREATE esp-erro-wms.
             ASSIGN esp-erro-wms.id             = esp-fila-wms.id
                    esp-erro-wms.data-hora-erro = NOW
                    esp-erro-wms.mensagem       = c-msg
                    esp-fila-wms.data-hora-integracao = ?.
             
        END.
    END.
    ELSE
        FOR EACH esp-fila-wms EXCLUSIVE-LOCK
           WHERE esp-fila-wms.cod-trans = "NFEntrada"
             AND esp-fila-wms.chave     = STRING(docum-est.cod-emitente) + "|" +
                                          docum-est.serie-docto + "|" +
                                          docum-est.nro-docto + "|" +
                                          docum-est.nat-operacao + "|" +                              
                                          p-acao:
    
             IF esp-fila-wms.data-hora-integracao = ? THEN
                 ASSIGN esp-fila-wms.data-hora-integracao = NOW.
        END.    
end.    

RUN pi_finalizar IN h-api.
    
DELETE PROCEDURE h-api.
