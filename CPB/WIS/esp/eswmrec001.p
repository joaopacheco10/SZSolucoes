/**************************************************************************
**   Programa: esp/eswmrec001.p
**   Data    : Dezembro de 2009
**   Autor   : Eder
**   Objetivo: Recebimento WMS - Executado como servi?o do Windows
**   Versao..: 
**************************************************************************/
DEF BUFFER prog_dtsul   for emsfnd.prog_dtsul.
DEF BUFFER usuar_mestre for emsfnd.usuar_mestre.

{include/i-prgvrs.i eswmrec001.p 2.11.00.001}

/* DEFINE NEW GLOBAL SHARED VAR h-btb432za AS HANDLE      NO-UNDO. */
DEF NEW GLOBAL SHARED VAR g-integrator AS LOG INIT YES NO-UNDO.

SESSION:DEBUG-ALERT = TRUE.

DEF VAR h-login-integr AS HANDLE  NO-UNDO.
DEF VAR l-login-integr AS LOGICAL NO-UNDO.

DEFINE VARIABLE c-usuario AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-senha   AS CHARACTER   NO-UNDO.

DEF VAR c-arq-log-wms  AS CHAR    NO-UNDO.

DEFINE TEMP-TABLE tt-erros
    FIELD cod-erro  AS INTEGER
    FIELD desc-erro AS CHARACTER FORMAT "x(256)"
    FIELD desc-arq  AS CHARACTER.

RUN criaAlias.

/* if not valid-handle (h-btb432za) then             */
/*     RUN btb/btb432za.p PERSISTENT SET h-btb432za. */
    
/*if not session:batch-mode then
    return.    */
    
/*log-manager:logfile-name    = "c:\temp\log_integrador.txt".
log-manager:logging-level   = 4.
log-manager:log-entry-types = "4GLTrace".*/
    
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

/* def temp-table tt-usuar-mestre-ext no-undo */
/*     like usuar-mestre-ext.                 */

DEF NEW SHARED STREAM s-log.

/*assign c-arq-log-wms = "LOG_WMS_" +
                   string(year(today),"9999") +
                   string(month(today),"99") + 
                   string(day(today),"99") +
                   replace(string(time,"hh:mm:ss"),":","") + 
                   ".txt".*/

ASSIGN c-arq-log-wms = "LOG_WMS_" +
                   STRING(YEAR(TODAY),"9999") +
                   STRING(MONTH(TODAY),"99") + 
                   STRING(DAY(TODAY),"99") +                    
                   ".txt" .                   
/**/
OUTPUT STREAM s-log TO VALUE(SESSION:TEMP-DIRECTORY + c-arq-log-wms) KEEP-MESSAGES UNBUFFERED
    CONVERT TARGET "iso8859-1".

/* output stream s-log to value(session:temp-dir + "~\" + c-arq-log-wms) keep-messages unbuffered */

/*Usu rio e senha sÆo informados no parametro de carga da sessÆo -param posi‡äes 4 e 5 */
IF c-seg-usuario = "" THEN DO:
    RUN btb/btapi910za.p (ENTRY(4,SESSION:PARAMETER),ENTRY(5,SESSION:PARAMETER),OUTPUT TABLE tt-erros).
    FIND FIRST tt-erros NO-ERROR.
    IF AVAIL tt-erros THEN DO:
        PUT STREAM s-log UNFORMATTED
                "# " STRING(TIME,"hh:mm:ss") 
                " - " tt-erros.desc-erro SKIP
                " - Erro ao efetuar o login no EMS - A integra‡Æo nÆo ser  iniciada." SKIP.
        OUTPUT STREAM s-log CLOSE.
        QUIT.
    END.
END.    

FIND FIRST esp-param-integr-wms NO-LOCK NO-ERROR.

DEFAULT-WINDOW:HIDDEN             = NO.
DEFAULT-WINDOW:HEIGHT             = 3.
DEFAULT-WINDOW:WIDTH              = 70.
DEFAULT-WINDOW:BGCOLOR            = 3.
DEFAULT-WINDOW:FONT               = 1.
DEFAULT-WINDOW:TITLE              = "Processamento INTEGRACAO ".

main_block:
REPEAT ON ERROR UNDO , RETRY
       ON STOP UNDO  , LEAVE
       ON ENDKEY UNDO, RETRY
       ON QUIT UNDO  , LEAVE:

    ASSIGN g-integrator = YES.
    
    MESSAGE " " .
    MESSAGE "Processando Integracao... " .
    
    DEFAULT-WINDOW:TITLE = 'Processando Integracao... '.
    
    SESSION:SET-WAIT-STATE("GENERAL":U).
    RUN pi-envia.

   /* /*Recebimento de Itens*/ /* nÆo utiliza */ 
    run esp/eswme001.p. */

    /*Recebimento de Ajustes de Estoque */
    run esp/eswme004.p.

    
    /*Recebimento Confirma?’o de Alocacao*/
    run esp/eswme007.p.

    /*Enviar nr da nota fiscal*/
    run pi-nota-fiscal.

    /*run esp/eswme016.p.*/

   /* run esp/eswms007b.p. VERIFICAR SER PRECISA MUDAR O PROGRAMA PARA SER CHAMADO*/

    PUT STREAM s-log UNFORMATTED
            "- " STRING(TIME,"hh:mm:ss") 
            " - Processando interface --> passou" SKIP.

    
    SESSION:SET-WAIT-STATE("":U).

    MESSAGE " " .
    MESSAGE "Aguardando !!! ... Integracao Automatica. " .
    
    DEFAULT-WINDOW:TITLE = 'Aguardando !!! ... Integracao Automatica. '.


    /*PAUSE esp-param-integr-wms.interv-proces NO-MESSAGE. */

    leave.
   
END.

OUTPUT STREAM s-log CLOSE.

QUIT.

/*
procedure pi-efetua-login:

    run  btb/btapi910zb.p persisten set h-login-integr (input 1).
    
    run pi_Login_Integr_Habilitado in h-login-integr ( output l-login-integr ).
    
    if l-login-integr then do:
        run pi_localiza_usuario_so in h-login-integr ( input "",
                                                       input "",
                                                       input-output table tt-usuar-mestre-ext).
    
        find first tt-usuar-mestre-ext no-lock no-error.
        if  not avail tt-usuar-mestre-ext then do:
            return "NOK".
        end.
    
        if not tt-usuar-mestre-ext.log-ativ then do:
            return "NOK".
        end.
    
        run pi_efetua_login in h-login-integr ( input-output table tt-usuar-mestre-ext).
        if return-value = 'NOK':U then
            return "NOK".
    end.

    delete procedure h-login-integr.

    return "OK".
end.
*/

PROCEDURE pi-nota-fiscal:
    
    FOR EACH esp-resumo-wms WHERE
        esp-resumo-wms.confirmado NO-LOCK,
        EACH nota-fiscal WHERE 
        nota-fiscal.cdd-embarq = esp-resumo-wms.nr-embarque AND
        nota-fisca.nr-resumo   = esp-resumo-wms.nr-resumo NO-LOCK:
        
        if not avail esp-param-integr-wms or 
           not can-do(esp-param-integr-wms.lista-estab-integr,nota-fiscal.cod-estabel) then
            next.
        
        run ClearReturnValue.
        
        find esp-nota-fiscal-wms of nota-fiscal no-lock no-error.
        if not avail esp-nota-fiscal-wms or not esp-nota-fiscal-wms.enviada then
            run esp/eswms007a.p (buffer nota-fiscal).
        else 
            next.
        
        if return-value = "OK" then do:
            find current esp-nota-fiscal-wms exclusive-lock no-error.
            if not avail esp-nota-fiscal-wms then do:
                create esp-nota-fiscal-wms.
                assign esp-nota-fiscal-wms.cod-estabel = nota-fiscal.cod-estabel
                       esp-nota-fiscal-wms.serie       = nota-fiscal.serie
                       esp-nota-fiscal-wms.nr-nota-fis = nota-fiscal.nr-nota-fis.
            end.
        
            assign esp-nota-fiscal-wms.enviada = yes.
        end.
            
        
    end.
end.

PROCEDURE ClearReturnValue:
    RETURN "".
END.

PROCEDURE pi-envia:

    def var i-nr-aloc as integer init 0 no-undo.

    FOR EACH esp-fila-wms EXCLUSIVE-LOCK
       WHERE esp-fila-wms.data-hora-integracao = ? 
          BY esp-fila-wms.id:
          
          
        PUT STREAM s-log UNFORMATTED
            "- " STRING(TIME,"hh:mm:ss") 
            " - Processando interface --> " esp-fila-wms.cod-trans SKIP.
       
        CASE esp-fila-wms.cod-trans:
            WHEN "Item" THEN DO:
                FIND ITEM NO-LOCK WHERE
                     ITEM.it-codigo = esp-fila-wms.chave NO-ERROR.
                IF AVAIL ITEM THEN 
                    RUN esp/eswms001.p (BUFFER ITEM).
                ELSE
                    DELETE esp-fila-wms.    
            END.
            WHEN "CodigoBarras" THEN DO:
                FIND ITEM NO-LOCK WHERE
                     ITEM.it-codigo = esp-fila-wms.chave NO-ERROR.
                IF AVAIL ITEM THEN 
                    RUN esp/eswms015.p (BUFFER ITEM).
                ELSE
                    DELETE esp-fila-wms.    
            END.
            /*WHEN "Embalagem" THEN DO:
                FIND FIRST un-item NO-LOCK
                     WHERE un-item.it-codigo EQ ENTRY(1,esp-fila-wms.chave,"|") NO-ERROR.
                IF AVAIL un-item THEN
                    run esp/eswms016.p (buffer un-item,
                                        entry(2,esp-fila-wms.chave,"|")).
                else
                    delete esp-fila-wms.                                                    
            END.*/
            WHEN "Emitente" THEN DO:
                FIND emitente NO-LOCK WHERE
                     emitente.cod-emitente = INTEGER(ENTRY(1,esp-fila-wms.chave,"|")) NO-ERROR.
                IF AVAIL emitente THEN
                    RUN esp/eswms002.p (BUFFER emitente,
                                        INTEGER(ENTRY(2,esp-fila-wms.chave,"|"))).
                ELSE
                    DELETE esp-fila-wms.                                                    
            END.
            WHEN "Transportadora" THEN DO:
                FIND transporte NO-LOCK WHERE
                     transporte.cod-transp = INTEGER(ENTRY(1,esp-fila-wms.chave,"|")) NO-ERROR.
                IF AVAIL transporte THEN
                    RUN esp/eswms003.p (BUFFER transporte).
                ELSE
                    DELETE esp-fila-wms.            
            END.
            
            /*when "NFCancelada" then do: /*NÆo sera utilizado*/
                find nota-fiscal no-lock where
                     nota-fiscal.cod-estabel  =         entry(1,esp-fila-wms.chave,"|") and
                     nota-fiscal.serie        =         entry(2,esp-fila-wms.chave,"|")  and
                     nota-fiscal.nr-nota-fis  =         entry(3,esp-fila-wms.chave,"|")  no-error.                     
                if avail nota-fiscal then
                    run esp/eswms014.p (buffer nota-fiscal,
                                        entry(4,esp-fila-wms.chave,"|")).
                else
                    delete esp-fila-wms.              
            end.*/

            
            WHEN "NFEntrada" THEN DO:
                    FIND docum-est NO-LOCK WHERE
                         docum-est.cod-emitente = INTEGER(ENTRY(1,esp-fila-wms.chave,"|")) AND
                         docum-est.serie-docto  =         ENTRY(2,esp-fila-wms.chave,"|")  AND
                         docum-est.nro-docto    =         ENTRY(3,esp-fila-wms.chave,"|")  AND                     
                         docum-est.nat-operacao =         ENTRY(4,esp-fila-wms.chave,"|")  NO-ERROR.                     
                    IF AVAIL docum-est THEN
                        RUN esp/eswms006.p (BUFFER docum-est,
                                            ENTRY(5,esp-fila-wms.chave,"|")).
                    ELSE
                        DELETE esp-fila-wms.              
            END.
            WHEN "NFRecFisico" THEN DO:
                 
                FIND doc-fisico NO-LOCK WHERE
                     doc-fisico.serie-docto  =         ENTRY(1,esp-fila-wms.chave,"|")  AND
                     doc-fisico.nro-docto    =         ENTRY(2,esp-fila-wms.chave,"|")  AND                     
                     doc-fisico.cod-emitente = INTEGER(ENTRY(3,esp-fila-wms.chave,"|")) NO-ERROR.                     
                IF AVAIL doc-fisico THEN
                    RUN esp/eswms012.p (BUFFER doc-fisico,
                                        ENTRY(4,esp-fila-wms.chave,"|")).
                ELSE
                    DELETE esp-fila-wms.              
            END.
            when "Embarque" then do:
                FOR FIRST res-cli WHERE
                    res-cli.cdd-embarq = INT(ENTRY(1,esp-fila-wms.chave,"|")) AND
                    res-cli.nr-resumo    = INT(ENTRY(2,esp-fila-wms.chave,"|")) NO-LOCK: END.
                IF AVAIL res-cli THEN DO:
                    FOR FIRST esp-resumo-wms WHERE
                        esp-resumo-wms.nr-embarque = INT(res-cli.cdd-embarq) AND
                        esp-resumo-wms.nr-resumo   = res-cli.nr-resumo NO-LOCK:

                        IF NOT esp-resumo-wms.cancelado THEN
                            run esp/eswms007.p (INPUT esp-fila-wms.chave,
                                                INPUT "Confirmar").
                        ELSE
                            run esp/eswms007.p (INPUT esp-fila-wms.chave,
                                                INPUT "Cancelar").
                    END.
                END.
                ELSE
                    DELETE esp-fila-wms.                    
            end.
            when "Movimenta‡Æo" then do:
                find movto-estoq no-lock where
                     movto-estoq.nr-trans = integer(entry(1,esp-fila-wms.chave,"|")) no-error.
                if avail movto-estoq then
                    run esp/eswms010.p (buffer movto-estoq).
                else
                    delete esp-fila-wms.            
            end.
            when "Requisi‡Æo" then do:
                find movto-estoq no-lock where
                     movto-estoq.nr-trans = integer(entry(1,esp-fila-wms.chave,"|")) no-error.
                if avail movto-estoq then
                    run esp/eswms011.p (buffer movto-estoq).
                else
                    delete esp-fila-wms.            
            end.
            otherwise delete esp-fila-wms.          
        
        END CASE.       
    END.
END.

PROCEDURE criaAlias:
       /* bancos do ems 2.06b */
    IF  CONNECTED("mgcad") THEN DO:
        CREATE ALIAS mgadm FOR DATABASE mgcad NO-ERROR.
        CREATE ALIAS mgadt FOR DATABASE mgcad NO-ERROR.
        CREATE ALIAS mgaps FOR DATABASE mgcad NO-ERROR.
        CREATE ALIAS mgcex FOR DATABASE mgcad NO-ERROR.
        CREATE ALIAS mgcld FOR DATABASE mgcad NO-ERROR.
        CREATE ALIAS mgdis FOR DATABASE mgcad NO-ERROR.
        CREATE ALIAS mgfis FOR DATABASE mgcad NO-ERROR.
        CREATE ALIAS mgind FOR DATABASE mgcad NO-ERROR.
        CREATE ALIAS mginv FOR DATABASE mgcad NO-ERROR.
        CREATE ALIAS mgmfg FOR DATABASE mgcad NO-ERROR.
        CREATE ALIAS mgmnt FOR DATABASE mgcad NO-ERROR.
        CREATE ALIAS mgmrp FOR DATABASE mgcad NO-ERROR.
        CREATE ALIAS mgrac FOR DATABASE mgcad NO-ERROR.
        CREATE ALIAS mgtmp FOR DATABASE mgcad NO-ERROR.
        CREATE ALIAS mguni FOR DATABASE mgcad NO-ERROR.
        CREATE ALIAS mgven FOR DATABASE mgcad NO-ERROR.
    END.
    IF  CONNECTED("mgmov") THEN DO:
        CREATE ALIAS movadm  FOR DATABASE mgmov NO-ERROR.
        CREATE ALIAS movdis  FOR DATABASE mgmov NO-ERROR.
        CREATE ALIAS movfis  FOR DATABASE mgmov NO-ERROR.
        CREATE ALIAS movind  FOR DATABASE mgmov NO-ERROR.
        CREATE ALIAS movmfg  FOR DATABASE mgmov NO-ERROR.
        CREATE ALIAS movmnt  FOR DATABASE mgmov NO-ERROR.
        CREATE ALIAS movrac  FOR DATABASE mgmov NO-ERROR.
        CREATE ALIAS wmovdis FOR DATABASE mgmov NO-ERROR.
    END.
    /* BANCOS EMS 5.06 */
    IF  CONNECTED("ems506") THEN DO:
        CREATE ALIAS emsbas FOR DATABASE ems506 NO-ERROR.
        CREATE ALIAS emsedi FOR DATABASE ems506 NO-ERROR.
        CREATE ALIAS emsfin FOR DATABASE ems506 NO-ERROR.
        CREATE ALIAS emsuni FOR DATABASE ems506 NO-ERROR.
        CREATE ALIAS emsven FOR DATABASE ems506 NO-ERROR.
        CREATE ALIAS movfin FOR DATABASE ems506 NO-ERROR.
    END.
END.
