DEFINE VARIABLE i-dias AS INTEGER     NO-UNDO.
DEF VAR h-acomp AS HANDLE NO-UNDO.

DEF NEW SHARED STREAM s-log.

DEFINE VARIABLE c-arq-log-wms AS CHARACTER   NO-UNDO.

ASSIGN c-arq-log-wms = "LOG_WMS_CARGA_CLIENTE_FORNEC" +
                   STRING(YEAR(TODAY),"9999") +
                   STRING(MONTH(TODAY),"99") + 
                   STRING(DAY(TODAY),"99") +                    
                   ".txt" .                   

OUTPUT STREAM s-log TO VALUE(SESSION:TEMP-DIR + "~\" + c-arq-log-wms) KEEP-MESSAGES UNBUFFERED
    CONVERT TARGET "iso8859-1".

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

/*RUN pi-inicializar IN h-acomp ("Itens").
    
FOR EACH ITEM NO-LOCK:
    RUN pi-acompanhar IN h-acomp (ITEM.it-codigo).
    run esp/eswms001.p (buffer item).
END.*/

ASSIGN i-dias = 365 * 3. /* 3 anos */

DEF NEW GLOBAL SHARED VAR g-integrator AS LOG NO-UNDO.

def var i as int no-undo.

ASSIGN g-integrator = YES.

RUN pi-inicializar IN h-acomp ("Clientes/Fornecedores").
FOR EACH emitente NO-LOCK /*where
         emitente.identific <> 2*/ :

    FIND FIRST nota-fiscal NO-LOCK
        WHERE nota-fiscal.cod-emitente = emitente.cod-emitente
          AND nota-fiscal.dt-emis-nota > TODAY - i-dias NO-ERROR.
    IF NOT AVAIL nota-fiscal THEN DO:

        FIND FIRST docum-est NO-LOCK
            WHERE docum-est.cod-emitente = emitente.cod-emitente
              AND docum-est.dt-trans > TODAY - i-dias NO-ERROR.

        IF AVAIL docum-est THEN DO:
        
           RUN pi-acompanhar IN h-acomp (STRING(emitente.cod-emitente)).
           
           assign i = i + 1.
           IF emitente.identific <> 2 THEN
               RUN esp/eswms002.p (BUFFER emitente, 1).
               
           IF emitente.identific <> 1 THEN
               RUN esp/eswms002.p (BUFFER emitente, 2).
               
           /*if i >= 10 then leave.    */
        END.
    END.
    ELSE DO:
       RUN pi-acompanhar IN h-acomp (STRING(emitente.cod-emitente)).
       assign i = i + 1.
       IF emitente.identific <> 2 THEN
           RUN esp/eswms002.p (BUFFER emitente, 1).
           
       IF emitente.identific <> 1 THEN
           RUN esp/eswms002.p (BUFFER emitente, 2).
           
       /*if i >= 10 then leave.*/    
    END.
END.

/*RUN pi-inicializar IN h-acomp ("Transportadoras").
FOR EACH transporte NO-LOCK:
    RUN pi-acompanhar IN h-acomp (transporte.cod-transp).
    run esp/eswms003.p (buffer transporte).
END.*/

OUTPUT STREAM S-LOG CLOSE.
    
RUN pi-finalizar IN h-acomp.




