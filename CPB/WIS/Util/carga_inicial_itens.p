DEFINE VARIABLE i-dias AS INTEGER     NO-UNDO.
DEF VAR h-acomp AS HANDLE NO-UNDO.
def var i-qt-item as int no-undo.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

RUN pi-inicializar IN h-acomp ("Itens").

DEF NEW GLOBAL SHARED VAR g-integrator AS LOG NO-UNDO.

ASSIGN g-integrator = YES.

DEF NEW SHARED STREAM s-log.

DEFINE VARIABLE c-arq-log-wms AS CHARACTER   NO-UNDO.

ASSIGN c-arq-log-wms = "LOG_WMS_CARGA_ITENS" +
                   STRING(YEAR(TODAY),"9999") +
                   STRING(MONTH(TODAY),"99") + 
                   STRING(DAY(TODAY),"99") +                    
                   ".txt" .                   

OUTPUT STREAM s-log TO VALUE(SESSION:TEMP-DIR + "~\" + c-arq-log-wms) KEEP-MESSAGES UNBUFFERED
    CONVERT TARGET "iso8859-1".

FOR EACH ITEM NO-LOCK
   WHERE trim(item.it-codigo) <> "" :
   
   
   /*IF NOT CAN-FIND(FIRST movto-estoq NO-LOCK
               WHERE movto-estoq.it-codigo = ITEM.it-codigo
                 AND movto-estoq.cod-estabel = "1"
                 AND movto-estoq.dt-trans > TODAY - (365 * 1)) THEN
       NEXT.*/
       
    assign i-qt-item = i-qt-item + 1.   

    RUN pi-acompanhar IN h-acomp (ITEM.it-codigo).
    run esp/eswms001.p (buffer item).
    
    run esp/eswms015.p (buffer item).
    
    /*if i-qt-item >= 1 then
        leave.*/
    
END.


OUTPUT STREAM S-LOG CLOSE.
    
RUN pi-finalizar IN h-acomp.




