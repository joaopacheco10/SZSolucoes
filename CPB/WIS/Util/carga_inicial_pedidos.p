
DEF NEW SHARED STREAM s-log.

DEFINE VARIABLE c-arq-log-wms AS CHARACTER   NO-UNDO.

ASSIGN c-arq-log-wms = "LOG_WMS_CARGA_PEDIDO" +
                   STRING(YEAR(TODAY),"9999") +
                   STRING(MONTH(TODAY),"99") + 
                   STRING(DAY(TODAY),"99") +                    
                   ".txt" .                   

OUTPUT STREAM s-log TO VALUE(SESSION:TEMP-DIR + "~\" + c-arq-log-wms) KEEP-MESSAGES UNBUFFERED
    CONVERT TARGET "iso8859-1".

DEF NEW GLOBAL SHARED VAR g-integrator AS LOG NO-UNDO.

def var i as int no-undo.

ASSIGN g-integrator = YES.

for each res-cli no-lock where
         res-cli.cdd-embarq = 13868:
         
    RUN esp/eswms007.p (INPUT STRING(res-cli.cdd-embarq) + "|" + STRING(res-cli.nr-resumo),
                        INPUT "Confirmar").              
end.

OUTPUT STREAM S-LOG CLOSE.


         
