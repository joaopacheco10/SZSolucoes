current-language = current-language.

DEF NEW SHARED STREAM s-log.

DEFINE VARIABLE c-arq-log-wms AS CHARACTER   NO-UNDO.

ASSIGN c-arq-log-wms = "LOG_WMS_CARGA_PEDIDO_NF_FIS" +
                   STRING(YEAR(TODAY),"9999") +
                   STRING(MONTH(TODAY),"99") + 
                   STRING(DAY(TODAY),"99") +                    
                   ".txt" .                   

OUTPUT STREAM s-log TO VALUE(SESSION:TEMP-DIR + "~\" + c-arq-log-wms) KEEP-MESSAGES UNBUFFERED
    CONVERT TARGET "iso8859-1".

DEF NEW GLOBAL SHARED VAR g-integrator AS LOG NO-UNDO.

def var i as int no-undo.

ASSIGN g-integrator = YES.

find nota-fiscal no-lock where
     nota-fiscal.cod-estabel = '1' and
     nota-fiscal.serie       = '3' and
     nota-fiscal.nr-nota-fis = '0154698' no-error.
if avail nota-fiscal then do:

    run esp/eswms007a.p (buffer nota-fiscal).
    
end.         

OUTPUT STREAM S-LOG CLOSE.


         
