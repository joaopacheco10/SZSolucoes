def var c-tp-nota as char no-undo.

DEF NEW SHARED STREAM s-log.

DEFINE VARIABLE c-arq-log-wms AS CHARACTER   NO-UNDO.

ASSIGN c-arq-log-wms = "LOG_WMS_CARGA_NOTA_RECEBIMENTO" +
                   STRING(YEAR(TODAY),"9999") +
                   STRING(MONTH(TODAY),"99") + 
                   STRING(DAY(TODAY),"99") +                    
                   ".txt" .                   

OUTPUT STREAM s-log TO VALUE(SESSION:TEMP-DIR + "~\" + c-arq-log-wms) KEEP-MESSAGES UNBUFFERED
    CONVERT TARGET "iso8859-1".

DEF NEW GLOBAL SHARED VAR g-integrator AS LOG NO-UNDO.

def var i as int no-undo.
def var qt-it as int no-undo.
def var l-ok as log no-undo.

ASSIGN g-integrator = YES.

FIND esp-param-integr-wms NO-LOCK NO-ERROR.    


for each docum-est no-lock where
         docum-est.nro-docto <> "0026482" and
         docum-est.nro-docto <> "0326263"
    break by docum-est.dt-emissao desc.
    
    find natur-oper no-lock where
         natur-oper.nat-operacao = docum-est.nat-operacao no-error.
    
    IF natur-oper.tipo-compra = 3 THEN DO:

        ASSIGN c-tp-nota = "DEV".
    END.
    ELSE
        IF natur-oper.transf THEN
            ASSIGN c-tp-nota = "TRA".
        ELSE 
            ASSIGN c-tp-nota = "FOR". 
            
            
    if c-tp-nota = "FOR" then do:
    
        assign l-ok  = no
               qt-it = 0.
    
        FOR EACH item-doc-est NO-LOCK OF docum-est,
            FIRST ITEM NO-LOCK 
            WHERE ITEM.it-codigo = item-doc-est.it-codigo,
             EACH rat-lote OF item-doc-est NO-LOCK:
             
             IF NOT CAN-DO(esp-param-integr-wms.lista-depos-integr,rat-lote.cod-depos) THEN NEXT .
             
             assign l-ok  = yes
                    qt-it = qt-it + 1.
             
        end.

        if l-ok and qt-it > 2 then do: 
            assign i = i + 1.
                
            RUN esp/eswms006.p (BUFFER docum-est, "atualiza").
            
            if i >= 1 then
                leave.
        end.
    end.
                
end.

OUTPUT STREAM S-LOG CLOSE.
