/**************************************************************************
**   Programa: upc/u01re1005rp.p
**   Data    : Janeiro de 2010
**   Autor   : Eder
**   Objetivo: Integra‡Æo WMS Nota Fiscais (RE1001)
**   Versao..: 
**************************************************************************/
DEFINE BUFFER prog_dtsul FOR emsfnd.prog_dtsul.
{include/i-epc200.i re1005rp}

define input param  p-ind-event as char no-undo.
define input-output param table for tt-epc.

DEFINE VARIABLE l-integra-doc AS LOGICAL     NO-UNDO.
def var l-item-ok as log no-undo.

if p-ind-event = "fim-atualizacao" then do:

    find first tt-epc
         where tt-epc.cod-event     = p-ind-event
           and tt-epc.cod-parameter = "docum-est rowid"  no-error.
    if avail tt-epc then do:
        find esp-param-integr-wms no-lock no-error.
        
        for first docum-est no-lock
            where rowid(docum-est) = to-rowid(tt-epc.val-parameter):
                       
            /*find first doc-fisico no-lock
                 where doc-fisico.cod-emitente = docum-est.cod-emitente
                   and doc-fisico.nro-docto    = docum-est.nro-docto
                   and doc-fisico.serie-docto  = docum-est.serie-docto no-error.
            if avail doc-fisico then
                return "OK".*/

            if not avail esp-param-integr-wms or 
               not can-do(esp-param-integr-wms.lista-estab-integr,docum-est.cod-estabel) then
                return "OK".
                
            /*ASSIGN l-integra-doc = NO .
            FOR EACH rat-lote of docum-est no-lock :
                IF can-do(esp-param-integr-wms.lista-depos-integr,rat-lote.cod-depos) THEN
                    ASSIGN l-integra-doc = YES .
            END.

            IF l-integra-doc = NO THEN RETURN.
            */
            
            /*find first rat-lote of docum-est no-lock no-error.

            if not avail rat-lote or
                not can-do(esp-param-integr-wms.lista-depos-integr,rat-lote.cod-depos) THEN
                return.*/
                
            assign l-item-ok = no.    
                
            FOR EACH item-doc-est NO-LOCK OF docum-est,
                FIRST ITEM NO-LOCK WHERE 
                      ITEM.it-codigo = item-doc-est.it-codigo:
                      
                if (item-doc-est.nat-operacao = "1102"  or
                    item-doc-est.nat-operacao = "2102"  or
                    item-doc-est.nat-operacao = "3102") and
                    item.fm-cod-com           = "711"   then
                    next.   
                  
                if item.politica = 6 then do:
                
                    IF NOT CAN-DO(esp-param-integr-wms.lista-depos-integr,item-doc-est.cod-depos) THEN NEXT .  
                    
                    assign l-item-ok = yes.
                end.
                else do:
                
                    for EACH rat-lote OF item-doc-est NO-LOCK:
        
                        IF NOT CAN-DO(esp-param-integr-wms.lista-depos-integr,rat-lote.cod-depos) THEN NEXT .
                        
                        assign l-item-ok = yes.
                        
                    end.
                end.                      
            end.            


            if l-item-ok then
                run esp/eswms006.p (buffer docum-est, "atualiza").
            
        end.
    end.
end.
