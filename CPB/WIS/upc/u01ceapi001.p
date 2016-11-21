
/**************************************************************************
**   Programa: upc/u01ceapi001.p
**   Data    : Julho de 2010
**   Autor   : SZ - Eder
**   Objetivo: Integra ?o WMS Movimento Estoque
**   Versao..: 
**************************************************************************/
DEFINE BUFFER prog_dtsul FOR emsfnd.prog_dtsul.

{include/i-epc200.i1} /* definicao da temp-table tt-epc */
 
procedure piControle-Vasilhames:
    
    def input        param p-ind-event as char no-undo.
    def input-output param table       for tt-epc.

    find first tt-epc
         where tt-epc.cod-event     = p-ind-event
           and tt-epc.cod-parameter = "movto-estoq-rowid"  no-error.
    if avail tt-epc then do:
        find esp-param-integr-wms no-lock no-error.
        
        for first movto-estoq no-lock
            where rowid(movto-estoq) = to-rowid(tt-epc.val-parameter):
                        
            if not avail esp-param-integr-wms or 
               not can-do(esp-param-integr-wms.lista-estab-integr,movto-estoq.cod-estabel) OR
               not can-do(esp-param-integr-wms.lista-depos-integr,movto-estoq.cod-depos) then
                return "OK".

           /*Transferencia considerar apenas a entrada*/
           if (movto-estoq.esp-docto     = 33 and 
              movto-estoq.cod-prog-orig = "CE0206" and
              movto-estoq.tipo-trans    = 1) or
              /*Reporte de produ ?o/Estorno Reporte*/
              (movto-estoq.esp-docto     = 01  or movto-estoq.esp-docto     = 08) then
                    
              run esp/eswms010.p (buffer movto-estoq).
                
        end.
    end.
end.


return "OK".


