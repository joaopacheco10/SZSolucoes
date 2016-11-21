/**************************************************************************
**   Programa: upc/u01re0402rp.p
**   Data    : Janeiro de 2010
**   Autor   : Eder
**   Objetivo: Integra‡Æo WMS Nota Fiscais (RE1001)
**   Versao..: 
**************************************************************************/
{include/i-bfems2.i}
{include/i-epc200.i re0402rp}

define input param  p-ind-event as char no-undo.
define input-output param table for tt-epc.

IF RETURN-VALUE = "NOK" OR RETURN-VALUE = "ADM-ERROR" THEN
    RETURN RETURN-VALUE.

if p-ind-event = "Fim-Atualizacao" then do:

    find first tt-epc
         where tt-epc.cod-event     = p-ind-event
           and tt-epc.cod-parameter = "docum-est-rowid"  no-error.
    if avail tt-epc then do:
        find esp-param-integr-wms no-lock no-error.

        for first docum-est no-lock
            where rowid(docum-est) = to-rowid(tt-epc.val-parameter):

            if not avail esp-param-integr-wms or 
               not can-do(esp-param-integr-wms.lista-estab-integr,docum-est.cod-estabel) then
                return "OK".

            run esp/eswms006.p (buffer docum-est, "desatualiza").

        end.
    end.
end.

