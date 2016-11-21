{include/i-bfems2.i }
{include/i-epc200.i}

define input param  p-ind-event as char no-undo.
define input-output param table for tt-epc.
   
def var h-bodi317sd as handle no-undo.
DEF VAR i-embarque AS INT NO-UNDO.
DEF VAR i-resumo   AS INT NO-UNDO.

if p-ind-event = "BeforeGeraResumosDoEmbarqueParaCalculo" then do:
    FOR FIRST tt-epc WHERE 
        tt-epc.cod-parameter = "cdd-embarq":
        ASSIGN i-embarque = INT(tt-epc.val-parameter).
    END.

    FOR FIRST tt-epc WHERE 
        tt-epc.cod-parameter = "nr-resumo":
        ASSIGN i-resumo = INT(tt-epc.val-parameter).
    END.

    h-bodi317sd = this-procedure:instantiating-procedure .

    FOR FIRST res-cli WHERE
        res-cli.cdd-embarq = i-embarque AND
        res-cli.nr-resumo  = i-resumo NO-LOCK:

        find esp-resumo-wms no-lock where
            esp-resumo-wms.nr-embarque = i-embarque AND
            esp-resumo-wms.nr-resumo   = i-resumo no-error.
        if avail esp-resumo-wms then do: 
        
            find first pre-fatur no-lock where
                       pre-fatur.cdd-embarq = esp-resumo-wms.nr-embarque and
                       pre-fatur.nr-resumo  = esp-resumo-wms.nr-resumo   and
                       pre-fatur.nome-abrev = res-cli.nome-abrev         no-error.
            if avail pre-fatur then do:
                if can-find(first it-pre-fat where
                            it-pre-fat.cdd-embarq = esp-resumo-wms.nr-embarque and
                            it-pre-fat.nr-resumo  = esp-resumo-wms.nr-resumo   and
                            it-pre-fat.nome-abrev  = res-cli.nome-abrev        and
                            it-pre-fat.nr-pedcli   = pre-fatur.nr-pedcli       and
                            it-pre-fat.int-2 <> 9) then do:
                    
                    RUN _insertErrorManual IN h-bodi317sd (INPUT 17006,
                                                           INPUT "EMS":U,
                                                           INPUT "ERROR":U,
                                                           INPUT "Resumo nao confirmado pelo WMS.",
                                                           INPUT "Para efetuar o calculo, necessario que todos os itens do resumo sejam liberados pelo WMS.",
                                                           INPUT "":U).
        
                end.
            end.    
        end.
    END.
end.



