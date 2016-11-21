/**************************************************************************
**   Programa: upc/u01eq0506.p
**   Data    : Maio 2015
**   Autor   : Eder
**   Objetivo: Integraá∆o WMS Embarques
**   Versao..: 
**************************************************************************/
{include/i-bfems2.i}
{include/i-prgvrs.i u01eq0506.p 2.11.00.001}

{tools/fc-handle-obj.i}
{tools/fc-falso.i}
{utp/ut-glob.i}
{eqp/eq0506b-b03.i -aux}
{eqp/eqapi300.i}
{cdp/cd0667.i}

DEF INPUT PARAMETER p-ind-event  AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-ind-object AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-wgh-object AS HANDLE            NO-UNDO.
DEF INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE     NO-UNDO.
DEF INPUT PARAMETER p-cod-table  AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-row-table  AS ROWID             NO-UNDO.

DEF NEW GLOBAL SHARED VARIABLE wh-wmeq0506-btcancela      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-wmeq0506-bt-mod         AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-wmeq0506-bt-modificar   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-wmeq0506-bt-del         AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-wmeq0506-bt-wms         AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-wmeq0506-objeto         AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-wmeq0506-nr-embarque    AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VARIABLE adm-broker-hdl             AS HANDLE        NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-wmeq0506-browse1        AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-folder                  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE r-res-cli-eq0506           AS ROWID         NO-UNDO.

DEFINE VARIABLE c-handle-obj AS CHAR NO-UNDO.

DEFINE VARIABLE c-char AS CHARACTER   NO-UNDO.

def var i-sequen         as int init 1    no-undo.
def var h-eqapi300       as handle        no-undo.
def var bo-ped-venda-can as handle        no-undo.
def var i-tp-cancel      as int init 0    no-undo.

def temp-table RowErrors no-undo
    field ErrorSequence    as integer
    field ErrorNumber      as integer
    field ErrorDescription as character
    field ErrorParameters  as character
    field ErrorType        as character
    field ErrorHelp        as character
    field ErrorSubType     as character.

/* MESSAGE "Evento..........:" STRING(p-ind-event)    SKIP */
/*         "Objeto..........:" STRING(p-ind-object)   SKIP */
/*         "Handle do Objeto:" STRING(p-wgh-object)   SKIP */
/*         "Handle da Frame.:" STRING(p-wgh-frame)    SKIP */
/*         "Tabela..........:" p-cod-table            SKIP */
/*         "Rowid...........:" STRING(p-row-table)    SKIP */
/*         "p-wgh-object....:" p-wgh-object:FILE-NAME SKIP */
/*         "c-char..........:" c-char VIEW-AS ALERT-BOX.   */

if p-ind-event  = "INITIALIZE" and
   p-ind-object = "BROWSER"    and
   p-wgh-object:FILE-NAME = "eqp/eq0506-b01.w" then do:
   
   ASSIGN wh-wmeq0506-browse1 = p-wgh-object.   
   
end.  

IF  p-ind-object = "CONTAINER"    /* Cria os Botoes */
AND p-ind-event = "INITIALIZE" THEN DO:

    RUN init-pages IN p-wgh-object ("1,2,3":U).
    
    /*RUN init-pages IN p-wgh-object (INPUT "2").
    RUN init-pages IN p-wgh-object (INPUT "3").
    
   ASSIGN wh-wmeq0506-objeto = p-wgh-object:NEXT-SIBLING.

   DO WHILE VALID-HANDLE(wh-wmeq0506-objeto):

       IF wh-wmeq0506-objeto:FILE-NAME = "eqp/eq0506-b01.w" THEN
           ASSIGN wh-wmeq0506-browse1 = wh-wmeq0506-objeto.

       ASSIGN wh-wmeq0506-objeto = wh-wmeq0506-objeto:NEXT-SIBLING.
   END.
   */

   assign c-handle-obj = fc-handle-obj("bt-mod,bt-wms,bt-del,cdd-embarq", p-wgh-frame).

   ASSIGN
          wh-wmeq0506-bt-mod         = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
          wh-wmeq0506-bt-wms         = WIDGET-HANDLE(ENTRY(2,c-handle-obj))
          wh-wmeq0506-bt-del         = WIDGET-HANDLE(ENTRY(3,c-handle-obj))
          wh-wmeq0506-nr-embarque    = WIDGET-HANDLE(ENTRY(4,c-handle-obj)).

   ON 'choose':U of wh-wmeq0506-bt-wms PERSISTENT RUN upc/u01eq0506.p (INPUT "choose-btwms",
                                                                       INPUT p-ind-object,
                                                                       INPUT p-wgh-object,
                                                                       INPUT p-wgh-frame,
                                                                       INPUT p-cod-table,
                                                                       INPUT p-row-table).
       
   CREATE BUTTON wh-wmeq0506-btcancela    /*Bot∆o Cancela*/
   ASSIGN FRAME         =  p-wgh-frame
          LABEL         =  wh-wmeq0506-bt-wms:LABEL 
          WIDTH         =  wh-wmeq0506-bt-wms:WIDTH 
          HEIGHT        =  wh-wmeq0506-bt-wms:HEIGHT 
          ROW           =  wh-wmeq0506-bt-wms:ROW 
          COL           =  wh-wmeq0506-bt-wms:COL + 15
          SENSITIVE     =  NO
          VISIBLE       =  YES 

    TRIGGERS:
        ON 'choose':U PERSISTENT RUN upc/u01eq0506.p (INPUT "choose-btcancela",
                                                      INPUT p-ind-object,
                                                      INPUT p-wgh-object,
                                                      INPUT p-wgh-frame,
                                                      INPUT p-cod-table,
                                                      INPUT p-row-table).
    END.

    wh-wmeq0506-btcancela:LOAD-IMAGE-UP("image\im-can.bmp").
    wh-wmeq0506-btcancela:LOAD-IMAGE-INSENSITIVE("image\ii-can.bmp").

END.

 /*
IF  p-ind-object = "VIEWER"   /*Desabilita e Habilita Botoes */
AND p-ind-event  = "DISPLAY" 
AND p-wgh-object:PRIVATE-DATA = "eqp/eq0506-v02.w" THEN DO:

    
    
END.
*/

IF  p-ind-object = "BROWSER"    /*Desabilita e Habilita Bot«úo Modificar */
AND p-ind-event  = "AFTER-OPEN-QUERY" 
AND p-wgh-object:PRIVATE-DATA = "eqp/eq0506-b02.w" THEN DO:

    ASSIGN c-handle-obj = fc-handle-obj("bt-modificar", p-wgh-frame)
           wh-wmeq0506-bt-modificar = WIDGET-HANDLE(ENTRY(1,c-handle-obj)).
    
END.
IF  (p-ind-object = "BROWSER" AND   /*Desabilita, Habilita Bot∆o Modificar e pega rowid res-cli */
    p-wgh-object:FILE-NAME = "eqp/eq0506-b03.w" AND
    p-ind-event  = "AFTER-VALUE-CHANGED") OR
    (p-ind-event  = "AFTER-OPEN-QUERY" AND
    p-ind-object = "BROWSER" AND
    p-wgh-object:FILE-NAME = "eqp/eq0506-b03.w") THEN DO:

    ASSIGN r-res-cli-eq0506 = p-row-table.

    IF r-res-cli-eq0506 <> ? AND VALID-HANDLE(wh-wmeq0506-nr-embarque) THEN DO:
        FOR FIRST res-cli WHERE
            ROWID(res-cli) = r-res-cli-eq0506 NO-LOCK:                
            FIND FIRST esp-resumo-wms WHERE
                esp-resumo-wms.nr-embarque = int(wh-wmeq0506-nr-embarque:SCREEN-VALUE) AND
                esp-resumo-wms.nr-resumo   = res-cli.nr-resumo NO-LOCK NO-ERROR.
            IF AVAIL esp-resumo-wms AND esp-resumo-wms.enviado = YES THEN
                ASSIGN wh-wmeq0506-bt-modificar:SENSITIVE = NO.
            ELSE
                ASSIGN wh-wmeq0506-bt-modificar:SENSITIVE = YES.        
            
        END.
    END.
    ELSE
        ASSIGN wh-wmeq0506-bt-modificar:SENSITIVE = YES.


    RUN pi-trata-botoes.
    
END.

def var i-tp-envio as int no-undo.

IF p-ind-event = "choose-btwms" THEN DO: /*Evento do Botao OK*/

    current-window:sensitive = false.
        
    run upc/u01eq0506a1.w (output i-tp-envio).
    
    current-window:sensitive = true.

    if i-tp-envio <> 0 then do:
    
        RUN utp/ut-msgs.p (INPUT "show", INPUT 27100, INPUT "Confirma?~~Deseja confirmar operaá∆o?").
    
        IF RETURN-VALUE = "YES" AND r-res-cli-eq0506 <> ? THEN DO:
            find embarque no-lock where
                 embarque.cdd-embarq = int(wh-wmeq0506-nr-embarque:screen-value) no-error.
                 
            if i-tp-envio = 1 then do:
            
                for each res-cli no-lock where
                         res-cli.cdd-embarq = embarque.cdd-embarq:
                         
                         
                    if not can-find(first esp-resumo-wms where
                                          esp-resumo-wms.nr-embarque = int(wh-wmeq0506-nr-embarque:SCREEN-VALUE) AND
                                          esp-resumo-wms.nr-resumo   = res-cli.nr-resumo) then do:
                         
                        RUN esp/eswms007.p (INPUT STRING(res-cli.cdd-embarq) + "|" + STRING(res-cli.nr-resumo),
                                            INPUT "Confirmar").
            
                        IF RETURN-VALUE <> "ok" THEN
                            RETURN "nok".
            
                        IF VALID-HANDLE(wh-wmeq0506-nr-embarque) THEN do:
                            CREATE esp-resumo-wms.
                            ASSIGN esp-resumo-wms.nr-embarque = int(wh-wmeq0506-nr-embarque:SCREEN-VALUE)
                                   esp-resumo-wms.nr-resumo   = res-cli.nr-resumo
                                   esp-resumo-wms.enviado     = YES
                                   esp-resumo-wms.dt-envio    = TODAY
                                   esp-resumo-wms.cod-usuar   = c-seg-usuario
                                   esp-resumo-wms.confirmado  = NO
                                   esp-resumo-wms.cancelado   = NO
                                   esp-resumo-wms.conf-fis    = no.
                
                            ASSIGN wh-wmeq0506-bt-mod:SENSITIVE    = NO
                                   wh-wmeq0506-bt-del:SENSITIVE    = NO
                                   wh-wmeq0506-btcancela:SENSITIVE = YES
                                   wh-wmeq0506-bt-wms:SENSITIVE    = NO.
                        END. 
                    end.                            
                end.
            end.
            else do:     
                FIND res-cli WHERE
                    ROWID(res-cli) = r-res-cli-eq0506 NO-LOCK NO-ERROR.
                /*
                FOR FIRST esp-fila-wms WHERE
                    esp-fila-wms.chave = STRING(res-cli.cdd-embarq) + "|" + STRING(res-cli.nr-resumo) + "|" + "Confirmar" EXCLUSIVE-LOCK:
        
                    ASSIGN esp-fila-wms.data-hora-integracao = ?.
                END.*/
        
                FIND FIRST esp-resumo-wms WHERE
                    esp-resumo-wms.nr-embarque = int(wh-wmeq0506-nr-embarque:SCREEN-VALUE) AND
                    esp-resumo-wms.nr-resumo   = res-cli.nr-resumo AND
                    esp-resumo-wms.cancelado NO-LOCK NO-ERROR.
                IF AVAIL esp-resumo-wms THEN
                    RUN esp/eswms007.p (INPUT STRING(res-cli.cdd-embarq) + "|" + STRING(res-cli.nr-resumo),
                                        INPUT "Reenvio").
                ELSE
                    RUN esp/eswms007.p (INPUT STRING(res-cli.cdd-embarq) + "|" + STRING(res-cli.nr-resumo),
                                        INPUT "Confirmar").
        
        
                IF RETURN-VALUE <> "ok" THEN
                    RETURN "nok".
        
                IF VALID-HANDLE(wh-wmeq0506-nr-embarque) THEN do :
                    FIND FIRST esp-resumo-wms WHERE
                        esp-resumo-wms.nr-embarque = int(wh-wmeq0506-nr-embarque:SCREEN-VALUE) AND
                        esp-resumo-wms.nr-resumo   = res-cli.nr-resumo EXCLUSIVE-LOCK NO-ERROR.
                    IF AVAIL esp-resumo-wms THEN DO:
                        /*verificar logica para reenvio aqui*/
                        ASSIGN esp-resumo-wms.enviado = YES
                               esp-resumo-wms.cancelado = NO
                               esp-resumo-wms.cod-usuar = c-seg-usuario.
            
                        ASSIGN wh-wmeq0506-bt-mod:SENSITIVE = NO
                               wh-wmeq0506-bt-del:SENSITIVE = NO
                               wh-wmeq0506-btcancela:SENSITIVE = YES
                               wh-wmeq0506-bt-wms:SENSITIVE = NO.
                    END.
            
                    ELSE DO:
                        CREATE esp-resumo-wms.
                        ASSIGN esp-resumo-wms.nr-embarque = int(wh-wmeq0506-nr-embarque:SCREEN-VALUE)
                               esp-resumo-wms.nr-resumo   = res-cli.nr-resumo
                               esp-resumo-wms.enviado     = YES
                               esp-resumo-wms.dt-envio    = TODAY
                               esp-resumo-wms.cod-usuar   = c-seg-usuario
                               esp-resumo-wms.confirmado  = NO
                               esp-resumo-wms.cancelado   = NO.
            
                        ASSIGN wh-wmeq0506-bt-mod:SENSITIVE = NO
                               wh-wmeq0506-bt-del:SENSITIVE = NO
                               wh-wmeq0506-btcancela:SENSITIVE = YES
                               wh-wmeq0506-bt-wms:SENSITIVE = NO.
            
                    END.
                END.
            end.    
        END.
    end.    
END.

IF p-ind-event = "choose-btcancela" THEN
DO:

    def var v-cdd-embarq like embarque.cdd-embarq no-undo.
    def var v-nr-resumo  like res-cli.nr-resumo   no-undo.
    def var v-nr-pedido  like ped-venda.nr-pedido no-undo.

    /*if not can-find(first usuar_grp_usuar where
                          usuar_grp_usuar.cod_usuario   = c-seg-usuario and
                          usuar_grp_usuar.cod_grp_usuar = "MFP")        then do:
                          
        run utp/ut-msgs.p (input "show",
                           input 17006,
                           input "Usu†rio sem permiss∆o~~Usu†rio n∆o possui permiss∆o para cancelar embarques e resumos":U).
        return no-apply.                   
                    
    end.*/
    
    current-window:sensitive = false.
        
    run upc/u01eq0506a.w (output i-tp-cancel).
    
    current-window:sensitive = true.
    
    if i-tp-cancel <> 0 then do:
        case i-tp-cancel:
            when 1 then
                run utp/ut-msgs.p (input "show", input 27100, input "Cancelar Embarque?~~Deseja realmente cancelar o Embarque?").
            
            when 2 then
                run utp/ut-msgs.p (input "show", input 27100, input "Cancelar e Desalocar Embarque?~~Deseja realmente cancelar e desalocar o Embarque?").
            
            when 3 then
                run utp/ut-msgs.p (input "show", input 27100, input "Cancelar Resumo?~~Deseja realmente cancelar o Resumo selecionado?").
            
            when 4 then 
                run utp/ut-msgs.p (input "show", input 27100, input "Cancelar Resumo e Pedido?~~Deseja realmente cancelar o Resumo e o Pedido selecionado?").
            
            when 5 then 
                run utp/ut-msgs.p (input "show", input 27100, input "Cancelar e Desalocar Resumo?~~Deseja realmente cancelar e desalocar o Resumo selecionado?").
                
        end.
        
        
        if return-value = "yes" then do:                    
    
            if valid-handle(wh-wmeq0506-nr-embarque) then do:
    
                find embarque no-lock where
                     embarque.cdd-embarq = int(wh-wmeq0506-nr-embarque:screen-value) no-error.
                     
                assign v-cdd-embarq = embarque.cdd-embarq.     

                if i-tp-cancel = 1 or i-tp-cancel = 2 then do:

                    for each res-cli no-lock where
                             res-cli.cdd-embarq = embarque.cdd-embarq:
                             
                        assign v-nr-resumo = res-cli.nr-resumo.

                        find esp-resumo-wms no-lock where
                             esp-resumo-wms.nr-embarque = int(res-cli.cdd-embarq) and
                             esp-resumo-wms.nr-resumo   = res-cli.nr-resumo  no-error.
                        if avail esp-resumo-wms         and 
                           not esp-resumo-wms.cancelado then do:

                            if res-cli.situacao = 3 then do:
                                run esp/eswms007.p (input string(res-cli.cdd-embarq) + "|" + string(res-cli.nr-resumo),
                                                    input "Cancelar").
                                /*else
                                    run esp/eswms008.p (buffer embarque).*/
        
                                if i-tp-cancel = 2 then do: /*desaloca*/
    
                                    run eqp/eqapi300.p persistent set h-eqapi300.
    
                                    empty temp-table tt-ped-venda.
                                    empty temp-table tt-it-pre-fat.
    
                                    for each it-pre-fat no-lock where
                                             it-pre-fat.cdd-embarq = res-cli.cdd-embarq and
                                             it-pre-fat.nr-resumo  = res-cli.nr-resumo:
    
                                        find ped-venda no-lock where
                                             ped-venda.nome-abrev = it-pre-fat.nome-abrev and
                                             ped-venda.nr-pedcli  = it-pre-fat.nr-pedcli  no-error.
                                        if avail ped-venda then do:
                                            if not ped-venda.ind-fat-par then do:
                                                if not can-find(first tt-ped-venda where
                                                                      tt-ped-venda.nome-abrev = it-pre-fat.nome-abrev and
                                                                      tt-ped-venda.nr-pedcli  = it-pre-fat.nr-pedcli) then do:
                                                    create tt-ped-venda.
                                                    assign tt-ped-venda.i-sequen     = i-sequen
                                                           tt-ped-venda.nome-abrev   = it-pre-fat.nome-abrev
                                                           tt-ped-venda.nr-pedcli    = it-pre-fat.nr-pedcli
                                                           tt-ped-venda.cdd-embarq   = it-pre-fat.cdd-embarq
                                                           tt-ped-venda.ind-oper     = 2
                                                           i-sequen                  = i-sequen + 1.
                                                end.
                                            end.
                                            else do:
                                                create tt-it-pre-fat.
                                                buffer-copy it-pre-fat to tt-it-pre-fat.
                                                assign tt-it-pre-fat.qt-a-alocar = it-pre-fat.qt-alocada * -1.
                                            end.
                                        end.
                                    end.                                
    
                                    run pi-recebe-tt-ped-venda  in h-eqapi300 (input table tt-ped-venda).
                                    run pi-trata-tt-ped-venda   in h-eqapi300 (input yes).
                                
                                    run pi-recebe-tt-it-pre-fat in h-eqapi300 (input table tt-it-pre-fat).
                                    run pi-trata-tt-it-pre-fat  in h-eqapi300 (input no).
                                    run pi-devolve-tt-erro      in h-eqapi300 (output table tt-erro).
    
                                    if  can-find(first tt-erro) then
                                        run cdp/cd0667.w (input table tt-erro).
                                    else do:
                                        find esp-resumo-wms exclusive-lock where
                                             esp-resumo-wms.nr-embarque = int(v-cdd-embarq) and
                                             esp-resumo-wms.nr-resumo   = v-nr-resumo  no-error.
                                        if avail esp-resumo-wms then do:
                                            assign esp-resumo-wms.cancelado = yes.
        
                                            create esp-res-wms-canc.
                                            assign esp-res-wms-canc.nr-embarque = esp-resumo-wms.nr-embarque
                                                   esp-res-wms-canc.nr-resumo   = esp-resumo-wms.nr-resumo
                                                   esp-res-wms-canc.cod-usuario = c-seg-usuario
                                                   esp-res-wms-canc.dt-canc     = today
                                                   esp-res-wms-canc.tp-canc     = i-tp-cancel.
                                                   
                                            run utp/ut-msgs.p (input 'show',
                                                               input 15825,
                                                               input 'Conclu°do com Sucesso!').       
                                        end.
                                    end.
    
                                    delete procedure h-eqapi300.
                                end.
                                else do:
                                
                                    find esp-resumo-wms exclusive-lock where
                                         esp-resumo-wms.nr-embarque = int(v-cdd-embarq) and
                                         esp-resumo-wms.nr-resumo   = v-nr-resumo  no-error.
                                    if avail esp-resumo-wms then do:
                                        assign esp-resumo-wms.cancelado = yes.
      
                                        create esp-res-wms-canc.
                                        assign esp-res-wms-canc.nr-embarque = esp-resumo-wms.nr-embarque
                                               esp-res-wms-canc.nr-resumo   = esp-resumo-wms.nr-resumo
                                               esp-res-wms-canc.cod-usuario = c-seg-usuario
                                               esp-res-wms-canc.dt-canc     = today
                                               esp-res-wms-canc.tp-canc     = i-tp-cancel.
                                               
                                        run utp/ut-msgs.p (input 'show',
                                                           input 15825,
                                                           input 'Conclu°do com Sucesso!').       
                    
                                    end.                          
                                end.
                            end.
                        end.
                    end.
                end.
                else do:
                    find res-cli where
                        rowid(res-cli) = r-res-cli-eq0506 no-lock no-error.
                    
                    if res-cli.situacao = 3 then do:
                    
                        assign v-nr-resumo = res-cli.nr-resumo.
                        
                        run esp/eswms007.p (input string(res-cli.cdd-embarq) + "|" + string(res-cli.nr-resumo),
                                            input "Cancelar").
                        /*else
                            run esp/eswms008.p (buffer embarque).*/
    
                        if i-tp-cancel = 4 or
                           i-tp-cancel = 5 then do:
    
                            /*Desaloca*/
                            run eqp/eqapi300.p persistent set h-eqapi300.
    
                            empty temp-table tt-ped-venda.
                            empty temp-table tt-it-pre-fat.
    
                            for each it-pre-fat no-lock where
                                     it-pre-fat.cdd-embarq = res-cli.cdd-embarq and
                                     it-pre-fat.nr-resumo  = res-cli.nr-resumo:
                                     
                                find ped-venda no-lock where
                                     ped-venda.nome-abrev = it-pre-fat.nome-abrev and
                                     ped-venda.nr-pedcli  = it-pre-fat.nr-pedcli  no-error.
                                if avail ped-venda then do:
                                    if not ped-venda.ind-fat-par then do:
                                    
                                        assign v-nr-pedido = ped-venda.nr-pedido.
                                    
                                        if not can-find(first tt-ped-venda where
                                                              tt-ped-venda.nome-abrev = it-pre-fat.nome-abrev and
                                                              tt-ped-venda.nr-pedcli  = it-pre-fat.nr-pedcli) then do:
                                            create tt-ped-venda.
                                            assign tt-ped-venda.i-sequen     = i-sequen
                                                   tt-ped-venda.nome-abrev   = it-pre-fat.nome-abrev
                                                   tt-ped-venda.nr-pedcli    = it-pre-fat.nr-pedcli
                                                   tt-ped-venda.cdd-embarq   = it-pre-fat.cdd-embarq
                                                   tt-ped-venda.ind-oper     = 2
                                                   i-sequen                  = i-sequen + 1.
                                        end.
                                    end.
                                    else do:
                                    
                                        create tt-it-pre-fat.
                                        buffer-copy it-pre-fat to tt-it-pre-fat.
                                        assign tt-it-pre-fat.qt-a-alocar = it-pre-fat.qt-alocada * -1.
                                    end.
                                end.
                            end.      
                            
                            run pi-recebe-tt-ped-venda  in h-eqapi300 (input table tt-ped-venda).
                            run pi-trata-tt-ped-venda   in h-eqapi300 (input yes).
                            
                            run pi-recebe-tt-it-pre-fat in h-eqapi300 (input table tt-it-pre-fat).
                            run pi-trata-tt-it-pre-fat  in h-eqapi300 (input no).
                            run pi-devolve-tt-erro      in h-eqapi300 (output table tt-erro).
                            
                            delete procedure h-eqapi300.
    
                            if  can-find(first tt-erro) then
                                run cdp/cd0667.w (input table tt-erro).
                            else do:
                            
                                /*Cancelar Pedido*/
                                if i-tp-cancel = 4 then do:
                                    if not valid-handle(bo-ped-venda-can) then
                                       run dibo/bodi159can.p persistent set bo-ped-venda-can.
                                       
                                    find ped-venda no-lock where
                                         ped-venda.nr-pedido = v-nr-pedido no-error.
        
                                    run validateCancelation in bo-ped-venda-can (input  rowid(ped-venda),
                                                                                 output table Rowerrors).
                                    if  session:set-wait-state("") then.
        
                                    if  can-find(first RowErrors) then do:
                                        find first RowErrors no-error.
                                        run utp/ut-msgs.p (input "show",
                                                           input RowErrors.ErrorNumber,
                                                           input RowErrors.ErrorDescription).
                                        return "NOK".            
                                    end. 
        
                                    if  not can-find(first RowErrors
                                                     where RowErrors.ErrorSubType = "Error":U) then do:
        
                                        if  session:set-wait-state("general") then.
        
                                        run inputReopenQuotation in bo-ped-venda-can(input no).
        
                                        run updateCancelation in bo-ped-venda-can(input rowid(ped-venda),
                                                                                  input "Cancelamento WMS",
                                                                                  input today,
                                                                                  input 1).
        
                                        run getRowErrors in bo-ped-venda-can (output table RowErrors). 
        
                                        if  can-find(first RowErrors) then do:
                                            find first RowErrors no-error.
                                            run utp/ut-msgs.p (input "show",
                                                               input RowErrors.ErrorNumber,
                                                               input RowErrors.ErrorDescription).
                                            return "NOK".
                                        end.
                                        
                                        if valid-handle(bo-ped-venda-can)  then do:
                                            delete procedure bo-ped-venda-can.
                                            assign bo-ped-venda-can = ?.
                                        end.
        
                                        if session:set-wait-state("") then.
        
                                    end.
                                    
                                    if can-find(ped-venda where
                                                ped-venda.nome-abrev = it-pre-fat.nome-abrev and
                                                ped-venda.nr-pedcli  = it-pre-fat.nr-pedcli) then do:
                                                
                                        run utp/ut-msgs.p (input "show",
                                                           input 17006,
                                                           input 'Pedido n∆o foi eliminado!').
                                                
                                    end.                                    
                                end.
    
                                if  not can-find(first RowErrors) then do:
                                    find esp-resumo-wms exclusive-lock where
                                         esp-resumo-wms.nr-embarque = int(v-cdd-embarq) and
                                         esp-resumo-wms.nr-resumo   = v-nr-resumo  no-error.
                                    if avail esp-resumo-wms then do:
                                        assign esp-resumo-wms.cancelado = yes.
            
                                        create esp-res-wms-canc.
                                        assign esp-res-wms-canc.nr-embarque = esp-resumo-wms.nr-embarque
                                               esp-res-wms-canc.nr-resumo   = esp-resumo-wms.nr-resumo
                                               esp-res-wms-canc.cod-usuario = c-seg-usuario
                                               esp-res-wms-canc.dt-canc     = today
                                               esp-res-wms-canc.tp-canc     = i-tp-cancel.
                                               
                                        run utp/ut-msgs.p (input 'show',
                                                           input 15825,
                                                           input 'Conclu°do com Sucesso!').       

            
                                    end.
                                end.
                            end.
                        end.
                        else do:
                            find esp-resumo-wms exclusive-lock where
                                 esp-resumo-wms.nr-embarque = int(v-cdd-embarq) and
                                 esp-resumo-wms.nr-resumo   = v-nr-resumo  no-error.
                            if avail esp-resumo-wms then do:
                                assign esp-resumo-wms.cancelado = yes.
        
                                create esp-res-wms-canc.
                                assign esp-res-wms-canc.nr-embarque = esp-resumo-wms.nr-embarque
                                       esp-res-wms-canc.nr-resumo   = esp-resumo-wms.nr-resumo
                                       esp-res-wms-canc.cod-usuario = c-seg-usuario
                                       esp-res-wms-canc.dt-canc     = today
                                       esp-res-wms-canc.tp-canc     = i-tp-cancel.
                                       
                                run utp/ut-msgs.p (input 'show',
                                                   input 15825,
                                                   input 'Conclu°do com Sucesso!').       

        
                            end.
                        end.
                    end.
                end.
            end.

            run dispatch in wh-wmeq0506-browse1 (input "open-query").
    
            assign wh-wmeq0506-btcancela:sensitive = no.
        end.
    end.
end.

PROCEDURE pi-trata-botoes:

    IF VALID-HANDLE(wh-wmeq0506-nr-embarque) AND r-res-cli-eq0506 <> ? THEN DO:    

        FIND FIRST embarque NO-LOCK
             WHERE embarque.cdd-embarq = int(wh-wmeq0506-nr-embarque:SCREEN-VALUE) NO-ERROR.
        
        find first esp-param-integr-wms no-lock no-error.
        if not avail esp-param-integr-wms or 
           not can-do(esp-param-integr-wms.lista-estab-integr,embarque.cod-estabel) THEN DO:
            wh-wmeq0506-bt-wms:SENSITIVE = NO.
            return "OK".
        END.
        
        /*find first it-dep-fat of embarque no-lock no-error.
        if not avail it-dep-fat or 
            not can-do(esp-param-integr-wms.lista-depos-integr,it-dep-fat.cod-depos) then do:
            wh-wmeq0506-bt-wms:SENSITIVE = NO.
            return "OK".
        end.        */

        wh-wmeq0506-bt-wms:SENSITIVE = YES.

        /*adicionar regra do resumo e verificar se resumo j† foi faturado*/
        FOR FIRST res-cli WHERE
            ROWID(res-cli) = r-res-cli-eq0506 NO-LOCK:

            IF res-cli.situacao > 3 THEN DO:
                ASSIGN wh-wmeq0506-bt-mod   :SENSITIVE = NO
                       wh-wmeq0506-bt-del   :SENSITIVE = NO
                       wh-wmeq0506-btcancela:SENSITIVE = NO
                       wh-wmeq0506-bt-wms   :SENSITIVE = NO.

            END.
            ELSE DO:
                FIND FIRST esp-resumo-wms WHERE
                    esp-resumo-wms.nr-embarque = int(wh-wmeq0506-nr-embarque:SCREEN-VALUE) AND
                    esp-resumo-wms.nr-resumo   = res-cli.nr-resumo NO-LOCK NO-ERROR.
                IF AVAIL esp-resumo-wms AND esp-resumo-wms.enviado AND NOT esp-resumo-wms.cancelado THEN DO:
                    ASSIGN wh-wmeq0506-bt-mod:SENSITIVE = NO
                           wh-wmeq0506-bt-del:SENSITIVE = NO
                           wh-wmeq0506-btcancela:SENSITIVE = YES
                           wh-wmeq0506-bt-wms:SENSITIVE = NO.
                END.
                ELSE IF AVAIL esp-resumo-wms then do:
                    if esp-resumo-wms.cancelado AND esp-resumo-wms.enviado THEN do:
                        ASSIGN wh-wmeq0506-bt-mod:SENSITIVE = NO
                               wh-wmeq0506-bt-del:SENSITIVE = NO
                               wh-wmeq0506-btcancela:SENSITIVE = NO
                               wh-wmeq0506-bt-wms:SENSITIVE = YES.                               
                               
                        /*if not can-find(first usuar_grp_usuar where
                                          usuar_grp_usuar.cod_usuario    = c-seg-usuario and
                                         (usuar_grp_usuar.cod_grp_usuar = "SAC"or
                                          usuar_grp_usuar.cod_grp_usuar = "GDD"))        then do:
                                          
                            find first pre-fatur no-lock where
                                       pre-fatur.cdd-embarq = res-cli.cdd-embarq and
                                       pre-fatur.nr-resumo  = res-cli.nr-resumo  no-error.
                            if avail pre-fatur then do:
                            
                                find ped-venda no-lock where
                                     ped-venda.nome-abrev = pre-fatur.nome-abrev and
                                     ped-venda.nr-pedcli  = pre-fatur.nr-pedcli  no-error.
                                if avail ped-venda then do:
                                
                                    find imp_pedidos no-lock where
                                         imp_pedidos.nr-pedido = ped-venda.nr-pedido no-error.
                                    if avail imp_pedidos and
                                       imp_pedidos.envio then
                                        assign wh-wmeq0506-bt-wms:SENSITIVE = YES.
                                    else
                                        assign wh-wmeq0506-bt-wms:SENSITIVE = NO. 
                                       
                                end.
                            end.
                        end.
                        else
                            assign wh-wmeq0506-bt-wms:SENSITIVE = YES.*/
                    end.           
                    else
                        if esp-resumo-wms.confirmado then
                            wh-wmeq0506-btcancela:SENSITIVE = yes.
                end.
                ELSE do:
                    ASSIGN wh-wmeq0506-bt-mod:SENSITIVE = YES
                           wh-wmeq0506-bt-del:SENSITIVE = YES
                           wh-wmeq0506-btcancela:SENSITIVE = NO
                           wh-wmeq0506-bt-wms:SENSITIVE = YES.
                           
                    /*if not can-find(first usuar_grp_usuar where
                                      usuar_grp_usuar.cod_usuario    = c-seg-usuario and
                                     (usuar_grp_usuar.cod_grp_usuar = "SAC" or
                                      usuar_grp_usuar.cod_grp_usuar = "GDD"))        then do:
                                      
                        find first pre-fatur no-lock where
                                   pre-fatur.cdd-embarq = res-cli.cdd-embarq and
                                   pre-fatur.nr-resumo  = res-cli.nr-resumo  no-error.
                        if avail pre-fatur then do:
                      
                            find ped-venda no-lock where
                                 ped-venda.nome-abrev = pre-fatur.nome-abrev and
                                 ped-venda.nr-pedcli  = pre-fatur.nr-pedcli  no-error.
                            if avail ped-venda then do:
                           
                                find imp_pedidos no-lock where
                                     imp_pedidos.nr-pedido = ped-venda.nr-pedido no-error.
                                if avail imp_pedidos and
                                   imp_pedidos.envio then
                                    assign wh-wmeq0506-bt-wms:SENSITIVE = YES.
                                else
                                    assign wh-wmeq0506-bt-wms:SENSITIVE = NO.   
                                 
                            end.
                        end.
                    end.
                    else
                        assign wh-wmeq0506-bt-wms:SENSITIVE = YES.  */     
                end.           
            END.
            
        END.

        IF p-wgh-object:FILE-NAME <> "eqp/eq0506-b03.w" AND valid-handle(wh-wmeq0506-bt-wms) THEN DO:
            ASSIGN wh-wmeq0506-bt-wms   :SENSITIVE = NO
                   wh-wmeq0506-btcancela:SENSITIVE = NO.
        END.

    END.
    ELSE IF VALID-HANDLE(wh-wmeq0506-bt-wms) THEN
        ASSIGN wh-wmeq0506-bt-wms   :SENSITIVE = NO
               wh-wmeq0506-btcancela:SENSITIVE = NO
               wh-wmeq0506-bt-mod   :SENSITIVE = YES
               wh-wmeq0506-bt-del   :SENSITIVE = YES.
END.

