&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*           This .W file was created with the Progress AppBuilder.     */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{utp/utapi019.i}

{include/i-freeac.i}

DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER
    FIELD ErrorParameters  AS CHARACTER
    FIELD ErrorType        AS CHARACTER
    FIELD ErrorHelp        AS CHARACTER
    FIELD ErrorSubType     AS CHARACTER.

def temp-table tt-ped-item-cotas no-undo
    field nr-sequencia as int 
    field it-codigo    as char
    field cod-refer    as char.

def buffer bf-es-pend-mlapp for es-pend-mlapp.
def buffer bf-ped-venda for ped-venda.
def buffer bf-es-acordo-pendencia for es-acordo-pendencia.

def var iErro as int initial 1 no-undo.
def var h-bodi261 as handle no-undo.

{cdp/cdapi013.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 14.17
         WIDTH              = 60.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{src/web2/wrap-cgi.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ************************  Main Code Block  *********************** */

/* Process the latest Web event. */
RUN process-web-request.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-outputHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE outputHeader Procedure 
PROCEDURE outputHeader :
output-http-header("Expires":U, "Mon, 01 Nov 1971 00:00:00 GMT":U).
    output-http-header("Pragma":U, "no-cache":U).
    output-http-header("Cache-Control":U, "no-cache":U).
 
    output-http-header("Last-Modified":U,DYNAMIC-FUNCTION("format-datetime":U,
                                                          "HTTP":U,
                                                           TODAY,
                                                           TIME,
                                                           "LOCAL":U)).

    
    output-http-header("Access-Control-Allow-Origin", "*").
    output-http-header("Content-Type":U, "application/json":U).

    output-http-header("charset":U, "UTF-8":U).
   
    /*output-http-header("Content-Length":U, STRING(LENGTH(cJSON,"RAW":U))).*/
    
    output-http-header("":U, "":U).

  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-envia-email) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-envia-email Procedure 
PROCEDURE pi-envia-email :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param c-status as char no-undo.
def input param c-des-faixa as char no-undo.

find repres no-lock where
     repres.nome-abrev = ped-venda.no-ab-reppri no-error.

find emitente no-lock where
     emitente.nome-abrev = ped-venda.nome-abrev no-error.

find first esp-perfil-usuar no-lock where
           esp-perfil-usuar.codigo-perfil = string(repres.cod-rep) no-error.
if avail esp-perfil-usuar then do:
    find mla-usuar-aprov no-lock where
         mla-usuar-aprov.cod-usuar = esp-perfil-usuar.cod-usuario no-error.
    if avail mla-usuar-aprov then do:

        empty temp-table tt-envio2.        

        FIND FIRST es-tipo-docto
             WHERE es-tipo-docto.tp-docto = es-pend-mlapp.tp-docto NO-LOCK NO-ERROR. 
        
        create tt-envio2.
        assign tt-envio2.versao-integracao = 1
               tt-envio2.exchange          = no
               tt-envio2.remetente         = "pedidoweb@bonyplus.com.br"
               tt-envio2.destino           = mla-usuar-aprov.e-mail
               tt-envio2.servidor          = param-global.serv-mail
               tt-envio2.porta             = param-global.porta-mail
               tt-envio2.assunto           = "Pedido - " + string(ped-venda.nr-pedido)
               tt-envio2.formato           = "Texto"
               tt-envio2.mensagem          = "Pedido " + es-tipo-docto.desc-docto + " - " + string(ped-venda.nr-pedido) + chr(10) +
                                             "Cliente: " + string(emitente.cod-emitente) + " " + string(emitente.nome-emit) + chr(10) + chr(10) +
                                             c-status + " pela Regra Comercial" + chr(10) +
                                             "Faixa: " + c-des-faixa + chr(10) +
                                             "Motivo: " + if c-status = "Aprovado" then es-pend-mlapp.narrativa-aprov else es-pend-mlapp.narrativa-rejei + chr(10) + chr(10) + 
                                             "Esse ‚ um e-mail autom tico, favor nÆo responder para pedidoweb@bonyplus.com.br.".    
                                                           
        run utp/utapi019.p persistent set h-utapi019.

        run pi-execute in h-utapi019 (input table tt-envio2,
                                      output table tt-erros).    
        
        if return-value = "NOK" then do:
            find first tt-erros no-error.   
            if avail tt-erros then
                message tt-erros.desc-erro.
            
        end.
    end.
end.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-process-web-request) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE process-web-request Procedure 
PROCEDURE process-web-request :
/*------------------------------------------------------------------------------
  Purpose:     Process the web request.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  /* 
   * Output the MIME header and set up the object as state-less or state-aware. 
   * This is required if any HTML is to be returned to the browser.
   */
    DEFINE VARIABLE cResult AS CHARACTER   NO-UNDO.

    WEB-CONTEXT:html-charset = "UTF-8".

    DEFINE VARIABLE c-usuario AS CHARACTER   NO-UNDO.

    assign c-usuario = get-value("username").
    
    DEFINE VARIABLE h-mlainterfac AS HANDLE      NO-UNDO.
    DEFINE VARIABLE cNrTrans AS int     NO-UNDO.
    DEFINE VARIABLE acao AS INTEGER     NO-UNDO.

    /**  Login Integrado
    **/
   
    DEFINE VARIABLE dominioSO                   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE usuarioSO                   AS CHARACTER NO-UNDO.
    
    def var c-action  as char no-undo.
    def var c-remarks as char no-undo.
    def var c-appType as char no-undo.    

    find first param-global no-lock no-error.

    assign c-action  = trim(get-value('action'))
           c-remarks = trim(get-value('remarks'))
           c-appType = get-value('appType').

    message 'user - ' get-value("username") skip
            'trans - ' get-value('nrTrans') skip
            'action - ' c-action skip
            'remarks - ' c-remarks skip
            'appType - ' c-appType skip.

    assign cNrTrans = int(get-value('nrTrans')).

    if c-action = "Aprovar" then
       assign acao = 1.
    else 
       if c-action = "Rejeitar" then
           assign acao = 2.
       else 
           assign acao = 0.
    
    if c-appType = 'Acordo' then do:
        find es-acordo-pendencia exclusive-lock where
             es-acordo-pendencia.nr-acordo-comerc = STRING(cNrTrans)  and
             es-acordo-pendencia.cod-aprovador    = c-usuario no-error.
        if avail es-acordo-pendencia then do:

            if c-action = "Aprovar" then do:

                assign es-acordo-pendencia.ind-situacao = 1
                       es-acordo-pendencia.dt-aprov     = today
                       es-acordo-pendencia.des-just     = c-remarks.

                if not can-find(first bf-es-acordo-pendencia where
                                      bf-es-acordo-pendencia.nr-acordo-comerc  = es-acordo-pendencia.nr-acordo-comerc and
                                      bf-es-acordo-pendencia.ind-situacao     <> 1    and
                                      bf-es-acordo-pendencia.lg-aprov          = yes) then do:

                    for each bf-es-acordo-pendencia exclusive-lock where
                             bf-es-acordo-pendencia.nr-acordo-comerc = es-acordo-pendencia.nr-acordo-comerc:

                        assign bf-es-acordo-pendencia.ind-situacao = 1
                               bf-es-acordo-pendencia.dt-aprov     = today
                               bf-es-acordo-pendencia.des-just     = c-remarks.

                    end.

                end.

            end.
            else do:

                assign es-acordo-pendencia.ind-situacao = 2
                       es-acordo-pendencia.dt-reprov    = today
                       es-acordo-pendencia.des-just     = c-remarks.

                if not can-find(first bf-es-acordo-pendencia where
                                      bf-es-acordo-pendencia.nr-acordo-comerc  = es-acordo-pendencia.nr-acordo-comerc and
                                      bf-es-acordo-pendencia.ind-situacao     <> 2    and
                                      bf-es-acordo-pendencia.lg-aprov          = yes) then do:


                    for each bf-es-acordo-pendencia exclusive-lock where
                             bf-es-acordo-pendencia.nr-acordo-comerc = es-acordo-pendencia.nr-acordo-comerc:
    
                        assign bf-es-acordo-pendencia.ind-situacao = 2
                               bf-es-acordo-pendencia.dt-reprov    = today
                               bf-es-acordo-pendencia.des-just     = c-remarks.
                    end.
                end.
            end.
        end.
    end.
    else do:
        find es-pend-mlapp exclusive-lock where
             es-pend-mlapp.nr-pendencia = cNrTrans no-error.
        if avail es-pend-mlapp then do:
    
            find ped-venda exclusive-lock where
                 ped-venda.nr-pedido = es-pend-mlapp.nr-pedido no-error.
            if avail ped-venda then do:
    
                if c-action = "Aprovar" then do:
                
                    find es-ped-venda-bonif no-lock where
                         es-ped-venda-bonif.nome-abrev = ped-venda.nome-abrev and
                         es-ped-venda-bonif.nr-pedcli  = ped-venda.nr-pedcli  no-error.
                    if avail es-ped-venda-bonif then do:
                        find bf-ped-venda no-lock where
                             bf-ped-venda.nr-pedido = int(es-ped-venda-bonif.nr-pedbonif) no-error.
                        if avail bf-ped-venda then do:
                            if can-find(first es-pend-mlapp where
                                              es-pend-mlapp.nr-pedido    = bf-ped-venda.nr-pedido and
                                              es-pend-mlapp.ind-situacao = 1)                     then do:
                                create RowErrors.
                                assign RowErrors.ErrorNumber      = 17006
                                       RowErrors.ErrorDescription = "Pedido de Venda " + string(bf-ped-venda.nr-pedido) + " Pendente de Aprova‡Æo".                
                            end.
                            
                            if can-find(first es-pend-mlapp where
                                              es-pend-mlapp.nr-pedido    = bf-ped-venda.nr-pedido and
                                              es-pend-mlapp.ind-situacao = 3)                     then do:
                                if not can-find(first es-pend-mlapp where
                                                      es-pend-mlapp.nr-pedido    = bf-ped-venda.nr-pedido and
                                                      es-pend-mlapp.ind-situacao = 2) then do:
    
                                    create RowErrors.
                                    assign RowErrors.ErrorNumber      = 17006
                                           RowErrors.ErrorDescription = "Pedido de Venda " + string(bf-ped-venda.nr-pedido) + " Reprovado".
                                end.
                            end.                  
                        end.         
                    end.                    
                    
                    if not can-find(first RowErrors) then do:
                        assign es-pend-mlapp.dt-aprova       = today
                               es-pend-mlapp.ind-situacao    = 2
                               es-pend-mlapp.narrativa-aprov = c-remarks.
        
                        if not can-find(first bf-es-pend-mlapp where
                                              bf-es-pend-mlapp.nr-pedido     = es-pend-mlapp.nr-pedido and
                                              bf-es-pend-mlapp.ind-situacao  = 1                       and
                                              bf-es-pend-mlapp.aprovador     = yes)                    then do:
        
                            for each bf-es-pend-mlapp exclusive-lock where
                                     bf-es-pend-mlapp.nr-pedido    = ped-venda.nr-pedido and
                                     bf-es-pend-mlapp.ind-situacao = 1                   and
                                     bf-es-pend-mlapp.aprovador    = no:
        
                                assign bf-es-pend-mlapp.dt-aprova       = today
                                       bf-es-pend-mlapp.ind-situacao    = 2
                                       bf-es-pend-mlapp.narrativa-aprov = c-remarks.
                            end.
        
                            find esp-ped-venda exclusive-lock where 
                                 esp-ped-venda.nome-abrev = ped-venda.nome-abrev and 
                                 esp-ped-venda.nr-pedcli  = ped-venda.nr-pedcli  no-error.
                            if avail esp-ped-venda then do:
                                assign esp-ped-venda.situacao-aprov    = 2
                                       esp-ped-venda.cod-usuario-comer = es-pend-mlapp.cod-aprovador
                                       esp-ped-venda.data-aprov        = today. 
    
                                if substring(esp-ped-venda.nr-pedcli, length(esp-ped-venda.nr-pedcli), 1) = "b" then
                                    assign esp-ped-venda.tipo-pend = 5.
                            end.
                            
                            find emitente no-lock where 
                                 emitente.cod-emitente = ped-venda.cod-emitente no-error.
                            
                            create tt-param-aval.
                            assign tt-param-aval.nr-pedido     = ped-venda.nr-pedido
                                   tt-param-aval.param-aval    = if emitente.ind-aval = 2 then 1 else 3
                                   tt-param-aval.embarque      = no
                                   tt-param-aval.efetiva       = yes 
                                   tt-param-aval.retorna       = yes 
                                   tt-param-aval.reavalia-forc = yes   
                                   tt-param-aval.vl-a-aval     = ped-venda.vl-liq-abe
                                   tt-param-aval.usuario       = es-pend-mlapp.cod-aprovador
                                   tt-param-aval.programa      = "MLApp".
                        
                            run cdp/cdapi013.p (input-output table tt-param-aval,
                                                input-output table tt-erros-aval).
                        
                            for each tt-erros-aval where 
                                     tt-erros-aval.cod-emitente = emitente.cod-emitente:
                            
                                create RowErrors.
                                assign RowErrors.errorSequence    = iErro + 1
                                       RowErrors.ErrorNumber      = 99999
                                       RowErrors.ErrorType        = "EMS":U
                                       RowErrors.ErrorSubType     = "ERROR":U
                                       RowErrors.ErrorDescription = "Pedido " + string(ped-venda.nr-pedcli) + " Pendente de Aprova‡Æo pela  rea de Cr‚dito!"
                                       RowErrors.ErrorHelp        = "Pedido Pendente de Aprova‡Æo pela  rea de Cr‚dito!"
                                       iErro                      = iErro + 1.
                    
                                if avail esp-ped-venda then
                                    assign esp-ped-venda.area-aprov = 2.
                            end.
                
                            find first RowErrors no-error.
                            if not avail RowErrors then do:                        
                                /* Atualiza‡Æo de Cotas */
                                if avail param-global     and
                                   param-global.modulo-08 then do:
                                    
                                    if  not valid-handle(h-bodi261) or
                                        h-bodi261:type      <> "PROCEDURE":U or
                                        h-bodi261:file-name <> "dibo/bodi261.p":U then
                                        run dibo/bodi261.p persistent set h-bodi261 no-error.
                            
                                    if ped-venda.cod-sit-com <> 1 then 
                                        run setarLogAtualizacao in h-bodi261. 
                
                                    run atualizarCotasPedido in h-bodi261(input rowid(ped-venda),
                                                                          input 1, /*atualiza*/
                                                                          input table tt-ped-item-cotas).
                            
                                    if valid-handle(h-bodi261) then do:
                                        delete procedure h-bodi261.
                                        assign h-bodi261 = ?.
                                    end.    
                                end.
                            
                                assign ped-venda.desc-forc-cr       = c-remarks
                                       ped-venda.dt-apr-cred        = today
                                       ped-venda.cod-sit-aval       = 3
                                       ped-venda.quem-aprovou       = "Sistema"
                                       ped-venda.dsp-pre-fat        = yes
                                       ped-venda.cod-message-alerta = ? 
                                       ped-venda.dt-mensagem        = ?.                
                    
                                validate ped-venda.
            
                            end.
    
                            find first es-pend-mlapp-det no-lock where
                                       es-pend-mlapp-det.nr-pendencia = es-pend-mlapp.nr-pendencia no-error.  
                            if avail es-pend-mlapp-det then do:
                                find first es-fam-docto-aprov no-lock where
                                           es-fam-docto-aprov.fm-cod-com = es-pend-mlapp-det.fm-cod-com and
                                           es-fam-docto-aprov.cod-usuar = es-pend-mlapp.cod-aprovador   no-error.
                                if avail es-fam-docto-aprov then do:
                                    find es-fam-docto-faixa no-lock where
                                         es-fam-docto-faixa.fm-cod-com  = es-fam-docto-aprov.fm-cod-com  and
                                         es-fam-docto-faixa.cod-lotacao = es-fam-docto-aprov.cod-lotacao and
                                         es-fam-docto-faixa.tp-docto    = es-fam-docto-aprov.tp-docto    and
                                         es-fam-docto-faixa.num-faixa   = es-fam-docto-aprov.num-faixa   no-error.
                                    if avail es-fam-docto-faixa then do:
    
                                        run pi-envia-email (input "Aprovado",
                                                            input es-fam-docto-faixa.desc-faixa).
    
                                    end.
                                end.
                            end.
                        end.
                    end.    
                end.
                else do:
    
                    assign ped-venda.desc-bloq-cr = c-remarks
                           ped-venda.dsp-pre-fat  = no
                           ped-venda.dt-mensagem  = today.
             
                    for each bf-es-pend-mlapp exclusive-lock where
                             bf-es-pend-mlapp.nr-pedido = ped-venda.nr-pedido:
                        assign bf-es-pend-mlapp.dt-rejeita      = today
                               bf-es-pend-mlapp.ind-situacao    = 3
                               bf-es-pend-mlapp.narrativa-rejei = c-remarks.
                    end.
             
                    find esp-ped-venda exclusive-lock where 
                         esp-ped-venda.nome-abrev = ped-venda.nome-abrev and 
                         esp-ped-venda.nr-pedcli  = ped-venda.nr-pedcli  no-error.
                    if avail esp-ped-venda then do:
                        assign esp-ped-venda.cod-usuario-comer = es-pend-mlapp.cod-aprovador
                               esp-ped-venda.data-aprov        = today. 
    
                        if substring(esp-ped-venda.nr-pedcli, length(esp-ped-venda.nr-pedcli), 1) = "b" then
                            assign esp-ped-venda.tipo-pend = 5.
                    end.
             
                    /* Atualiza‡Æo de Cotas */
                    if avail param-global and
                       param-global.modulo-08 then do:
                        if  not valid-handle(h-bodi261) or
                            h-bodi261:type      <> "PROCEDURE":U or
                            h-bodi261:file-name <> "dibo/bodi261.p":U then
                            run dibo/bodi261.p persistent set h-bodi261 no-error.
             
                        if ped-venda.cod-sit-com <> 1 then
                            run setarLogAtualizacao in h-bodi261.
    
                        run atualizarCotasPedido in h-bodi261(input rowid(ped-venda),
                                                              input 1,
                                                              input table tt-ped-item-cotas). /* a bo verifica se est  
                                                                                                 reprov por cr‚dito */
                        if valid-handle(h-bodi261) then do:
                            delete procedure h-bodi261.
                            assign h-bodi261 = ?.
                        end.
                    end.
    
                    find first es-pend-mlapp-det no-lock where
                               es-pend-mlapp-det.nr-pendencia = es-pend-mlapp.nr-pendencia no-error.  
                    if avail es-pend-mlapp-det then do:
                        find first es-fam-docto-aprov no-lock where
                                   es-fam-docto-aprov.fm-cod-com = es-pend-mlapp-det.fm-cod-com and
                                   es-fam-docto-aprov.cod-usuar = es-pend-mlapp.cod-aprovador   no-error.
                        if avail es-fam-docto-aprov then do:
                            find es-fam-docto-faixa no-lock where
                                 es-fam-docto-faixa.fm-cod-com  = es-fam-docto-aprov.fm-cod-com  and
                                 es-fam-docto-faixa.cod-lotacao = es-fam-docto-aprov.cod-lotacao and
                                 es-fam-docto-faixa.tp-docto    = es-fam-docto-aprov.tp-docto    and
                                 es-fam-docto-faixa.num-faixa   = es-fam-docto-aprov.num-faixa   no-error.
                            if avail es-fam-docto-faixa then do:
    
                                run pi-envia-email (input "Reprovado",
                                                    input es-fam-docto-faixa.desc-faixa).
    
                            end.
                        end.
                    end.
                end.
            end.
        end.
    end.

    if can-find(first RowErrors ) then do:
        find first RowErrors no-lock no-error .
        if avail RowErrors then do:
            if acao = 1 then do:
                if RowErrors.ErrorNumber = 99999 then
                    cResult = '~{~"success~":~"true~",~"message~":~"' + RowErrors.ErrorDescription + '~"~}'.
                else
                    cResult = '~{~"success~":~"false~",~"message~":~"' + RowErrors.ErrorDescription + '~"~}'.
            end.
            else
                cResult = '~{~"success~":~"false~",~"message~":~"' + RowErrors.ErrorDescription + '~"~}'.
        end.
    end.
    else do:
        if acao = 1 then do:
            cResult = "~{~"success~":~"true~",~"message~":~"Pendˆncia aprovada com sucesso~"~}".

        end.
        else do:
            cResult = "~{~"success~":~"true~",~"message~":~"Pendˆncia rejeitada com sucesso~"~}".

        end.
    end.

    run outputHeader.

    cResult = codepage-convert(cResult,"UTF-8").
    {&out} cResult.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

