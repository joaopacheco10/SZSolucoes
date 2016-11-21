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

def var c-nome-abrev as char no-undo.
DEFINE VARIABLE d-dt-vencimento   AS DATE        NO-UNDO.
define variable c-chave as char no-undo.

def stream s-log.

{include/i-freeac.i}

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
    define variable lcDocuments  as longchar  no-undo.
    define variable i-ind-perfil as integer   no-undo.
    define variable c-cod-perfil as character no-undo.
    define variable c-usuario    as character no-undo.
    define variable c-nome-rep   as char      no-undo.
    
    def var c-tipo-doc    as char initial "Desconto,Bonifica‡Æo":U no-undo.
    def var d-aux-tot-fam as dec                                   no-undo.
    def var c-val-perc    as char                                  no-undo.
    def var c-nar-motiv   as char                                  no-undo.
    def var d-perc-fam    like es-pend-mlapp-det.perc-familia      no-undo.
    def var i-aux         as int                                   no-undo.
    def var idx-aux       as int                                   no-undo.
    
    def var c-des-pagto as char no-undo.

    assign c-usuario = get-value("username").

    assign c-usuario = replace(c-usuario, " ", "").

    message "###### - " c-usuario.

    web-context:html-charset = "UTF-8".

    assign lcDocuments = '~{"pendings": [' .

    for each es-pend-mlapp no-lock where
             es-pend-mlapp.ind-situacao  = 1 and
             es-pend-mlapp.cod-aprovador = trim(c-usuario),
        first ped-venda no-lock where
              ped-venda.nr-pedido  = es-pend-mlapp.nr-pedido and
              ped-venda.dt-cancela = ?
        break by es-pend-mlapp.tp-docto:

        message "# Pendencia - " es-pend-mlapp.nr-pendencia skip
                "# Pedido    - " es-pend-mlapp.nr-pedido skip.

        if first-of(es-pend-mlapp.tp-docto) then do:

            assign lcDocuments = lcDocuments + 
                                 '~{"type":~"' + trim(entry(es-pend-mlapp.tp-docto, c-tipo-doc)) + '~"' +
                                 ',"docNumber":~"' + string(es-pend-mlapp.tp-docto) + '~"' + 
                                 ',"index":~"' + string(idx-aux) + '~"' + 
                                 ',"documents": ['.

            assign idx-aux = idx-aux + 1.
        end.

        find emitente no-lock where
             emitente.nome-abrev = ped-venda.nome-abrev no-error.

        find usuar_mestre no-lock where
             usuar_mestre.cod_usuario = ped-venda.user-impl no-error.
        if avail usuar_mestre then
            assign c-nome-rep = usuar_mestre.nom_usuario.
        else
            assign c-nome-rep = usuar_mestre.nom_usuario.

        find es-ped-web no-lock where
             es-ped-web.nr-pedido = ped-venda.nr-pedido no-error.
        if avail es-ped-web then do:

            assign c-nar-motiv = "".
            
            do i-aux = 1 to length(es-ped-web.narrativa):
    
                assign c-nar-motiv = c-nar-motiv + substring(es-ped-web.narrativa,i-aux,1). 
    
            end.

            assign c-nar-motiv = replace(replace(replace(c-nar-motiv, '"', ''), chr(10), " "), CHR(92), CHR(47)).
        end.
        
        find cond-pagto no-lock where
             cond-pagto.cod-cond-pag = ped-venda.cod-cond-pag no-error.
        if avail cond-pagto then
            assign c-des-pagto = string(cond-pagto.qtd-dias-prazo-medio) + " - " + string(cond-pagto.num-parcelas).
        else
            assign c-des-pagto = "".    

        assign lcDocuments = lcDocuments +
                             '~{"id":' + string(es-pend-mlapp.nr-pendencia) + 
                             ',"docKey":"' + string(ped-venda.nr-pedido)  + '"' +
                             ',"docDate":~"' + c-des-pagto + '~"' +
                             ',"value":~"' + trim(string(ped-venda.vl-tot-ped,">>>,>>>,>>9.99")) + '~"' +
                             ',"user":~"' + c-nome-rep + '~"' +
                             ',"supplier":~"' + emitente.nome-emit + '~"' +
                             ',"obsPedido":~"' + c-nar-motiv + '~"' +
                             ',"showApproval":~"' + string(es-pend-mlapp.aprovador) + '~"' +
                             ',"family": ['.

        for each ped-item of ped-venda no-lock,
            first item no-lock where
                  item.it-codigo = ped-item.it-codigo
            break by item.fm-cod-com:
            
            if not can-find(first es-pend-mlapp-det where
                                  es-pend-mlapp-det.nr-pendencia = es-pend-mlapp.nr-pendencia and
                                  es-pend-mlapp-det.fm-cod-com   = item.fm-cod-com) then
                next.                  

            find es-pend-mlapp-det no-lock where
                 es-pend-mlapp-det.nr-pendencia = es-pend-mlapp.nr-pendencia and
                 es-pend-mlapp-det.fm-cod-com   = item.fm-cod-com            no-error.
            if avail es-pend-mlapp-det then
                assign d-perc-fam = es-pend-mlapp-det.perc-familia.

            if first-of (item.fm-cod-com) then do:

                find fam-comerc no-lock where
                     fam-comerc.fm-cod-com = item.fm-cod-com no-error.

                assign lcDocuments = lcDocuments +
                                     "~{" +
                                     "~"famDes~":" + "~"" + fam-comerc.descricao + "~"," +
                                     "~"typeDes~":" + "~"" + trim(entry(es-pend-mlapp.tp-docto, c-tipo-doc)) + "~"," +
                                     "~"percValue~":" + "~"" + string(d-perc-fam) + "~"," +
                                     '"items": ['.

                assign d-aux-tot-fam = 0.
                                        
            end.

            assign d-aux-tot-fam = d-aux-tot-fam + (ped-item.vl-preuni * ped-item.qt-pedida).

            if es-pend-mlapp.tp-docto = 1 then
                assign c-val-perc = trim(string(ped-item.per-des-item,"->>9.99")).
            else do:
                if avail es-pend-mlapp-det then
                    assign c-val-perc = trim(string(es-pend-mlapp-det.perc-ped,"->>9.99")).
                else
                    assign c-val-perc = "0".
            end.

            assign lcDocuments = lcDocuments +
                                 "~{" +
                                 "~"itemcode~":" + "~"" + string(item.it-codigo) + "~"," +
                                 "~"description~":" + "~"" + string(item.desc-item) + "~"," +
                                 "~"quantity~":" + "~"" + string(ped-item.qt-pedida) + "~"," +
                                 "~"value~":" + "~"" + trim(string(ped-item.vl-preuni,">>>,>>>,>>9.99")) + "~"," +
                                 "~"total~":" + "~"" + trim(string(ped-item.vl-preuni * ped-item.qt-pedida,">>>,>>>,>>9.99")) + "~"," +
                                 "~"type~":" + "~"" + trim(entry(es-pend-mlapp.tp-docto, c-tipo-doc)) + "~"," +
                                 "~"valPerc~":" + "~"" + c-val-perc + "~"" +
                                 "~},".

            if last-of (item.fm-cod-com) then do:

                if substring(lcDocuments,length(lcDocuments),1) = "," then
                    assign substring(lcDocuments,length(lcDocuments),1) = "".

                assign lcDocuments = lcDocuments + 
                                     "],~"famValue~":" + "~"" + trim(string(d-aux-tot-fam,">>>,>>>,>>9.99")) +
                                     "~"~},".

            end.
        end.

        if substring(lcDocuments,length(lcDocuments),1) = "," then
            assign substring(lcDocuments,length(lcDocuments),1) = "".

        assign lcDocuments = lcDocuments + ']~},'.

        
    
        if last-of(es-pend-mlapp.tp-docto) then do:
            if substring(lcDocuments,length(lcDocuments),1) = "," then
                assign substring(lcDocuments,length(lcDocuments),1) = "".
            
            
            assign lcDocuments = lcDocuments + ']~},'.
        end.
    end.

    if substring(lcDocuments,length(lcDocuments),1) = "," then
        assign substring(lcDocuments,length(lcDocuments),1) = "".
    
    assign lcDocuments = lcDocuments + ']~}'.

    output stream s-log to value("\\10.185.32.46\erp\wrk\log_getdocuments.txt").
    
    export stream s-log lcDocuments.

    output stream s-log close.

    RUN outputHeader.

    lcDocuments = codepage-convert(lcDocuments,"UTF-8").
    {&out-long} lcDocuments.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF






                          
