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
    def var lcAgreements as longchar  no-undo.
    def var c-usuario    as character no-undo.
    def var c-emit       as char      no-undo.
    def var c-repres     as char      no-undo.
    def var c-area       as char      no-undo.
    def var c-objetivo   as char      no-undo.
    def var c-pagto      as char      no-undo.
    def var c-canal      as char      no-undo.
    def var c-acao       as char      no-undo.
    def var d-dt-pend    as date      no-undo.
    def var d-dt-per-ini as date      no-undo.
    def var d-dt-per-fim as date      no-undo.
    def var d-dt-pagto   as date      no-undo.
    def var idx-aux      as int       no-undo.
    
    assign c-usuario = get-value("username").
           c-usuario = replace(c-usuario, " ", "").

    web-context:html-charset = "UTF-8".

    assign lcAgreements = '~{"pendings": [' . 

    if can-find(first es-acordo-pendencia where
                      es-acordo-pendencia.ind-situacao  = 0                and
                      es-acordo-pendencia.cod-aprovador = trim(c-usuario)) then do: 

        assign idx-aux = 0.

        for each es-acordo-pendencia no-lock where
                 es-acordo-pendencia.ind-situacao  = 0 and
                 es-acordo-pendencia.cod-aprovador = trim(c-usuario)
            break by es-acordo-pendencia.tp-docto:

            if first-of(es-acordo-pendencia.tp-docto) then do:

                find es-tipo-docto no-lock where
                     es-tipo-docto.tp-docto = es-acordo-pendencia.tp-docto no-error.
                if avail es-tipo-docto then
                    assign lcAgreements = lcAgreements + 
                                         '~{"type":~"' + es-tipo-docto.desc-docto + '~"' +
                                         ',"docNumber":~"' + string(es-tipo-docto.tp-docto) + '~"' + 
                                         ',"index":~"' + string(idx-aux) + '~"' + 
                                         ',"agreements": ['
                           idx-aux      = idx-aux + 1.
            end.

            find es-acordo-comerc no-lock where
                 es-acordo-comerc.nr-acordo-comerc = es-acordo-pendencia.nr-acordo-comerc no-error.
            if avail es-acordo-comerc then do:
                
                find emitente no-lock where
                     emitente.cod-emitente = es-acordo-comerc.cod-emitente no-error.
                if avail emitente then
                    assign c-emit = emitente.nome-emit.
                else
                    assign c-emit = 'Cliente'.

                find repres no-lock where
                     repres.cod-rep = es-acordo-comerc.cod-repres no-error.
                if avail repres then
                    assign c-repres = repres.nome.
                else
                    assign c-repres = 'Representante'.

                find es-acordo-area no-lock where
                     es-acordo-area.cod-area = es-acordo-comerc.cod-area no-error.
                if avail es-acordo-area then
                    assign c-area = es-acordo-area.descricao.
                else
                    assign c-area = es-acordo-area.descricao.
                
                find es-acordo-acao no-lock where
                     es-acordo-acao.cod-acao = es-acordo-comerc.cod-acao no-error.
                if avail es-acordo-acao then
                    assign c-acao = es-acordo-acao.descricao.
                else
                    assign c-acao = 'Acao'.

                assign c-objetivo = replace(replace(es-acordo-comerc.objetivo, '"', ''), chr(10), " ").

                find cond-pagto no-lock where
                     cond-pagto.cod-cond-pag = es-acordo-comerc.cod-cond-pag no-error.
                if avail cond-pagto then
                    assign c-pagto = cond-pagto.descricao.
                else
                    assign c-pagto = 'Cond Pagto'.

                find canal-venda no-lock where
                     canal-venda.cod-canal-venda = es-acordo-comerc.cod-canal-venda no-error.
                if avail canal-venda then
                    assign c-canal = canal-venda.descricao.
                else
                    assign c-canal = 'Canal'.

                if es-acordo-pendencia.dt-pendencia = ? then
                    assign d-dt-pend = 12/31/9999.
                else
                    assign d-dt-pend = es-acordo-pendencia.dt-pendencia.
    
                if es-acordo-comerc.dt-ini-period = ? then
                    assign d-dt-per-ini = 12/31/9999.
                else
                    assign d-dt-per-ini = es-acordo-comerc.dt-ini-period.
    
                if es-acordo-comerc.dt-fim-period = ? then
                    assign d-dt-per-fim = 12/31/9999.
                else
                    assign d-dt-per-fim = es-acordo-comerc.dt-fim-period.
    
                if es-acordo-comerc.dt-pagto = ? then
                    assign d-dt-pagto = 12/31/9999.
                else
                    assign d-dt-pagto = es-acordo-comerc.dt-pagto.

                /*
                Numero Acordo - nr-acordo-comerc - ok
                Valor Investimento - vl-invest - ok
                Valor Compra - vl-compra - ok
                Valor Acordo - vl-acordo - ok
                Cliente - cod-emitente - ok
                Representante - cod-repres - ok
                Area - cod-area - ok
                A‡Æo - cod-acao - ok
                Descri‡Æo A‡Æo - objetivo - ok
                Cond. Pagamento - cod-cond-pag - ok
                Canal de Venda - cod-canal-venda - ok
                Periodo Inicial - dt-ini-period - ok
                Periodo Final - dt-fim-period - ok
                Dt Pagamento - dt-pagto - ok
                No Parcelas - num-parc - ok
                */

                assign lcAgreements = lcAgreements +
                                     '~{"numAgree":' + string(es-acordo-comerc.nr-acordo-comerc) + 
                                     ',"docDate":~"' + string(d-dt-pend,"99/99/9999") + '~"' +
                                     ',"perIni":~"' + string(d-dt-per-ini,"99/99/9999") + '~"' +
                                     ',"perFim":~"' + string(d-dt-per-fim,"99/99/9999") + '~"' +                                     
                                     ',"payDate":~"' + string(d-dt-pagto,"99/99/9999") + '~"' +
                                     ',"valueInvest":~"' + trim(string(es-acordo-comerc.vl-invest,">>>,>>>,>>9.99")) + '~"' +
                                     ',"valueBuy":~"' + trim(string(es-acordo-comerc.vl-compra,">>>,>>>,>>9.99")) + '~"' +
                                     ',"valueAgree":~"' + trim(string(es-acordo-comerc.vl-acordo,">>>,>>>,>>9.99")) + '~"' +
                                     ',"user":~"' +  c-repres + '~"' +
                                     ',"customer":~"' + c-emit + '~"' +
                                     ',"area":~"' + c-area + '~"' +
                                     ',"action":~"' + c-acao + '~"' +
                                     ',"objetivo":~"' + c-objetivo + '~"' +
                                     ',"condPagto":~"' + c-pagto + '~"' +
                                     ',"canal":~"' + c-canal + '~"' +
                                     ',"docName":~"' + es-tipo-docto.desc-docto + '~"' +
                                     ',"numParc":~"' + string(es-acordo-comerc.num-parc) + '~"' +
                                     ',"showApproval":~"' + string(es-acordo-pendencia.lg-aprov)  + "~"" +
                                     "~},".
            end.

            if last-of(es-acordo-pendencia.tp-docto) then do:
                if substring(lcAgreements,length(lcAgreements),1) = "," then
                    assign substring(lcAgreements,length(lcAgreements),1) = "".
                                
                assign lcAgreements = lcAgreements + ']~},'.
            end.
        end.

/*         if substring(lcAgreements,length(lcAgreements),1) = "," then    */
/*             assign substring(lcAgreements,length(lcAgreements),1) = "". */
/*                                                                         */
/*         assign lcAgreements = lcAgreements + ']~}'.                     */

    end.

    if substring(lcAgreements,length(lcAgreements),1) = "," then
        assign substring(lcAgreements,length(lcAgreements),1) = "".
    
    assign lcAgreements = lcAgreements + ']~}'.

    output stream s-log to value("\\10.185.32.46\erp\wrk\log_getAgreements.txt").
    
    export stream s-log lcAgreements.

    output stream s-log close.

    run outputHeader.

    lcAgreements = codepage-convert(lcAgreements,"UTF-8").
    {&out-long} lcAgreements.    
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF







