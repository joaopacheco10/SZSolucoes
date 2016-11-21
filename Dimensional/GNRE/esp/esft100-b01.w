&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r11 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESFT100-B01 12.01.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
def new global shared var g-row-es-lote-gnre as rowid no-undo.
def new global shared var gr-nota-fiscal     as rowid no-undo.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def temp-table tt-nota-fiscal no-undo like nota-fiscal
    field lg-selec    as log label 'X':C5
    field vl-bsubs-it as dec label 'Vl.Bs. ICMS ST'
    field vl-icms-st  as dec label 'Vl. ICMS ST'
    field vl-fcp      as dec label 'Vl. FCP'.

def var c-est-ini as char no-undo.
def var c-est-fim as char no-undo.
def var c-ser-ini as char no-undo.
def var c-ser-fim as char no-undo.
def var c-not-ini as char no-undo.
def var c-not-fim as char no-undo.
def var c-dt-ini  as date no-undo.
def var c-dt-fim  as date no-undo.
def var c-uf-ini  as char no-undo.
def var c-uf-fim  as char no-undo.
def var c-nom-ini as char no-undo.
def var c-nom-fim as char no-undo.

def var h-env-lote as handle no-undo.
def var h-con-lote as handle no-undo.
def var h-server   as handle no-undo.

def var h-acomp  as handle no-undo.

def buffer bf-es-lote-gnre for es-lote-gnre.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-nota

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-nota-fiscal

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-nota                                       */
&Scoped-define FIELDS-IN-QUERY-br-nota tt-nota-fiscal.lg-selec tt-nota-fiscal.cod-estabel tt-nota-fiscal.serie tt-nota-fiscal.nr-nota-fis tt-nota-fiscal.nome-ab-cli tt-nota-fiscal.dt-emis-nota tt-nota-fiscal.estado tt-nota-fiscal.vl-tot-nota tt-nota-fiscal.vl-bsubs-it tt-nota-fiscal.vl-icms-st   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-nota tt-nota-fiscal.lg-selec   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-nota tt-nota-fiscal
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-nota tt-nota-fiscal
&Scoped-define SELF-NAME br-nota
&Scoped-define QUERY-STRING-br-nota for each tt-nota-fiscal where no-lock
&Scoped-define OPEN-QUERY-br-nota open query {&SELF-NAME} for each tt-nota-fiscal where no-lock.
&Scoped-define TABLES-IN-QUERY-br-nota tt-nota-fiscal
&Scoped-define FIRST-TABLE-IN-QUERY-br-nota tt-nota-fiscal


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-gera-lote bt-enviar bt-consulta br-nota ~
bt-marca bt-desmarcar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS
><EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnConectar B-table-Win 
FUNCTION fnConectar returns logical
  (p-cod-estabel as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnDesconectar B-table-Win 
FUNCTION fnDesconectar returns character
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-consulta 
     LABEL "Consultar Lote" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bt-desmarcar 
     LABEL "Desmarcar" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bt-enviar 
     LABEL "Enviar Lote" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bt-gera-lote 
     LABEL "Gerar Lote" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bt-marca 
     LABEL "Marcar" 
     SIZE 15 BY 1.13.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-nota FOR 
      tt-nota-fiscal SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-nota
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-nota B-table-Win _FREEFORM
  QUERY br-nota NO-LOCK DISPLAY
      tt-nota-fiscal.lg-selec view-as toggle-box    
tt-nota-fiscal.cod-estabel  format "x(3)":U
tt-nota-fiscal.serie        format "x(5)":U
tt-nota-fiscal.nr-nota-fis  format "x(16)":U             width 12
tt-nota-fiscal.nome-ab-cli  format "x(12)":U
tt-nota-fiscal.dt-emis-nota format "99/99/9999":U
tt-nota-fiscal.estado       format "x(04)":U
tt-nota-fiscal.vl-tot-nota  format ">>,>>>,>>>,>>9.99":U width 11
tt-nota-fiscal.vl-bsubs-it  format ">>,>>>,>>>,>>9.99":U width 12
tt-nota-fiscal.vl-icms-st   format ">>,>>>,>>>,>>9.99":U width 11
enable
tt-nota-fiscal.lg-selec
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 103 BY 16.5
         TITLE "Notas Fiscais" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     bt-gera-lote AT ROW 1.5 COL 2 WIDGET-ID 2
     bt-enviar AT ROW 1.5 COL 19 WIDGET-ID 4
     bt-consulta AT ROW 1.5 COL 36 WIDGET-ID 6
     br-nota AT ROW 3.25 COL 1
     bt-marca AT ROW 20 COL 2 WIDGET-ID 8
     bt-desmarcar AT ROW 20 COL 19 WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 20.5
         WIDTH              = 103.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{include/c-browse.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br-nota bt-consulta F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-nota
/* Query rebuild information for BROWSE br-nota
     _START_FREEFORM
open query {&SELF-NAME} for each tt-nota-fiscal where no-lock.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE br-nota */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-nota
&Scoped-define SELF-NAME br-nota
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-nota B-table-Win
ON MOUSE-SELECT-DBLCLICK OF br-nota IN FRAME F-Main /* Notas Fiscais */
DO:

    if avail tt-nota-fiscal then do:

        find nota-fiscal no-lock where
             nota-fiscal.cod-estabel = tt-nota-fiscal.cod-estabel and
             nota-fiscal.serie       = tt-nota-fiscal.serie       and
             nota-fiscal.nr-nota-fis = tt-nota-fiscal.nr-nota-fis no-error.
        if avail nota-fiscal then do:
            assign gr-nota-fiscal = rowid(nota-fiscal).

            run ftp/ft0904.w.

        end.
    end.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-nota B-table-Win
ON ROW-ENTRY OF br-nota IN FRAME F-Main /* Notas Fiscais */
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-nota B-table-Win
ON ROW-LEAVE OF br-nota IN FRAME F-Main /* Notas Fiscais */
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-nota B-table-Win
ON VALUE-CHANGED OF br-nota IN FRAME F-Main /* Notas Fiscais */
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-consulta B-table-Win
ON CHOOSE OF bt-consulta IN FRAME F-Main /* Consultar Lote */
DO:

    def var l-xml-lote     as longchar no-undo.
    def var l-xml-lote-ret as longchar no-undo.
    def var c-cod-estabel-aux as char     no-undo.

    if not valid-handle(h-con-lote) then
        run adapters/esaxsft002.p persistent set h-con-lote.

    for each es-lote-gnre no-lock where
             es-lote-gnre.uf-gnre      = 1 and
             es-lote-gnre.idi-situacao = 2:

        assign l-xml-lote = ''.

        find first es-nota-fiscal-gnre no-lock where
                   es-nota-fiscal-gnre.cod-lote = es-lote-gnre.cod-lote no-error.
        if avail es-nota-fiscal-gnre then
            assign c-cod-estabel-aux = es-nota-fiscal-gnre.cod-estabel.


        run pi-gera-xml in h-con-lote (input es-lote-gnre.cod-lote,
                                       input c-cod-estabel-aux,
                                       output l-xml-lote).

        if l-xml-lote <> '' then do:

            fnConectar(c-cod-estabel-aux).

            if h-server:connected() then
                run service/consultaLote.p on h-server (input l-xml-lote,
                                                        input c-cod-estabel-aux,
                                                        output l-xml-lote-ret).

            fnDesconectar().

            if l-xml-lote-ret <> '' then
                run pi-le-retorno in h-con-lote (input es-lote-gnre.cod-lote,
                                                 input l-xml-lote-ret).

            find bf-es-lote-gnre no-lock where
                 rowid(bf-es-lote-gnre) = rowid(es-lote-gnre) no-error.
            if avail bf-es-lote-gnre            and
               bf-es-lote-gnre.idi-situacao = 3 then
                run esp/esftapi100.p (rowid(bf-es-lote-gnre)).  
        end.
    end.

    run utp/ut-msgs.p (input 'show',
                       input 15825,
                       input 'Consulta de Lotes conclu¡da!').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desmarcar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desmarcar B-table-Win
ON CHOOSE OF bt-desmarcar IN FRAME F-Main /* Desmarcar */
DO:

    for each tt-nota-fiscal:
        assign tt-nota-fiscal.lg-selec = no.
    end.

    {&open-query-br-nota}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-enviar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-enviar B-table-Win
ON CHOOSE OF bt-enviar IN FRAME F-Main /* Enviar Lote */
DO:

    def var l-xml-lote        as longchar no-undo.
    def var l-xml-lote-ret    as longchar no-undo.
    def var c-cod-estabel-aux as char     no-undo.

    if not valid-handle(h-env-lote) then
        run adapters/esaxsft001.p persistent set h-env-lote.

    for each es-lote-gnre no-lock where
             es-lote-gnre.uf-gnre      = 1 and
             es-lote-gnre.idi-situacao = 1:

        assign l-xml-lote = ''.

        find first es-nota-fiscal-gnre no-lock where
                   es-nota-fiscal-gnre.cod-lote = es-lote-gnre.cod-lote no-error.
        if avail es-nota-fiscal-gnre then
            assign c-cod-estabel-aux = es-nota-fiscal-gnre.cod-estabel.

        run pi-gera-xml in h-env-lote (input es-lote-gnre.cod-lote,
                                       output l-xml-lote).

        if l-xml-lote <> '' then do:

            fnConectar(c-cod-estabel-aux).

            if h-server:connected() then
                run service/enviaLote.p on h-server (input l-xml-lote,
                                                     input c-cod-estabel-aux,
                                                     output l-xml-lote-ret).

            fnDesconectar().

            if l-xml-lote-ret <> '' then
                run pi-le-retorno in h-env-lote (input l-xml-lote-ret,
                                                 input es-lote-gnre.cod-lote).

        end.
    end.

    run utp/ut-msgs.p (input 'show',
                       input 15825,
                       input 'Envio de Lotes conclu¡do!').
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-gera-lote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-gera-lote B-table-Win
ON CHOOSE OF bt-gera-lote IN FRAME F-Main /* Gerar Lote */
DO:
    def var i-seq       as int  no-undo.
    def var c-lote-aux  as char no-undo.
    def var d-dt-pagto  as date no-undo.
    def var d-dt-vencto as date no-undo.

    assign i-seq = 1.

    if avail tt-nota-fiscal then do:

        if not can-find(first tt-nota-fiscal where
                              tt-nota-fiscal.lg-selec = yes) then do:

            run utp/ut-msgs.p (input 'show',
                               input 17006,
                               input 'Nenhuma Nota Fiscal selecionada!').
            return no-apply.

        end.
        else do:

            current-window:sensitive = no.

            run esp/esft100b.w (output d-dt-vencto,
                                output d-dt-pagto).

            current-window:sensitive = yes.

            if d-dt-vencto <> ? and
               d-dt-pagto  <> ? then do:

                do while true:                
    
                    assign c-lote-aux = string(day(today), '99')    +
                                        string(month(today), '99')  +
                                        string(year(today), '9999') +
                                        string(i-seq, '9999').
    
                    if not can-find(es-lote-gnre where
                                    es-lote-gnre.cod-lote = c-lote-aux) then
                        leave.
                    else
                        assign i-seq = i-seq + 1.
    
                end.
    
                create es-lote-gnre.
                assign es-lote-gnre.cod-lote     = c-lote-aux
                       es-lote-gnre.cod-usuario  = c-seg-usuario
                       es-lote-gnre.dt-lote      = today
                       es-lote-gnre.dt-proc      = ''
                       es-lote-gnre.idi-situacao = 1
                       es-lote-gnre.num-recibo   = 0
                       es-lote-gnre.uf-gnre      = 1
                       es-lote-gnre.dt-vencto    = d-dt-vencto
                       es-lote-gnre.dt-pagto     = d-dt-pagto.
    
                for each tt-nota-fiscal where
                         tt-nota-fiscal.lg-selec = yes:

                    if tt-nota-fiscal.vl-icms-st > 0 then do:
                        create es-nota-fiscal-gnre.
                        assign es-nota-fiscal-gnre.cod-estabel = tt-nota-fiscal.cod-estabel
                               es-nota-fiscal-gnre.cod-lote    = es-lote-gnre.cod-lote
                               es-nota-fiscal-gnre.cod-receita = 100099
                               es-nota-fiscal-gnre.nr-nota-fis = tt-nota-fiscal.nr-nota-fis
                               es-nota-fiscal-gnre.serie       = tt-nota-fiscal.serie
                               es-nota-fiscal-gnre.vl-receita  = tt-nota-fiscal.vl-icms-st /*- tt-nota-fiscal.vl-fcp*/ .
                    end.
    
                    /*if tt-nota-fiscal.vl-fcp > 0 then do:
                        create es-nota-fiscal-gnre.
                        assign es-nota-fiscal-gnre.cod-estabel = tt-nota-fiscal.cod-estabel
                               es-nota-fiscal-gnre.cod-lote    = es-lote-gnre.cod-lote
                               es-nota-fiscal-gnre.cod-receita = 100129
                               es-nota-fiscal-gnre.nr-nota-fis = tt-nota-fiscal.nr-nota-fis
                               es-nota-fiscal-gnre.serie       = tt-nota-fiscal.serie
                               es-nota-fiscal-gnre.vl-receita  = tt-nota-fiscal.vl-fcp.
                    end.                */
                end.
    
                run utp/ut-msgs.p (input 'show',
                                   input 15825,
                                   input 'Lote ' + es-lote-gnre.cod-lote + ' gerado com sucesso!').
    
                run pi-carrega-browser.

                assign g-row-es-lote-gnre = rowid(es-lote-gnre).
    
                run esp/esft102.w.
                
            end.
        end.
    end.
    else do:
        run utp/ut-msgs.p (input 'show',
                           input 17006,
                           input 'Nenhuma Nota Fiscal dispon¡vel!':U).

        return no-apply.
    end.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-marca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-marca B-table-Win
ON CHOOSE OF bt-marca IN FRAME F-Main /* Marcar */
DO:

    for each tt-nota-fiscal:
        assign tt-nota-fiscal.lg-selec = yes.
    end.

    {&open-query-br-nota}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

ON 'value-changed':U OF tt-nota-fiscal.lg-selec in browse br-nota
DO:

    assign tt-nota-fiscal.lg-selec = input browse br-nota tt-nota-fiscal.lg-selec.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-browser B-table-Win 
PROCEDURE pi-carrega-browser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    empty temp-table tt-nota-fiscal.

    run utp/ut-acomp.p persistent set h-acomp.
    run pi-inicializar in h-acomp (input 'Carregando...').
    run pi-desabilita-cancela in h-acomp.

    for each nota-fiscal no-lock where
             nota-fiscal.cod-estabel      >= c-est-ini and
             nota-fiscal.cod-estabel      <= c-est-fim and
             nota-fiscal.serie            >= c-ser-ini and
             nota-fiscal.serie            <= c-ser-fim and
             nota-fiscal.nr-nota-fis      >= c-not-ini and
             nota-fiscal.nr-nota-fis      <= c-not-fim and
             nota-fiscal.dt-emis-nota     >= c-dt-ini  and
             nota-fiscal.dt-emis-nota     <= c-dt-fim  and
             nota-fiscal.estado           >= c-uf-ini  and
             nota-fiscal.estado           <= c-uf-fim  and
             nota-fiscal.nome-ab-cli      >= c-nom-ini and
             nota-fiscal.nome-ab-cli      <= c-nom-fim and
             nota-fiscal.dt-cancela        = ? and
             nota-fiscal.idi-sit-nf-eletro = 3:

        if nota-fiscal.estado = 'SP' or
           nota-fiscal.estado = 'RJ' or
           nota-fiscal.estado = 'ES' then
            next.

        if can-find(first es-nota-fiscal where
                          es-nota-fiscal.cod-estabel = nota-fiscal.cod-estabel  and
                          es-nota-fiscal.serie       = nota-fiscal.serie        and
                          es-nota-fiscal.nr-nota-fis = nota-fiscal.nr-nota-fis) then
            next.

        run pi-acompanhar in h-acomp (input 'Nota..: ' + nota-fiscal.nr-nota-fis). 

        create tt-nota-fiscal.
        buffer-copy nota-fiscal to tt-nota-fiscal.

        for each it-nota-fisc no-lock where
                 it-nota-fisc.cod-estabel = nota-fiscal.cod-estabel and
                 it-nota-fisc.serie       = nota-fiscal.serie       and
                 it-nota-fisc.nr-nota-fis = nota-fiscal.nr-nota-fis:

            assign tt-nota-fiscal.vl-bsubs-it = tt-nota-fiscal.vl-bsubs-it + it-nota-fisc.vl-bsubs-it
                   tt-nota-fiscal.vl-icms-st  = tt-nota-fiscal.vl-icms-st + it-nota-fisc.vl-icmsub-it.

        end.

        /*for each ext-it-nota-fisc-fcp no-lock where
                 ext-it-nota-fisc-fcp.cod-estabel = nota-fiscal.cod-estabel and
                 ext-it-nota-fisc-fcp.serie       = nota-fiscal.serie       and
                 ext-it-nota-fisc-fcp.nr-nota-fis = nota-fiscal.nr-nota-fis:

            assign tt-nota-fiscal.vl-fcp = tt-nota-fiscal.vl-fcp + ext-it-nota-fisc-fcp.val-fcp.

        end.*/

        if tt-nota-fiscal.vl-icms-st = 0 then
            delete tt-nota-fiscal.

    end.

    run pi-finalizar in h-acomp.

    {&open-query-br-nota}


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-rec-selec B-table-Win 
PROCEDURE pi-rec-selec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param p-est-ini as char no-undo.
def input param p-est-fim as char no-undo.
def input param p-ser-ini as char no-undo.
def input param p-ser-fim as char no-undo.
def input param p-not-ini as char no-undo.
def input param p-not-fim as char no-undo.
def input param p-dt-ini  as date no-undo.
def input param p-dt-fim  as date no-undo.
def input param p-uf-ini  as char no-undo.
def input param p-uf-fim  as char no-undo.
def input param p-nom-ini as char no-undo.
def input param p-nom-fim as char no-undo.

assign c-est-ini = p-est-ini
       c-est-fim = p-est-fim
       c-ser-ini = p-ser-ini
       c-ser-fim = p-ser-fim
       c-not-ini = p-not-ini
       c-not-fim = p-not-fim
       c-dt-ini  = p-dt-ini 
       c-dt-fim  = p-dt-fim 
       c-uf-ini  = p-uf-ini 
       c-uf-fim  = p-uf-fim
       c-nom-ini = p-nom-ini
       c-nom-fim = p-nom-fim.

run pi-carrega-browser.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-nota-fiscal"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnConectar B-table-Win 
FUNCTION fnConectar returns logical
  (p-cod-estabel as char) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    fnDesconectar().

    find es-param-gnre no-lock where
         es-param-gnre.cod-estabel = p-cod-estabel no-error.
    if avail es-param-gnre then do:   
        create server h-server.
        h-server:connect(es-param-gnre.des-appserver).
    end.

    return h-server:connected().   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnDesconectar B-table-Win 
FUNCTION fnDesconectar returns character
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    if valid-handle(h-server) and
       h-server:connected()   then
        h-server:disconnect().

    return "".   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

