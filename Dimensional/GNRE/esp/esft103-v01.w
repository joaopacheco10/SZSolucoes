&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgesp            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer fornecedor for mgcad.fornecedor.


{include/i-prgvrs.i V99XX999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w

/* global variable definitions */
def new global shared var g-row-conf-uf-gnre-zoom as rowid no-undo.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES es-conf-uf-gnre
&Scoped-define FIRST-EXTERNAL-TABLE es-conf-uf-gnre


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es-conf-uf-gnre.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es-conf-uf-gnre.cod-fornec ~
es-conf-uf-gnre.receita es-conf-uf-gnre.det-receita ~
es-conf-uf-gnre.cont-dest es-conf-uf-gnre.cod-det-receita ~
es-conf-uf-gnre.cont-emit es-conf-uf-gnre.produto es-conf-uf-gnre.dat-pagto ~
es-conf-uf-gnre.cod-produto es-conf-uf-gnre.dat-vencto ~
es-conf-uf-gnre.doc-orig es-conf-uf-gnre.chave-acesso ~
es-conf-uf-gnre.cod-tp-docto es-conf-uf-gnre.parcela ~
es-conf-uf-gnre.convenio es-conf-uf-gnre.tp-valor es-conf-uf-gnre.per-apur ~
es-conf-uf-gnre.per-refer es-conf-uf-gnre.uf-favor 
&Scoped-define ENABLED-TABLES es-conf-uf-gnre
&Scoped-define FIRST-ENABLED-TABLE es-conf-uf-gnre
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold RECT-2 RECT-3 
&Scoped-Define DISPLAYED-FIELDS es-conf-uf-gnre.cod-uf ~
es-conf-uf-gnre.cod-receita es-conf-uf-gnre.cod-fornec ~
es-conf-uf-gnre.receita es-conf-uf-gnre.det-receita ~
es-conf-uf-gnre.cont-dest es-conf-uf-gnre.cod-det-receita ~
es-conf-uf-gnre.cont-emit es-conf-uf-gnre.produto es-conf-uf-gnre.dat-pagto ~
es-conf-uf-gnre.cod-produto es-conf-uf-gnre.dat-vencto ~
es-conf-uf-gnre.doc-orig es-conf-uf-gnre.chave-acesso ~
es-conf-uf-gnre.cod-tp-docto es-conf-uf-gnre.parcela ~
es-conf-uf-gnre.convenio es-conf-uf-gnre.tp-valor es-conf-uf-gnre.per-apur ~
es-conf-uf-gnre.per-refer es-conf-uf-gnre.uf-favor 
&Scoped-define DISPLAYED-TABLES es-conf-uf-gnre
&Scoped-define FIRST-DISPLAYED-TABLE es-conf-uf-gnre
&Scoped-Define DISPLAYED-OBJECTS c-des-fornec c-des-det c-des-prod ~
c-des-docto 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-MODIFY-FIELDS es-conf-uf-gnre.cod-fornec ~
es-conf-uf-gnre.receita es-conf-uf-gnre.det-receita ~
es-conf-uf-gnre.cont-dest es-conf-uf-gnre.cod-det-receita ~
es-conf-uf-gnre.cont-emit es-conf-uf-gnre.produto es-conf-uf-gnre.dat-pagto ~
es-conf-uf-gnre.cod-produto es-conf-uf-gnre.dat-vencto ~
es-conf-uf-gnre.doc-orig es-conf-uf-gnre.chave-acesso ~
es-conf-uf-gnre.cod-tp-docto es-conf-uf-gnre.parcela ~
es-conf-uf-gnre.per-apur es-conf-uf-gnre.per-refer es-conf-uf-gnre.uf-favor 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
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


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE c-des-det AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20.86 BY .88 NO-UNDO.

DEFINE VARIABLE c-des-docto AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25.43 BY .88 NO-UNDO.

DEFINE VARIABLE c-des-fornec AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24.43 BY .88 NO-UNDO.

DEFINE VARIABLE c-des-prod AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 23.14 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 24.86 BY 3.5.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19.14 BY 3.5.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 2.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 11.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     es-conf-uf-gnre.cod-uf AT ROW 1.17 COL 36.86 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     es-conf-uf-gnre.cod-receita AT ROW 2.17 COL 37 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     es-conf-uf-gnre.cod-fornec AT ROW 3.75 COL 26.57 COLON-ALIGNED WIDGET-ID 108
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .88
     c-des-fornec AT ROW 3.75 COL 38.14 COLON-ALIGNED NO-LABEL WIDGET-ID 110
     es-conf-uf-gnre.receita AT ROW 4.96 COL 11 WIDGET-ID 66
          VIEW-AS TOGGLE-BOX
          SIZE 11 BY .83
     es-conf-uf-gnre.det-receita AT ROW 4.96 COL 51.86 WIDGET-ID 52
          VIEW-AS TOGGLE-BOX
          SIZE 15 BY .83
     es-conf-uf-gnre.cont-dest AT ROW 5.96 COL 11 WIDGET-ID 44
          VIEW-AS TOGGLE-BOX
          SIZE 26.57 BY .83
     es-conf-uf-gnre.cod-det-receita AT ROW 5.96 COL 49.86 COLON-ALIGNED WIDGET-ID 4 FORMAT "999999"
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     c-des-det AT ROW 5.96 COL 58 COLON-ALIGNED NO-LABEL WIDGET-ID 102
     es-conf-uf-gnre.cont-emit AT ROW 6.96 COL 11 WIDGET-ID 46
          VIEW-AS TOGGLE-BOX
          SIZE 23.43 BY .83
     es-conf-uf-gnre.produto AT ROW 6.96 COL 51.86 WIDGET-ID 64
          VIEW-AS TOGGLE-BOX
          SIZE 11 BY .83
     es-conf-uf-gnre.dat-pagto AT ROW 7.96 COL 11 WIDGET-ID 48
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY .83
     es-conf-uf-gnre.cod-produto AT ROW 7.96 COL 49.86 COLON-ALIGNED WIDGET-ID 6 FORMAT "9999"
          VIEW-AS FILL-IN 
          SIZE 5.72 BY .88
     c-des-prod AT ROW 7.96 COL 55.72 COLON-ALIGNED NO-LABEL WIDGET-ID 104
     es-conf-uf-gnre.dat-vencto AT ROW 8.96 COL 11 WIDGET-ID 50
          VIEW-AS TOGGLE-BOX
          SIZE 19 BY .83
     es-conf-uf-gnre.doc-orig AT ROW 8.96 COL 51.86 WIDGET-ID 54
          VIEW-AS TOGGLE-BOX
          SIZE 21 BY .83
     es-conf-uf-gnre.chave-acesso AT ROW 9.96 COL 11 WIDGET-ID 56
          VIEW-AS TOGGLE-BOX
          SIZE 23 BY .83
     es-conf-uf-gnre.cod-tp-docto AT ROW 9.96 COL 49.86 COLON-ALIGNED WIDGET-ID 88
          LABEL "Tipo Docto" FORMAT "99"
          VIEW-AS FILL-IN 
          SIZE 3.43 BY .88
     c-des-docto AT ROW 9.96 COL 53.43 COLON-ALIGNED NO-LABEL WIDGET-ID 106
     es-conf-uf-gnre.parcela AT ROW 10.96 COL 11 WIDGET-ID 58
          VIEW-AS TOGGLE-BOX
          SIZE 11.57 BY .83
     es-conf-uf-gnre.convenio AT ROW 11.71 COL 44 NO-LABEL WIDGET-ID 98
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "NÆo Exigido", 1,
"Opcional", 2,
"Exigido", 3
          SIZE 12 BY 3
     es-conf-uf-gnre.tp-valor AT ROW 11.71 COL 63.43 NO-LABEL WIDGET-ID 74
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Valor Principal", 1,
"Valor Total", 2,
"Valor Principal ou Total", 3,
"Nenhum", 4
          SIZE 20.43 BY 3
     es-conf-uf-gnre.per-apur AT ROW 11.96 COL 11 WIDGET-ID 60
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY .83
     es-conf-uf-gnre.per-refer AT ROW 12.96 COL 11 WIDGET-ID 62
          VIEW-AS TOGGLE-BOX
          SIZE 23 BY .83
     es-conf-uf-gnre.uf-favor AT ROW 13.96 COL 11 WIDGET-ID 68
          VIEW-AS TOGGLE-BOX
          SIZE 17 BY .83
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME f-main
     "Convenio" VIEW-AS TEXT
          SIZE 6.72 BY .67 AT ROW 11.13 COL 42.14 WIDGET-ID 96
     "Tipo Valor" VIEW-AS TEXT
          SIZE 7 BY .67 AT ROW 11.13 COL 62 WIDGET-ID 92
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 3.5 COL 1
     RECT-2 AT ROW 11.46 COL 61 WIDGET-ID 84
     RECT-3 AT ROW 11.46 COL 41 WIDGET-ID 94
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgesp.es-conf-uf-gnre
   Allow: Basic,DB-Fields
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 14.25
         WIDTH              = 88.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}
{include/i_dbtype.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN c-des-det IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-des-docto IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-des-fornec IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-des-prod IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX es-conf-uf-gnre.chave-acesso IN FRAME f-main
   3                                                                    */
/* SETTINGS FOR FILL-IN es-conf-uf-gnre.cod-det-receita IN FRAME f-main
   3 EXP-FORMAT                                                         */
/* SETTINGS FOR FILL-IN es-conf-uf-gnre.cod-fornec IN FRAME f-main
   3                                                                    */
/* SETTINGS FOR FILL-IN es-conf-uf-gnre.cod-produto IN FRAME f-main
   3 EXP-FORMAT                                                         */
/* SETTINGS FOR FILL-IN es-conf-uf-gnre.cod-receita IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN es-conf-uf-gnre.cod-tp-docto IN FRAME f-main
   3 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN es-conf-uf-gnre.cod-uf IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX es-conf-uf-gnre.cont-dest IN FRAME f-main
   3                                                                    */
/* SETTINGS FOR TOGGLE-BOX es-conf-uf-gnre.cont-emit IN FRAME f-main
   3                                                                    */
/* SETTINGS FOR TOGGLE-BOX es-conf-uf-gnre.dat-pagto IN FRAME f-main
   3                                                                    */
/* SETTINGS FOR TOGGLE-BOX es-conf-uf-gnre.dat-vencto IN FRAME f-main
   3                                                                    */
/* SETTINGS FOR TOGGLE-BOX es-conf-uf-gnre.det-receita IN FRAME f-main
   3                                                                    */
/* SETTINGS FOR TOGGLE-BOX es-conf-uf-gnre.doc-orig IN FRAME f-main
   3                                                                    */
/* SETTINGS FOR TOGGLE-BOX es-conf-uf-gnre.parcela IN FRAME f-main
   3                                                                    */
/* SETTINGS FOR TOGGLE-BOX es-conf-uf-gnre.per-apur IN FRAME f-main
   3                                                                    */
/* SETTINGS FOR TOGGLE-BOX es-conf-uf-gnre.per-refer IN FRAME f-main
   3                                                                    */
/* SETTINGS FOR TOGGLE-BOX es-conf-uf-gnre.produto IN FRAME f-main
   3                                                                    */
/* SETTINGS FOR TOGGLE-BOX es-conf-uf-gnre.receita IN FRAME f-main
   3                                                                    */
/* SETTINGS FOR TOGGLE-BOX es-conf-uf-gnre.uf-favor IN FRAME f-main
   3                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME es-conf-uf-gnre.cod-det-receita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conf-uf-gnre.cod-det-receita V-table-Win
ON F5 OF es-conf-uf-gnre.cod-det-receita IN FRAME f-main /* Det Receita */
DO:

    {include~/zoomvar.i &prog-zoom=esp/esft103-z02.w
                        &campo=es-conf-uf-gnre.cod-det-receita
                        &campozoom=cod-det-receita
                        &campo2=c-des-det
                        &campozoom2=des-det-receita}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conf-uf-gnre.cod-det-receita V-table-Win
ON LEAVE OF es-conf-uf-gnre.cod-det-receita IN FRAME f-main /* Det Receita */
DO:

    {include/leave.i &tabela=es-det-rec-gnre
                      &atributo-ref=des-det-receita
                      &variavel-ref=c-des-det
                      &where="es-det-rec-gnre.cod-uf = es-conf-uf-gnre.cod-uf and
                              es-det-rec-gnre.cod-receita = es-conf-uf-gnre.cod-receita and
                              es-det-rec-gnre.cod-det-receita = input frame f-main es-conf-uf-gnre.cod-det-receita"}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conf-uf-gnre.cod-det-receita V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-conf-uf-gnre.cod-det-receita IN FRAME f-main /* Det Receita */
DO:

    apply 'F5' to self.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-conf-uf-gnre.cod-fornec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conf-uf-gnre.cod-fornec V-table-Win
ON F5 OF es-conf-uf-gnre.cod-fornec IN FRAME f-main /* Fornecedor */
DO:

    {include~/zoomvar.i &prog-zoom=adzoom/z01ad098.w
                        &campo=es-conf-uf-gnre.cod-fornec
                        &campozoom=cod-emitente
                        &campo2=c-des-fornec
                        &campozoom2=nome-abrev}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conf-uf-gnre.cod-fornec V-table-Win
ON LEAVE OF es-conf-uf-gnre.cod-fornec IN FRAME f-main /* Fornecedor */
DO:
  
    {include/leave.i &tabela=emitente
                     &atributo-ref=nome-abrev
                     &variavel-ref=c-des-fornec
                     &where="emitente.cod-emitente = input frame f-main es-conf-uf-gnre.cod-fornec"}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conf-uf-gnre.cod-fornec V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-conf-uf-gnre.cod-fornec IN FRAME f-main /* Fornecedor */
DO:

    apply 'f5' to self.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-conf-uf-gnre.cod-produto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conf-uf-gnre.cod-produto V-table-Win
ON F5 OF es-conf-uf-gnre.cod-produto IN FRAME f-main /* Produto */
DO:

    {include~/zoomvar.i &prog-zoom=esp/esft103-z04.w
                        &campo=es-conf-uf-gnre.cod-produto
                        &campozoom=cod-produto
                        &campo2=c-des-prod
                        &campozoom2=des-produto}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conf-uf-gnre.cod-produto V-table-Win
ON LEAVE OF es-conf-uf-gnre.cod-produto IN FRAME f-main /* Produto */
DO:

    {include/leave.i &tabela=es-prod-gnre
                      &atributo-ref=des-produto
                      &variavel-ref=c-des-prod
                      &where="es-prod-gnre.cod-uf      = es-conf-uf-gnre.cod-uf and
                              es-prod-gnre.cod-receita = es-conf-uf-gnre.cod-receita and
                              es-prod-gnre.cod-produto = input frame f-main es-conf-uf-gnre.cod-produto"}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conf-uf-gnre.cod-produto V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-conf-uf-gnre.cod-produto IN FRAME f-main /* Produto */
DO:

    apply 'f5' to self.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-conf-uf-gnre.cod-tp-docto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conf-uf-gnre.cod-tp-docto V-table-Win
ON F5 OF es-conf-uf-gnre.cod-tp-docto IN FRAME f-main /* Tipo Docto */
DO:

    {include~/zoomvar.i &prog-zoom=esp/esft103-z03.w
                        &campo=es-conf-uf-gnre.cod-tp-docto
                        &campozoom=cod-tp-docto
                        &campo2=c-des-docto
                        &campozoom2=des-tp-docto}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conf-uf-gnre.cod-tp-docto V-table-Win
ON LEAVE OF es-conf-uf-gnre.cod-tp-docto IN FRAME f-main /* Tipo Docto */
DO:

    {include/leave.i &tabela=es-tp-docto-gnre
                      &atributo-ref=des-tp-docto
                      &variavel-ref=c-des-docto
                      &where="es-tp-docto-gnre.cod-uf       = es-conf-uf-gnre.cod-uf and
                              es-tp-docto-gnre.cod-receita  = es-conf-uf-gnre.cod-receita and
                              es-tp-docto-gnre.cod-tp-docto = input frame f-main es-conf-uf-gnre.cod-tp-docto"}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conf-uf-gnre.cod-tp-docto V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-conf-uf-gnre.cod-tp-docto IN FRAME f-main /* Tipo Docto */
DO:

    apply 'f5' to self.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-conf-uf-gnre.det-receita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conf-uf-gnre.det-receita V-table-Win
ON VALUE-CHANGED OF es-conf-uf-gnre.det-receita IN FRAME f-main /* Det. Receita */
DO:

    assign es-conf-uf-gnre.cod-det-receita:sensitive in frame {&frame-name} = input frame {&frame-name} es-conf-uf-gnre.det-receita.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-conf-uf-gnre.doc-orig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conf-uf-gnre.doc-orig V-table-Win
ON VALUE-CHANGED OF es-conf-uf-gnre.doc-orig IN FRAME f-main /* Documento Origem */
DO:

    assign es-conf-uf-gnre.cod-tp-docto:sensitive in frame {&frame-name}    = input frame {&frame-name} es-conf-uf-gnre.doc-orig.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-conf-uf-gnre.produto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conf-uf-gnre.produto V-table-Win
ON VALUE-CHANGED OF es-conf-uf-gnre.produto IN FRAME f-main /* Produto */
DO:

    assign es-conf-uf-gnre.cod-produto:sensitive in frame {&frame-name}     = input frame {&frame-name} es-conf-uf-gnre.produto.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

es-conf-uf-gnre.cod-det-receita:load-mouse-pointer("image~\lupa.cur":U) in frame {&frame-name}.
es-conf-uf-gnre.cod-produto:load-mouse-pointer("image~\lupa.cur":U) in frame {&frame-name}.
es-conf-uf-gnre.cod-tp-docto:load-mouse-pointer("image~\lupa.cur":U) in frame {&frame-name}.
es-conf-uf-gnre.cod-fornec:load-mouse-pointer("image~\lupa.cur":U) in frame {&frame-name}.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "es-conf-uf-gnre"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es-conf-uf-gnre"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME f-main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    {include/i-valid.i}
    
    /*:T Ponha na pi-validate todas as valida‡äes */
    /*:T NÆo gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */


    if es-conf-uf-gnre.produto then do:
        if not can-find(es-prod-gnre where
                        es-prod-gnre.cod-uf      = es-conf-uf-gnre.cod-uf       and
                        es-prod-gnre.cod-receita = es-conf-uf-gnre.cod-receita  and
                        es-prod-gnre.cod-produto = input frame {&frame-name} es-conf-uf-gnre.cod-produto) then do:
    
            run utp/ut-msgs.p (input "show":U, 
                               input 17006, 
                               input "Produto Inv lido!").
            apply 'entry' to es-conf-uf-gnre.cod-produto in frame {&frame-name}.
            return 'ADM-ERROR':U.
    
        end.
    end.

    if es-conf-uf-gnre.det-receita then do:
        if not can-find(es-det-rec-gnre where
                        es-det-rec-gnre.cod-uf          = es-conf-uf-gnre.cod-uf          and
                        es-det-rec-gnre.cod-receita     = es-conf-uf-gnre.cod-receita     and
                        es-det-rec-gnre.cod-det-receita = input frame {&frame-name} es-conf-uf-gnre.cod-det-receita) then do:
    
            run utp/ut-msgs.p (input "show":U, 
                               input 17006, 
                               input "Detalhe Receita Inv lido!").
            apply 'entry' to es-conf-uf-gnre.cod-det-receita in frame {&frame-name}.
            return 'ADM-ERROR':U.
    
        end.
    end.

    if es-conf-uf-gnre.doc-orig then do:
        if not can-find(es-tp-docto-gnre where
                        es-tp-docto-gnre.cod-uf       = es-conf-uf-gnre.cod-uf       and
                        es-tp-docto-gnre.cod-receita  = es-conf-uf-gnre.cod-receita  and
                        es-tp-docto-gnre.cod-tp-docto = input frame {&frame-name} es-conf-uf-gnre.cod-tp-docto) then do:
    
            run utp/ut-msgs.p (input "show":U, 
                               input 17006, 
                               input "Tipo de Documento Inv lido!").
            apply 'entry' to es-conf-uf-gnre.cod-tp-docto in frame {&frame-name}.
            return 'ADM-ERROR':U.
    
        end.
    end.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    disable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    run dispatch in this-procedure ( input 'display-fields':U ) .

    if avail es-conf-uf-gnre then do:

        assign g-row-conf-uf-gnre-zoom = rowid(es-conf-uf-gnre).

        find es-prod-gnre no-lock where
             es-prod-gnre.cod-uf      = es-conf-uf-gnre.cod-uf      and
             es-prod-gnre.cod-receita = es-conf-uf-gnre.cod-receita and
             es-prod-gnre.cod-produto = es-conf-uf-gnre.cod-produto no-error.
        if avail es-prod-gnre then
            assign c-des-prod:screen-value in frame {&frame-name} = es-prod-gnre.des-produto.
        else
            assign c-des-prod:screen-value in frame {&frame-name} = ''.

        find es-tp-docto-gnre no-lock where
             es-tp-docto-gnre.cod-uf       = es-conf-uf-gnre.cod-uf       and
             es-tp-docto-gnre.cod-receita  = es-conf-uf-gnre.cod-receita  and
             es-tp-docto-gnre.cod-tp-docto = es-conf-uf-gnre.cod-tp-docto no-error.
        if avail es-tp-docto-gnre then
            assign c-des-docto:screen-value in frame {&frame-name} = es-tp-docto-gnre.des-tp-docto.
        else
            assign c-des-docto:screen-value in frame {&frame-name} = ''.

        find es-det-rec-gnre no-lock where
             es-det-rec-gnre.cod-uf          = es-conf-uf-gnre.cod-uf          and
             es-det-rec-gnre.cod-receita     = es-conf-uf-gnre.cod-receita     and
             es-det-rec-gnre.cod-det-receita = es-conf-uf-gnre.cod-det-receita no-error.
        if avail es-det-rec-gnre then
            assign c-des-det:screen-value in frame {&frame-name} = es-det-rec-gnre.des-det-receita.
        else
            assign c-des-det:screen-value in frame {&frame-name} = ''.

        find emitente no-lock where
             emitente.cod-emitente = es-conf-uf-gnre.cod-fornec no-error.
        if avail emitente then
            assign c-des-fornec:screen-value in frame {&frame-name} = emitente.nome-abrev.
        else
            assign c-des-fornec:screen-value in frame {&frame-name} = ''.

    end.
    else
        assign g-row-conf-uf-gnre-zoom = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    if adm-new-record = yes then
        enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    
    assign es-conf-uf-gnre.cod-det-receita:sensitive in frame {&frame-name} = input frame {&frame-name} es-conf-uf-gnre.det-receita
           es-conf-uf-gnre.cod-produto:sensitive in frame {&frame-name}     = input frame {&frame-name} es-conf-uf-gnre.produto
           es-conf-uf-gnre.cod-tp-docto:sensitive in frame {&frame-name}    = input frame {&frame-name} es-conf-uf-gnre.doc-orig.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-parent V-table-Win 
PROCEDURE pi-atualiza-parent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define input parameter v-row-parent-externo as rowid no-undo.
    
    assign v-row-parent = v-row-parent-externo.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-get-rowid V-table-Win 
PROCEDURE pi-get-rowid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def output param r-row-conf as rowid no-undo.


if avail es-conf-uf-gnre then
    assign r-row-conf = rowid(es-conf-uf-gnre).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*:T------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: NÆo fazer assign aqui. Nesta procedure
  devem ser colocadas apenas valida‡äes, pois neste ponto do programa o registro 
  ainda nÆo foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /*:T Valida‡Æo de dicion rio */
    
/*:T    Segue um exemplo de valida‡Æo de programa */
/*       find tabela where tabela.campo1 = c-variavel and               */
/*                         tabela.campo2 > i-variavel no-lock no-error. */
      
      /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
/*       {include/i-vldprg.i}                                             */
/*       run utp/ut-msgs.p (input "show":U, input 7, input return-value). */
/*       return 'ADM-ERROR':U.                                            */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "es-conf-uf-gnre"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
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
      {src/adm/template/vstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

