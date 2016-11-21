&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 
&global-define program CSCD0004
&global-define version 1.00.00.001

{include/i-prgvrs.i {&program} {&version}}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/*:T Preprocessadores do Template de Relat¢rio                            */
/*:T Obs: Retirar o valor do preprocessador para as p†ginas que n∆o existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGIMP f-pg-imp

&GLOBAL-DEFINE RTF   no
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG
    field c-arq-item-cli   as char
    field c-arq-subst-tribut as char.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.
def var c-lin-prog       as char.
def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-rtf              as char    no-undo.
def var c-modelo-default   as char    no-undo.

/*15/02/2005 - tech1007 - Variavel definida para tratar se o programa est† rodando no WebEnabler*/
DEFINE SHARED VARIABLE hWenController AS HANDLE NO-UNDO.

def temp-table tt-item-cli no-undo
    field nome-abrev        as char
    field it-codigo         as char
    field cod-emit          as int
    field perc-subst-trib   as dec
    index id 
          nome-abrev
          it-codigo.

def temp-table tt-subst-tribut no-undo
    field it-codigo         as char
    field cod-estado-orig   as char
    field estado            as char
    field perc-sub-tri      as dec
    field perc-red-sub      as dec
    field val-icms-est-sub  as dec
    field val-icms-reduc    as dec
    field val-fcp           as dec
    field des-msg-fcp       as char
    field des-imp           as char
    index id
          it-codigo
          cod-estado-orig
          estado.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-imp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-9 rs-destino bt-arquivo ~
bt-config-impr c-arquivo rs-execucao 
&Scoped-Define DISPLAYED-OBJECTS rs-destino c-arquivo rs-execucao 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-arquivo 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-config-impr 
     IMAGE-UP FILE "image\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.57 BY .63 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execuá∆o" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Terminal", 3
     SIZE 11.72 BY 1.08 NO-UNDO.

DEFINE VARIABLE rs-execucao AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line", 1
     SIZE 27.72 BY .92 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 2.79.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE BUTTON bt-modelo-rtf-2 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-modelo-rtf-3 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arq-item-cli AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 67 BY 1 NO-UNDO.

DEFINE VARIABLE c-arq-subst-tribut AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 67 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 2.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 2.

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1.

DEFINE IMAGE im-pg-imp
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-sel
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 79 BY 1.42
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 0    
     SIZE 78.72 BY .13
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 11.38
     FGCOLOR 0 .

DEFINE RECTANGLE rt-folder-left
     EDGE-PIXELS 0    
     SIZE .43 BY 11.21
     BGCOLOR 15 .

DEFINE RECTANGLE rt-folder-right
     EDGE-PIXELS 0    
     SIZE .43 BY 11.17
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder-top
     EDGE-PIXELS 0    
     SIZE 78.72 BY .13
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     RECT-1 AT ROW 14.29 COL 2
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder AT ROW 2.5 COL 2
     im-pg-imp AT ROW 1.5 COL 17.86
     im-pg-sel AT ROW 1.5 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar WIDGET-ID 100.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 1.63 COL 3.29 HELP
          "Destino de Impress∆o do Relat¢rio" NO-LABEL
     bt-arquivo AT ROW 2.71 COL 43.29 HELP
          "Escolha do nome do arquivo"
     bt-config-impr AT ROW 2.71 COL 43.29 HELP
          "Configuraá∆o da impressora"
     c-arquivo AT ROW 2.75 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 5.13 COL 2.86 HELP
          "Modo de Execuá∆o" NO-LABEL
     text-destino AT ROW 1.04 COL 3.86 NO-LABEL
     text-modo AT ROW 4.38 COL 1.14 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.33 COL 2.14
     RECT-9 AT ROW 4.58 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10.5 WIDGET-ID 100.

DEFINE FRAME f-pg-sel
     c-arq-item-cli AT ROW 1.96 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     bt-modelo-rtf-2 AT ROW 1.96 COL 71 HELP
          "Escolha do nome do arquivo" WIDGET-ID 12
     c-arq-subst-tribut AT ROW 4.58 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     bt-modelo-rtf-3 AT ROW 4.58 COL 71 HELP
          "Escolha do nome do arquivo" WIDGET-ID 14
     " Arquivo Atualizaá∆o Subst Tribut Item/UF" VIEW-AS TEXT
          SIZE 29.14 BY .54 AT ROW 3.67 COL 3.86 WIDGET-ID 20
     " Arquivo Atualizaá∆o Item Cliente" VIEW-AS TEXT
          SIZE 23 BY .54 AT ROW 1.04 COL 3.86 WIDGET-ID 8
     RECT-10 AT ROW 1.33 COL 2 WIDGET-ID 6
     RECT-11 AT ROW 3.96 COL 2 WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.86 BY 9.63
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-relat
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-relat ASSIGN
         HIDDEN             = YES
         TITLE              = "<Title>"
         HEIGHT             = 15
         WIDTH              = 81.14
         MAX-HEIGHT         = 22.33
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22.33
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-relat 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-relat.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-relat
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-pg-imp
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Execuá∆o".

/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FRAME f-relat
                                                                        */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-left IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-right IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-top IN FRAME f-relat
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-relat)
THEN w-relat:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-imp
/* Query rebuild information for FRAME f-pg-imp
     _Query            is NOT OPENED
*/  /* FRAME f-pg-imp */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-sel
/* Query rebuild information for FRAME f-pg-sel
     _Query            is NOT OPENED
*/  /* FRAME f-pg-sel */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON END-ERROR OF w-relat /* <Title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* <Title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-relat
ON CHOOSE OF bt-ajuda IN FRAME f-relat /* Ajuda */
DO:
   {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo w-relat
ON CHOOSE OF bt-arquivo IN FRAME f-pg-imp
DO:
    {include/i-rparq.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-relat
ON CHOOSE OF bt-cancelar IN FRAME f-relat /* Fechar */
DO:
   apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-config-impr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-config-impr w-relat
ON CHOOSE OF bt-config-impr IN FRAME f-pg-imp
DO:
   {include/i-rpimp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar w-relat
ON CHOOSE OF bt-executar IN FRAME f-relat /* Executar */
DO:
   do  on error undo, return no-apply:
       run pi-executar.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME bt-modelo-rtf-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modelo-rtf-2 w-relat
ON CHOOSE OF bt-modelo-rtf-2 IN FRAME f-pg-sel
DO:
    def var l-ok  as logical   no-undo.
    def var c-arq as character no-undo.

    SYSTEM-DIALOG GET-FILE c-arq
        FILTERS    "Planilhas Excel (*.csv)"   "*.csv"
        MUST-EXIST
        USE-FILENAME
        UPDATE l-ok.
      
    IF l-ok THEN
        display c-arq @ c-arq-item-cli with frame f-pg-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-modelo-rtf-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modelo-rtf-3 w-relat
ON CHOOSE OF bt-modelo-rtf-3 IN FRAME f-pg-sel
DO:
    def var l-ok  as logical   no-undo.
    def var c-arq as character no-undo.

    SYSTEM-DIALOG GET-FILE c-arq
        FILTERS    "Planilhas Excel (*.csv)"   "*.csv"
        MUST-EXIST
        USE-FILENAME
        UPDATE l-ok.
      
    IF l-ok THEN
        display c-arq @ c-arq-subst-tribut with frame f-pg-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME im-pg-imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-imp w-relat
ON MOUSE-SELECT-CLICK OF im-pg-imp IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-sel w-relat
ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME rs-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino w-relat
ON VALUE-CHANGED OF rs-destino IN FRAME f-pg-imp
DO:
/*Alterado 15/02/2005 - tech1007 - Evento alterado para correto funcionamento dos novos widgets
  utilizados para a funcionalidade de RTF*/
do  with frame f-pg-imp:
    case self:screen-value:
        when "1" then do:
            assign c-arquivo:sensitive    = no
                   bt-arquivo:visible     = no
                   bt-config-impr:visible = YES
                   /*Alterado 17/02/2005 - tech1007 - Realizado teste de preprocessador para
                     verificar se o RTF est† ativo*/
                   &IF "{&RTF}":U = "YES":U &THEN
                   l-habilitaRtf:sensitive  = NO
                   l-habilitaRtf:SCREEN-VALUE IN FRAME f-pg-imp = "No"
                   l-habilitaRtf = NO
                   &endif
                   /*Fim alteracao 17/02/2005*/
                   .
        end.
        when "2" then do:
            assign c-arquivo:sensitive     = yes
                   bt-arquivo:visible      = yes
                   bt-config-impr:visible  = NO
                   /*Alterado 17/02/2005 - tech1007 - Realizado teste de preprocessador para
                     verificar se o RTF est† ativo*/
                   &IF "{&RTF}":U = "YES":U &THEN
                   l-habilitaRtf:sensitive  = YES
                   &endif
                   /*Fim alteracao 17/02/2005*/
                   .
        end.
        when "3" then do:
            assign c-arquivo:sensitive     = no
                   bt-arquivo:visible      = no
                   bt-config-impr:visible  = no
                   /*Alterado 17/02/2005 - tech1007 - Realizado teste de preprocessador para
                     verificar se o RTF est† ativo*/
                   &IF "{&RTF}":U = "YES":U &THEN
                   l-habilitaRtf:sensitive  = YES
                   &endif
                   /*Fim alteracao 17/02/2005*/
                   .
            /*Alterado 15/02/2005 - tech1007 - Teste para funcionar corretamente no WebEnabler*/
            &IF "{&RTF}":U = "YES":U &THEN
            IF VALID-HANDLE(hWenController) THEN DO:
                ASSIGN l-habilitaRtf:sensitive  = NO
                       l-habilitaRtf:SCREEN-VALUE IN FRAME f-pg-imp = "No"
                       l-habilitaRtf = NO.
            END.
            &endif
            /*Fim alteracao 15/02/2005*/
        end.
    end case.
end.
&IF "{&RTF}":U = "YES":U &THEN
RUN pi-habilitaRtf.
&endif
/*Fim alteracao 15/02/2005*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-execucao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-execucao w-relat
ON VALUE-CHANGED OF rs-execucao IN FRAME f-pg-imp
DO:
   {include/i-rprse.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "{&program}" "{&version}"}

/*:T inicializaá‰es do template de relat¢rio */
{include/i-rpini.i}

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

{include/i-rplbl.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN enable_UI.
    
    {include/i-rpmbl.i}
  
    IF  NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-relat  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-relat  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-relat  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-relat)
  THEN DELETE WIDGET w-relat.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-relat  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE im-pg-imp im-pg-sel bt-executar bt-cancelar bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-7 RECT-9 rs-destino bt-arquivo bt-config-impr c-arquivo 
         rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY c-arq-item-cli c-arq-subst-tribut 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE RECT-10 RECT-11 c-arq-item-cli bt-modelo-rtf-2 c-arq-subst-tribut 
         bt-modelo-rtf-3 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  VIEW w-relat.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-relat 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar w-relat 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var r-tt-digita as rowid no-undo.

do on error undo, return error on stop  undo, return error:
    {include/i-rpexa.i}

    if input frame f-pg-sel c-arq-item-cli = "" and input frame f-pg-sel c-arq-subst-tribut = "" then do:
        run utp/ut-msgs.p (input "show",
                           input 17006,
                           input "Arquivo Inv†lido!" + "~~" + "ê preciso informar pelo menos 1 arquivo para importaá∆o.").

        apply "entry" to c-arq-item-cli in frame f-pg-sel.
        return error.
    end.

    if input frame f-pg-sel c-arq-item-cli <> "" and search(input frame f-pg-sel c-arq-item-cli) = ? then do:
        run utp/ut-msgs.p (input "show",
                           input 17006,
                           input "Arquivo Inv†lido!" + "~~" + "O arquivo informado para a atualizaá∆o dos itens do cliente n∆o foi encontrado.").

        apply "entry" to c-arq-item-cli in frame f-pg-sel.
        return error.
    end.

    if input frame f-pg-sel c-arq-subst-tribut <> "" and search(input frame f-pg-sel c-arq-subst-tribut) = ? then do:
        run utp/ut-msgs.p (input "show",
                           input 17006,
                           input "Arquivo Inv†lido!" + "~~" + "O arquivo informado para a Atualizaá∆o Subst Tribut Item/UF n∆o foi encontrado.").

        apply "entry" to c-arq-subst-tribut in frame f-pg-sel.
        return error.
    end.

    /*14/02/2005 - tech1007 - Alterada condicao para n∆o considerar mai o RTF como destino*/
    if input frame f-pg-imp rs-destino = 2 and
       input frame f-pg-imp rs-execucao = 1 then do:
        run utp/ut-vlarq.p (input input frame f-pg-imp c-arquivo).
        
        if return-value = "NOK":U then do:
            run utp/ut-msgs.p (input "show":U, input 73, input "").
            
            apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
            apply "ENTRY":U to c-arquivo in frame f-pg-imp.
            return error.
        end.
    end.

    /*14/02/2005 - tech1007 - Teste efetuado para nao permitir modelo em branco*/
    &IF "{&RTF}":U = "YES":U &THEN
    IF ( INPUT FRAME f-pg-imp c-modelo-rtf = "" AND
         INPUT FRAME f-pg-imp l-habilitaRtf = "Yes" ) OR
       ( SEARCH(INPUT FRAME f-pg-imp c-modelo-rtf) = ? AND
         input frame f-pg-imp rs-execucao = 1 AND
         INPUT FRAME f-pg-imp l-habilitaRtf = "Yes" )
         THEN DO:
        run utp/ut-msgs.p (input "show":U, input 73, input "").        
        apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
        /*30/12/2004 - tech1007 - Evento removido pois causa problemas no WebEnabler*/
        /*apply "CHOOSE":U to bt-modelo-rtf in frame f-pg-imp.*/
        return error.
    END.
    &endif
    /*Fim teste Modelo*/
    
    /*:T Coloque aqui as validaá‰es da p†gina de Digitaá∆o, lembrando que elas devem
       apresentar uma mensagem de erro cadastrada, posicionar nesta p†gina e colocar
       o focus no campo com problemas */
    /*browse br-digita:SET-REPOSITIONED-ROW (browse br-digita:DOWN, "ALWAYS":U).*/
    /*
    for each tt-digita no-lock:
        assign r-tt-digita = rowid(tt-digita).
        
        /*:T Validaá∆o de duplicidade de registro na temp-table tt-digita */
        find first b-tt-digita where b-tt-digita.ordem = tt-digita.ordem and 
                                     rowid(b-tt-digita) <> rowid(tt-digita) no-lock no-error.
        if avail b-tt-digita then do:
            apply "MOUSE-SELECT-CLICK":U to im-pg-dig in frame f-relat.
            reposition br-digita to rowid rowid(b-tt-digita).
            
            run utp/ut-msgs.p (input "show":U, input 108, input "").
            apply "ENTRY":U to tt-digita.ordem in browse br-digita.
            
            return error.
        end.
        
        /*:T As demais validaá‰es devem ser feitas aqui */
        if tt-digita.ordem <= 0 then do:
            assign browse br-digita:CURRENT-COLUMN = tt-digita.ordem:HANDLE in browse br-digita.
            
            apply "MOUSE-SELECT-CLICK":U to im-pg-dig in frame f-relat.
            reposition br-digita to rowid r-tt-digita.
            
            run utp/ut-msgs.p (input "show":U, input 99999, input "").
            apply "ENTRY":U to tt-digita.ordem in browse br-digita.
            
            return error.
        end.
        
    end.
    */
    
    
    /*:T Coloque aqui as validaá‰es das outras p†ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p†gina com 
       problemas e colocar o focus no campo com problemas */
    
    
    
    /*:T Aqui s∆o gravados os campos da temp-table que ser† passada como parÉmetro
       para o programa RP.P */
    
    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame f-pg-imp rs-destino
           tt-param.data-exec       = today
           tt-param.hora-exec       = time
           tt-param.c-arq-item-cli     = input frame f-pg-sel c-arq-item-cli
           tt-param.c-arq-subst-tribut = input frame f-pg-sel c-arq-subst-tribut

           /*tt-param.classifica      = input frame f-pg-cla rs-classif
           tt-param.desc-classifica = entry((tt-param.classifica - 1) * 2 + 1, 
                                            rs-classif:radio-buttons in frame f-pg-cla)*/
           &IF "{&RTF}":U = "YES":U &THEN
           tt-param.modelo-rtf      = INPUT FRAME f-pg-imp c-modelo-rtf
           /*Alterado 14/02/2005 - tech1007 - Armazena a informaá∆o se o RTF est† habilitado ou n∆o*/
           tt-param.l-habilitaRtf     = INPUT FRAME f-pg-imp l-habilitaRtf
           /*Fim alteracao 14/02/2005*/ 
           &endif
           .
    
    /*Alterado 14/02/2005 - tech1007 - Alterado o teste para verificar se a opá∆o de RTF est† selecionada*/
    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    /*Fim alteracao 14/02/2005*/

    /*:T Coloque aqui a/l¢gica de gravaá∆o dos demais campos que devem ser passados
       como parÉmetros para o programa RP.P, atravÇs da temp-table tt-param */
    
    
    
    /*:T Executar do programa RP.P que ir† criar o relat¢rio */
    /*{include/i-rpexb.i}*/
    
    SESSION:SET-WAIT-STATE("general":U).
    
    /*{include/i-rprun.i xxp/xx9999rp.p}*/

    run pi-importar.

    /*{include/i-rpexc.i}*/
    
    SESSION:SET-WAIT-STATE("":U).
    
    run utp/ut-msgs.p (input "show",
                       input 19085,
                       input "Atualizaá∆o Finalizada!" + "~~" + "").

    /*{include/i-rptrm.i}*/
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-importar w-relat 
PROCEDURE pi-importar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    def var h-acomp as handle no-undo.

    assign input frame f-pg-sel c-arq-item-cli
           input frame f-pg-sel c-arq-subst-tribut.

    run utp/ut-acomp.p persistent set h-acomp.

    run pi-inicializar in h-acomp ("Atualizaá∆o de Cadastros").
    run pi-acompanhar  in h-acomp ("Importando Planilhas...").

    run pi-importar-item-cli.
    run pi-importar-subst-tribut.

    run pi-acompanhar in h-acomp ("Atualizando. Aguarde...").

    if temp-table tt-item-cli:has-records then do:
        for each tt-item-cli:

            do transaction on error undo, leave on stop  undo, leave on quit  undo, leave:

                find first item-cli exclusive-lock
                    where item-cli.nome-abrev = tt-item-cli.nome-abrev
                      and item-cli.it-codigo  = tt-item-cli.it-codigo no-error.
                if not avail item-cli then do:

                    for first item fields(un) no-lock
                        where item.it-codigo = tt-item-cli.it-codigo:

                        create item-cli.
                        assign item-cli.nome-abrev   = upper(tt-item-cli.nome-abrev)
                               item-cli.it-codigo    = tt-item-cli.it-codigo
                               item-cli.unid-med-cli = upper(item.un)
                               item-cli.cod-emitente = tt-item-cli.cod-emit
                               item-cli.dec-1        = tt-item-cli.perc-subst-trib.
                    end.
                end.
                else
                    assign item-cli.dec-1 = tt-item-cli.perc-subst-trib.
            end.
        end.
    end.

    if temp-table tt-subst-tribut:has-records then do:
        for each tt-subst-tribut:

            do transaction on error undo, leave on stop  undo, leave on quit  undo, leave:
                find first item-uf exclusive-lock
                    where item-uf.it-codigo       = tt-subst-tribut.it-codigo
                      and item-uf.cod-estado-orig = tt-subst-tribut.cod-estado-orig
                      and item-uf.estado          = tt-subst-tribut.estado no-error.
                if avail item-uf then do:
                    assign item-uf.per-sub-tri      = tt-subst-tribut.perc-sub-tri     
                           item-uf.perc-red-sub     = tt-subst-tribut.perc-red-sub    
                           item-uf.dec-1            = tt-subst-tribut.val-icms-est-sub.
                           
                    if tt-subst-tribut.val-fcp > 0 then do:       
                           
                        find ext-item-uf-fcp exclusive-lock where
                             ext-item-uf-fcp.it-codigo       = item-uf.it-codigo       and
                             ext-item-uf-fcp.cod-estado-orig = item-uf.cod-estado-orig and
                             ext-item-uf-fcp.estado          = item-uf.estado          and
                             ext-item-uf-fcp.cod-estab       = item-uf.cod-estab       no-error.
                        if not avail ext-item-uf-fcp then do:
                        
                            create ext-item-uf-fcp.
                            assign ext-item-uf-fcp.it-codigo       = item-uf.it-codigo
                                   ext-item-uf-fcp.cod-estado-orig = item-uf.cod-estado-orig
                                   ext-item-uf-fcp.estado          = item-uf.estado
                                   ext-item-uf-fcp.cod-estab       = item-uf.cod-estab.                    
                        end.
                        
                        assign ext-item-uf-fcp.perc-fcp    = tt-subst-tribut.val-fcp
                               ext-item-uf-fcp.des-imp     = tt-subst-tribut.des-imp
                               ext-item-uf-fcp.des-msg-fcp = tt-subst-tribut.des-msg-fcp.
                    end.           
                end.                               
                else
                do:
                    create item-uf.
                    assign item-uf.it-codigo        = tt-subst-tribut.it-codigo
                           item-uf.cod-estado-orig  = tt-subst-tribut.cod-estado-orig
                           item-uf.estado           = tt-subst-tribut.estado
                           item-uf.pais             = "Brasil"
                           item-uf.per-sub-tri      = tt-subst-tribut.perc-sub-tri     
                           item-uf.perc-red-sub     = tt-subst-tribut.perc-red-sub    
                           item-uf.dec-1            = tt-subst-tribut.val-icms-est-sub.
                                               
                    if tt-subst-tribut.val-fcp > 0 then do:       
                        find ext-item-uf-fcp exclusive-lock where
                             ext-item-uf-fcp.it-codigo       = item-uf.it-codigo       and
                             ext-item-uf-fcp.cod-estado-orig = item-uf.cod-estado-orig and
                             ext-item-uf-fcp.estado          = item-uf.estado          and
                             ext-item-uf-fcp.cod-estab       = item-uf.cod-estab       no-error.
                        if not avail ext-item-uf-fcp then do:
                        
                            create ext-item-uf-fcp.
                            assign ext-item-uf-fcp.it-codigo       = item-uf.it-codigo
                                   ext-item-uf-fcp.cod-estado-orig = item-uf.cod-estado-orig
                                   ext-item-uf-fcp.estado          = item-uf.estado
                                   ext-item-uf-fcp.cod-estab       = item-uf.cod-estab.                    
                        end.
                                                                
                        assign ext-item-uf-fcp.perc-fcp    = tt-subst-tribut.val-fcp
                               ext-item-uf-fcp.des-imp     = tt-subst-tribut.des-imp
                               ext-item-uf-fcp.des-msg-fcp = tt-subst-tribut.des-msg-fcp.       
                    end.           
                end.
            end.
        end.
    end.

    run pi-finalizar in h-acomp.

    return "ok".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-importar-item-cli w-relat 
PROCEDURE pi-importar-item-cli :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    def var i-linha     as int          no-undo.
    def var i-columns   as int          no-undo.
    def var v-excel     as com-handle   no-undo.    

    empty temp-table tt-item-cli no-error.

    if c-arq-item-cli = "" then
        return "ok".

    create 'excel.application' v-excel no-error.
    v-excel:workbooks:open(c-arq-item-cli).
    v-excel:visible = no.
    assign i-linha = 2.

    repeat:
        if v-excel:workbooks(1):Worksheets(1):cells(i-linha,1):value = ? then
            leave.

        create tt-item-cli.
        assign tt-item-cli.nome-abrev       = substr(v-excel:workbooks(1):Worksheets(1):cells(i-linha, 8):value, 1, length(v-excel:workbooks(1):Worksheets(1):cells(i-linha, 8):value) - 1)
               tt-item-cli.it-codigo        = substr(v-excel:workbooks(1):Worksheets(1):cells(i-linha, 4):value, 1, length(v-excel:workbooks(1):Worksheets(1):cells(i-linha, 4):value) - 1)
               tt-item-cli.cod-emit         = int(v-excel:workbooks(1):Worksheets(1):cells(i-linha, 7):value)
               tt-item-cli.perc-subst-trib  = dec(v-excel:workbooks(1):Worksheets(1):cells(i-linha, 9):value).

        assign i-linha = i-linha + 1.
    end.

    v-excel:workbooks:close().
    release object v-excel no-error.

    return "ok".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-importar-subst-tribut w-relat 
PROCEDURE pi-importar-subst-tribut :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    def var i-linha     as int          no-undo.
    def var i-columns   as int          no-undo.
    def var v-excel     as com-handle   no-undo.    

    empty temp-table tt-subst-tribut no-error.

    if c-arq-subst-tribut = "" then
        return "ok".

    input from value(c-arq-subst-tribut) no-convert.

    repeat:
        import unformatted c-lin-prog.
        
        create tt-subst-tribut.
        assign tt-subst-tribut.it-codigo         = entry(1,c-lin-prog,";")
               tt-subst-tribut.cod-estado-orig   = entry(2,c-lin-prog,";")
               tt-subst-tribut.estado            = entry(3,c-lin-prog,";")
               tt-subst-tribut.perc-sub-tri      = dec(entry(4,c-lin-prog,";"))
               tt-subst-tribut.perc-red-sub      = dec(entry(5,c-lin-prog,";"))
               tt-subst-tribut.val-icms-est-sub  = dec(entry(6,c-lin-prog,";")).
               
        if num-entries (c-lin-prog, ";") >= 7 then
            assign tt-subst-tribut.val-fcp           = dec(entry(7,c-lin-prog,";"))
                   tt-subst-tribut.des-msg-fcp       = entry(8,c-lin-prog,";")
                   tt-subst-tribut.des-imp           = entry(9,c-lin-prog,";").
        
        assign i-linha = i-linha + 1.
    end.

    return "ok".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina w-relat 
PROCEDURE pi-troca-pagina :
/*:T------------------------------------------------------------------------------
  Purpose: Gerencia a Troca de P†gina (folder)   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{include/i-rptrp.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-relat  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-relat, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-relat 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
  
  run pi-trata-state (p-issuer-hdl, p-state).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

