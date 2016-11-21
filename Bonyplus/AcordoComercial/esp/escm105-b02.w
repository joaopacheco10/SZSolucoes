&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgesp            PROGRESS
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
{include/i-prgvrs.i B99XX999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/browserd.w

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
define variable c-lista-valor as character init '':U no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES es-acordo-faixa

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table es-acordo-faixa.cod-area ~
es-acordo-faixa.tp-docto es-acordo-faixa.num-faixa ~
es-acordo-faixa.desc-faixa es-acordo-faixa.limite-inicial ~
es-acordo-faixa.limite-final 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table 
&Scoped-define QUERY-STRING-br-table FOR EACH es-acordo-faixa WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-table OPEN QUERY br-table FOR EACH es-acordo-faixa WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-table es-acordo-faixa
&Scoped-define FIRST-TABLE-IN-QUERY-br-table es-acordo-faixa


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 ~
IMAGE-6 IMAGE-7 IMAGE-8 IMAGE-9 IMAGE-10 c-ini-area c-fim-area bt-confirma ~
c-ini-docto c-fim-docto c-ini-faixa c-fim-faixa br-table 
&Scoped-Define DISPLAYED-OBJECTS c-ini-area c-fim-area c-ini-docto ~
c-fim-docto c-ini-faixa c-fim-faixa 

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
cod-area||y|mgesp.es-acordo-faixa.cod-area
tp-docto||y|mgesp.es-acordo-faixa.tp-docto
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "cod-area,tp-docto"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
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
************************
* Initialize Filter Attributes */
RUN set-attribute-list IN THIS-PROCEDURE ('
  Filter-Value=':U).
/************************
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-confirma 
     IMAGE-UP FILE "image\im-sav":U
     LABEL "Button 1" 
     SIZE 5.14 BY 1.

DEFINE VARIABLE c-fim-area AS INTEGER FORMAT ">>>>>9" INITIAL 999999 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE c-fim-docto AS INTEGER FORMAT ">9" INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 3.43 BY .88 NO-UNDO.

DEFINE VARIABLE c-fim-faixa AS INTEGER FORMAT ">9" INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 3.43 BY .88 NO-UNDO.

DEFINE VARIABLE c-ini-area LIKE es-acordo-faixa.cod-area
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE c-ini-docto LIKE es-acordo-faixa.tp-docto
     VIEW-AS FILL-IN 
     SIZE 3.43 BY .88 NO-UNDO.

DEFINE VARIABLE c-ini-faixa LIKE es-acordo-faixa.num-faixa
     VIEW-AS FILL-IN 
     SIZE 3.43 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-10
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-2
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-3
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-4
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-5
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-6
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-7
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-8
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-9
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      es-acordo-faixa SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _STRUCTURED
  QUERY br-table NO-LOCK DISPLAY
      es-acordo-faixa.cod-area COLUMN-LABEL "µrea":C FORMAT ">>>>>9":U
            WIDTH 7
      es-acordo-faixa.tp-docto COLUMN-LABEL "Documento":C FORMAT ">9":U
            WIDTH 11
      es-acordo-faixa.num-faixa COLUMN-LABEL "Faixa":C FORMAT ">9":U
            WIDTH 6
      es-acordo-faixa.desc-faixa COLUMN-LABEL "Descri‡Æo":C FORMAT "x(35)":U
            WIDTH 36.43
      es-acordo-faixa.limite-inicial COLUMN-LABEL "% Inicial":C FORMAT ">>9.99":U
      es-acordo-faixa.limite-final COLUMN-LABEL "% Final":C FORMAT ">>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 80 BY 6.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     c-ini-area AT ROW 1 COL 26 COLON-ALIGNED HELP
          "C¢digo da µrea"
     c-fim-area AT ROW 1 COL 42.43 COLON-ALIGNED NO-LABEL
     bt-confirma AT ROW 1 COL 76
     c-ini-docto AT ROW 2.04 COL 26 COLON-ALIGNED HELP
          "Tipo do Documento" WIDGET-ID 12
     c-fim-docto AT ROW 2.04 COL 42.43 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     c-ini-faixa AT ROW 3.08 COL 26 COLON-ALIGNED HELP
          "N£mero da Faixa" WIDGET-ID 24
     c-fim-faixa AT ROW 3.08 COL 42.43 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     br-table AT ROW 4.5 COL 1
     IMAGE-1 AT ROW 1 COL 36.57
     IMAGE-2 AT ROW 1 COL 41
     IMAGE-3 AT ROW 2.04 COL 36.57 WIDGET-ID 6
     IMAGE-4 AT ROW 2.04 COL 41 WIDGET-ID 8
     IMAGE-5 AT ROW 2.04 COL 36.57 WIDGET-ID 14
     IMAGE-6 AT ROW 2.04 COL 41 WIDGET-ID 16
     IMAGE-7 AT ROW 3.08 COL 36.57 WIDGET-ID 26
     IMAGE-8 AT ROW 3.08 COL 41 WIDGET-ID 28
     IMAGE-9 AT ROW 3.08 COL 36.57 WIDGET-ID 30
     IMAGE-10 AT ROW 3.08 COL 41 WIDGET-ID 32
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
         HEIGHT             = 10.17
         WIDTH              = 80.14.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{include/c-brwzoo.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit L-To-R                            */
/* BROWSE-TAB br-table c-fim-faixa F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN c-ini-area IN FRAME F-Main
   LIKE = mgesp.es-acordo-faixa.cod-area EXP-SIZE                       */
/* SETTINGS FOR FILL-IN c-ini-docto IN FRAME F-Main
   LIKE = mgesp.es-acordo-faixa.tp-docto EXP-SIZE                       */
/* SETTINGS FOR FILL-IN c-ini-faixa IN FRAME F-Main
   LIKE = mgesp.es-acordo-faixa.num-faixa EXP-SIZE                      */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _TblList          = "mgesp.es-acordo-faixa"
     _Options          = "NO-LOCK INDEXED-REPOSITION KEY-PHRASE SORTBY-PHRASE"
     _FldNameList[1]   > mgesp.es-acordo-faixa.cod-area
"cod-area" ? ? "integer" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "C" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > mgesp.es-acordo-faixa.tp-docto
"tp-docto" ? ? "integer" ? ? ? ? ? ? no ? no no "11" yes no no "U" "" "C" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > mgesp.es-acordo-faixa.num-faixa
"num-faixa" ? ? "integer" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "C" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > mgesp.es-acordo-faixa.desc-faixa
"desc-faixa" ? ? "character" ? ? ? ? ? ? no ? no no "36.43" yes no no "U" "" "C" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = mgesp.es-acordo-faixa.limite-inicial
     _FldNameList[6]   = mgesp.es-acordo-faixa.limite-final
     _Query            is NOT OPENED
*/  /* BROWSE br-table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-table
&Scoped-define SELF-NAME br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON MOUSE-SELECT-DBLCLICK OF br-table IN FRAME F-Main
DO:
    RUN New-State('DblClick':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON ROW-ENTRY OF br-table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
  
  run new-state('New-Line|':U + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}))).
  run seta-valor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON ROW-LEAVE OF br-table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON VALUE-CHANGED OF br-table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  run new-state('New-Line|':U + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}))).
  run new-state('Value-Changed|':U + string(this-procedure)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma B-table-Win
ON CHOOSE OF bt-confirma IN FRAME F-Main /* Button 1 */
DO:
  assign input frame {&frame-name} c-ini-area c-ini-docto c-ini-faixa c-fim-area c-fim-docto c-fim-faixa.
  RUN dispatch IN THIS-PROCEDURE ('open-query':U).
  apply 'value-changed':U to {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR Filter-Value AS CHAR NO-UNDO.

  /* Copy 'Filter-Attributes' into local variables. */
  RUN get-attribute ('Filter-Value':U).
  Filter-Value = RETURN-VALUE.

  /* No Foreign keys are accepted by this SmartObject. */

  {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view B-table-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  apply 'value-changed':U to {&browse-name} in frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-retorna-valor B-table-Win 
PROCEDURE pi-retorna-valor :
DEFINE INPUT PARAMETER P-CAMPO AS CHARACTER NO-UNDO.

    DEFINE VARIABLE P-VALOR AS CHAR INIT "" NO-UNDO.

    if  avail mgesp.es-acordo-faixa then do:
        case p-campo:
            when "cod-area" then
                assign p-valor = string(es-acordo-faixa.cod-area).
            when "tp-docto" then
                assign p-valor = string(es-acordo-faixa.tp-docto).
            when "num-faixa" then
                assign p-valor = string(es-acordo-faixa.num-faixa).
            when "desc-faixa" then
                assign p-valor = string(es-acordo-faixa.desc-faixa).
            when "limite-inicial" then
                assign p-valor = string(es-acordo-faixa.limite-inicial).
            when "limite-final" then
                assign p-valor = string(es-acordo-faixa.limite-final).
        end.
    end.
    return p-valor.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "cod-area" "es-acordo-faixa" "cod-area"}
  {src/adm/template/sndkycas.i "tp-docto" "es-acordo-faixa" "tp-docto"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "es-acordo-faixa"}

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
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "RetornaValorCampo" B-table-Win _INLINE
/* Actions: ? ? ? ? support/brwrtval.p */
/* Procedure desativada */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

