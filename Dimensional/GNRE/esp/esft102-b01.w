&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgesp            PROGRESS
          movdis           PROGRESS
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
{include/i-prgvrs.i ESFT102-B02 12.01.00.001}

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

/*:T Variaveis usadas internamente pelo estilo, favor nao elimina-las     */

/*:T v†ri†veis de uso globla */
def new global shared var v-row-parent as rowid no-undo.

/*:T vari†veis de uso local */
def var v-row-table  as rowid.

/*:T fim das variaveis utilizadas no estilo */

def buffer bf-es-nota-fiscal-gnre for es-nota-fiscal-gnre.

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

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES es-lote-gnre
&Scoped-define FIRST-EXTERNAL-TABLE es-lote-gnre


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es-lote-gnre.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES es-nota-fiscal-gnre nota-fiscal

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table nota-fiscal.estado ~
es-nota-fiscal-gnre.cod-estabel es-nota-fiscal-gnre.serie ~
es-nota-fiscal-gnre.nr-nota-fis nota-fiscal.nome-ab-cli ~
nota-fiscal.dt-emis-nota es-nota-fiscal-gnre.cod-receita ~
es-nota-fiscal-gnre.vl-receita 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table 
&Scoped-define QUERY-STRING-br-table FOR EACH es-nota-fiscal-gnre OF es-lote-gnre NO-LOCK, ~
      EACH nota-fiscal OF es-nota-fiscal-gnre NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-table OPEN QUERY br-table FOR EACH es-nota-fiscal-gnre OF es-lote-gnre NO-LOCK, ~
      EACH nota-fiscal OF es-nota-fiscal-gnre NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-table es-nota-fiscal-gnre nota-fiscal
&Scoped-define FIRST-TABLE-IN-QUERY-br-table es-nota-fiscal-gnre
&Scoped-define SECOND-TABLE-IN-QUERY-br-table nota-fiscal


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-table bt-eliminar 

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
cod-estabel||y|mgesp.es-nota-fiscal-gnre.cod-estabel
cod-lote||y|mgesp.es-nota-fiscal-gnre.cod-lote
serie||y|mgesp.es-nota-fiscal-gnre.serie
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "cod-estabel,cod-lote,serie"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Optionsososos" B-table-Win _INLINE
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
DEFINE BUTTON bt-eliminar 
     LABEL "Eliminar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-idet 
     LABEL "&Detalhar" 
     SIZE 10 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      es-nota-fiscal-gnre, 
      nota-fiscal SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _STRUCTURED
  QUERY br-table NO-LOCK DISPLAY
      nota-fiscal.estado FORMAT "x(04)":U
      es-nota-fiscal-gnre.cod-estabel FORMAT "X(3)":U
      es-nota-fiscal-gnre.serie FORMAT "X(5)":U
      es-nota-fiscal-gnre.nr-nota-fis FORMAT "X(16)":U
      nota-fiscal.nome-ab-cli FORMAT "x(12)":U
      nota-fiscal.dt-emis-nota FORMAT "99/99/9999":U
      es-nota-fiscal-gnre.cod-receita FORMAT "999999":U
      es-nota-fiscal-gnre.vl-receita COLUMN-LABEL "Vl. Receita" FORMAT "->>>,>>>,>>9.99":U
            WIDTH 17.72
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 75 BY 6.5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-table AT ROW 1 COL 1
     bt-eliminar AT ROW 7.5 COL 1.14 WIDGET-ID 2
     bt-idet AT ROW 7.5 COL 12.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: mgesp.es-lote-gnre
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
         HEIGHT             = 7.54
         WIDTH              = 75.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit L-To-R                            */
/* BROWSE-TAB br-table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-idet IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       bt-idet:HIDDEN IN FRAME F-Main           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _TblList          = "mgesp.es-nota-fiscal-gnre OF mgesp.es-lote-gnre,movdis.nota-fiscal OF mgesp.es-nota-fiscal-gnre"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = movdis.nota-fiscal.estado
     _FldNameList[2]   = mgesp.es-nota-fiscal-gnre.cod-estabel
     _FldNameList[3]   = mgesp.es-nota-fiscal-gnre.serie
     _FldNameList[4]   = mgesp.es-nota-fiscal-gnre.nr-nota-fis
     _FldNameList[5]   = movdis.nota-fiscal.nome-ab-cli
     _FldNameList[6]   = movdis.nota-fiscal.dt-emis-nota
     _FldNameList[7]   = mgesp.es-nota-fiscal-gnre.cod-receita
     _FldNameList[8]   > mgesp.es-nota-fiscal-gnre.vl-receita
"es-nota-fiscal-gnre.vl-receita" "Vl. Receita" ? "decimal" ? ? ? ? ? ? no ? no no "17.72" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
    RUN New-State("DblClick, SELF":U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON ROW-ENTRY OF br-table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
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
  /* run new-state('New-Line|':U + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}))). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-eliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-eliminar B-table-Win
ON CHOOSE OF bt-eliminar IN FRAME F-Main /* Eliminar */
DO:

    if avail es-nota-fiscal-gnre then do:

        find es-lote-gnre no-lock where
             es-lote-gnre.cod-lote = es-nota-fiscal-gnre.cod-lote no-error.
        if avail es-lote-gnre then do:

            if es-lote-gnre.idi-situacao <> 1 then do:

                run utp/ut-msgs.p (input 'show',
                                   input 17006,
                                   input 'Nota Fiscal n∆o pode ser eliminada!~~Situaá∆o do Lote n∆o permite alteraá‰es':U).

                return no-apply.

            end.
            else do:

                run utp/ut-msgs.p (input "show":U,
                                   input  27100,
                                   input 'Eliminar Nota Fiscal do Lote?~~Confirma a eliminaá∆o da nota fiscal selecionado do Lote GNRE').

                if return-value = "YES":U then do:


                    for each bf-es-nota-fiscal-gnre exclusive-lock where
                             bf-es-nota-fiscal-gnre.cod-estabel = es-nota-fiscal-gnre.cod-estabel and
                             bf-es-nota-fiscal-gnre.serie       = es-nota-fiscal-gnre.serie       and
                             bf-es-nota-fiscal-gnre.nr-nota-fis = es-nota-fiscal-gnre.nr-nota-fis:

                        delete bf-es-nota-fiscal-gnre.

                    end.

                    {&open-query-br-table}

                end.
            end.
        end.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-idet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-idet B-table-Win
ON CHOOSE OF bt-idet IN FRAME F-Main /* Detalhar */
DO:
  apply "value-changed":U to br-table.
  RUN pi-atributo.
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "es-lote-gnre"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es-lote-gnre"}

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
  {src/adm/template/sndkycas.i "cod-estabel" "es-nota-fiscal-gnre" "cod-estabel"}
  {src/adm/template/sndkycas.i "cod-lote" "es-nota-fiscal-gnre" "cod-lote"}
  {src/adm/template/sndkycas.i "serie" "es-nota-fiscal-gnre" "serie"}

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
  {src/adm/template/snd-list.i "es-lote-gnre"}
  {src/adm/template/snd-list.i "es-nota-fiscal-gnre"}
  {src/adm/template/snd-list.i "nota-fiscal"}

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

