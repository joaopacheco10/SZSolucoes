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
{include/i-prgvrs.i ESWM005 2.06.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w

/* global variable definitions */

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
&Scoped-define EXTERNAL-TABLES esp-natur-frete
&Scoped-define FIRST-EXTERNAL-TABLE esp-natur-frete


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR esp-natur-frete.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS esp-natur-frete.nat-estadual ~
esp-natur-frete.nat-interestadual 
&Scoped-define ENABLED-TABLES esp-natur-frete
&Scoped-define FIRST-ENABLED-TABLE esp-natur-frete
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold RECT-3 
&Scoped-Define DISPLAYED-FIELDS esp-natur-frete.nat-operacao ~
esp-natur-frete.nat-estadual esp-natur-frete.nat-interestadual 
&Scoped-define DISPLAYED-TABLES esp-natur-frete
&Scoped-define FIRST-DISPLAYED-TABLE esp-natur-frete
&Scoped-Define DISPLAYED-OBJECTS c-desc-natur c-desc-natur-estadual ~
c-desc-natur-interestadual 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS esp-natur-frete.nat-operacao 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
nat-operacao|y|y|mgesp.esp-natur-frete.nat-operacao
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "nat-operacao",
     Keys-Supplied = "nat-operacao"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE c-desc-natur AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-natur-estadual AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-natur-interestadual AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 83 BY 3.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 1.5.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.43 BY 4.04.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     esp-natur-frete.nat-operacao AT ROW 1.29 COL 21.14 COLON-ALIGNED WIDGET-ID 6
          LABEL "Natureza"
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     c-desc-natur AT ROW 1.29 COL 31.29 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     esp-natur-frete.nat-estadual AT ROW 3.83 COL 21 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     c-desc-natur-estadual AT ROW 3.83 COL 31.14 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     esp-natur-frete.nat-interestadual AT ROW 4.83 COL 21 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     c-desc-natur-interestadual AT ROW 4.83 COL 31.14 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     "Natureza Frete" VIEW-AS TEXT
          SIZE 15 BY .67 AT ROW 2.92 COL 6.57 WIDGET-ID 18
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 2.71 COL 1
     RECT-3 AT ROW 3.25 COL 4 WIDGET-ID 16
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgesp.esp-natur-frete
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
         HEIGHT             = 6
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

/* SETTINGS FOR FILL-IN c-desc-natur IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc-natur-estadual IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc-natur-interestadual IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN esp-natur-frete.nat-operacao IN FRAME f-main
   NO-ENABLE 1 EXP-LABEL                                                */
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

&Scoped-define SELF-NAME esp-natur-frete.nat-estadual
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL esp-natur-frete.nat-estadual V-table-Win
ON F5 OF esp-natur-frete.nat-estadual IN FRAME f-main /* Estadual */
DO:
     {include/zoomvar.i &prog-zoom="inzoom/z01in245.w"
                        &campo=esp-natur-frete.nat-estadual
                        &campozoom=nat-operacao}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL esp-natur-frete.nat-estadual V-table-Win
ON LEAVE OF esp-natur-frete.nat-estadual IN FRAME f-main /* Estadual */
DO:
    FIND natur-oper
        WHERE natur-oper.nat-operacao = INPUT FRAME {&FRAME-NAME} esp-natur-frete.nat-estadual NO-LOCK NO-ERROR.
    IF AVAIL natur-oper THEN
        ASSIGN c-desc-natur-estadual:SCREEN-VALUE IN FRAME {&FRAME-NAME} = natur-oper.denominacao.
    ELSE
        ASSIGN c-desc-natur-estadual:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL esp-natur-frete.nat-estadual V-table-Win
ON MOUSE-SELECT-DBLCLICK OF esp-natur-frete.nat-estadual IN FRAME f-main /* Estadual */
DO:
   APPLY 'F5' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME esp-natur-frete.nat-interestadual
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL esp-natur-frete.nat-interestadual V-table-Win
ON F5 OF esp-natur-frete.nat-interestadual IN FRAME f-main /* Interestadual */
DO:
    {include/zoomvar.i &prog-zoom="inzoom/z01in245.w"
                       &campo=esp-natur-frete.nat-interestadual
                       &campozoom=nat-operacao}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL esp-natur-frete.nat-interestadual V-table-Win
ON LEAVE OF esp-natur-frete.nat-interestadual IN FRAME f-main /* Interestadual */
DO:
    FIND natur-oper
        WHERE natur-oper.nat-operacao = INPUT FRAME {&FRAME-NAME} esp-natur-frete.nat-interestadual NO-LOCK NO-ERROR.
    IF AVAIL natur-oper THEN
        ASSIGN c-desc-natur-interestadual:SCREEN-VALUE IN FRAME {&FRAME-NAME} = natur-oper.denominacao.
    ELSE
        ASSIGN c-desc-natur-interestadual:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL esp-natur-frete.nat-interestadual V-table-Win
ON MOUSE-SELECT-DBLCLICK OF esp-natur-frete.nat-interestadual IN FRAME f-main /* Interestadual */
DO:
    APPLY 'F5' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME esp-natur-frete.nat-operacao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL esp-natur-frete.nat-operacao V-table-Win
ON F5 OF esp-natur-frete.nat-operacao IN FRAME f-main /* Natureza */
DO:
    {include/zoomvar.i &prog-zoom="inzoom/z01in245.w"
                       &campo=esp-natur-frete.nat-operacao
                       &campozoom=nat-operacao}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL esp-natur-frete.nat-operacao V-table-Win
ON LEAVE OF esp-natur-frete.nat-operacao IN FRAME f-main /* Natureza */
DO:
    FIND natur-oper
        WHERE natur-oper.nat-operacao = INPUT FRAME {&FRAME-NAME} esp-natur-frete.nat-operacao NO-LOCK NO-ERROR.
    IF AVAIL natur-oper THEN
        ASSIGN c-desc-natur:SCREEN-VALUE IN FRAME {&FRAME-NAME} = natur-oper.denominacao.
    ELSE
        ASSIGN c-desc-natur:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL esp-natur-frete.nat-operacao V-table-Win
ON MOUSE-SELECT-DBLCLICK OF esp-natur-frete.nat-operacao IN FRAME f-main /* Natureza */
DO:
     APPLY 'F5' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

esp-natur-frete.nat-operacao:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
esp-natur-frete.nat-estadual:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
esp-natur-frete.nat-interestadual:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR key-value AS CHAR NO-UNDO.
  DEF VAR row-avail-enabled AS LOGICAL NO-UNDO.

  /* LOCK status on the find depends on FIELDS-ENABLED. */
  RUN get-attribute ('FIELDS-ENABLED':U).
  row-avail-enabled = (RETURN-VALUE eq 'yes':U).
  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'nat-operacao':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = esp-natur-frete
           &WHERE = "WHERE esp-natur-frete.nat-operacao eq key-value"
       }
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  {src/adm/template/row-list.i "esp-natur-frete"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "esp-natur-frete"}

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
/*     {include/i-valid.i} */

     IF  NOT FRAME {&FRAME-NAME}:VALIDATE() THEN
      RETURN 'ADM-ERROR':U.

     IF adm-new-record THEN DO:

         FIND natur-oper
            WHERE natur-oper.nat-operacao = INPUT FRAME {&FRAME-NAME} esp-natur-frete.nat-operacao NO-LOCK NO-ERROR.
         IF NOT AVAIL natur-oper THEN DO:
             RUN utp/ut-msgs.p (INPUT "show":U, 
                           INPUT 2, 
                           INPUT "Natureza Opera‡Æo").
             APPLY "entry" TO esp-natur-frete.nat-operacao IN FRAME {&FRAME-NAME}.
             UNDO, RETURN "adm-error". 
         END.

         IF esp-natur-frete.nat-operacao:SCREEN-VALUE = "" THEN DO:
             RUN utp/ut-msgs.p(INPUT 'show':u,
                               INPUT 164,
                               INPUT "Natureza").
             RETURN 'ADM-ERROR':U.
         END.

         FIND esp-natur-frete
            WHERE esp-natur-frete.nat-operacao = INPUT FRAME {&FRAME-NAME} esp-natur-frete.nat-operacao NO-LOCK NO-ERROR.
        IF AVAIL esp-natur-frete THEN DO:
            RUN utp/ut-msgs.p (INPUT "show":U, 
                               INPUT 1, 
                               INPUT "Natureza").
            APPLY "entry" TO esp-natur-frete.nat-operacao IN FRAME {&FRAME-NAME}.
            UNDO, RETURN "adm-error". 
        END.
     END.

     IF esp-natur-frete.nat-estadual:SCREEN-VALUE <> "" THEN DO:

        FIND natur-oper
           WHERE natur-oper.nat-operacao = INPUT FRAME {&FRAME-NAME} esp-natur-frete.nat-estadual NO-LOCK NO-ERROR.
        IF NOT AVAIL natur-oper THEN DO:
            RUN utp/ut-msgs.p (INPUT "show":U, 
                          INPUT 56, 
                          INPUT "Natureza Opera‡Æo").
            APPLY "entry" TO esp-natur-frete.nat-estadual IN FRAME {&FRAME-NAME}.
            UNDO, RETURN "adm-error". 
        END.
     END.

     IF esp-natur-frete.nat-interestadual:SCREEN-VALUE <> "" THEN DO:

        FIND natur-oper
           WHERE natur-oper.nat-operacao = INPUT FRAME {&FRAME-NAME} esp-natur-frete.nat-interestadual NO-LOCK NO-ERROR.
        IF NOT AVAIL natur-oper THEN DO:
            RUN utp/ut-msgs.p (INPUT "show":U, 
                          INPUT 56, 
                          INPUT "Natureza Opera‡Æo").
            APPLY "entry" TO esp-natur-frete.nat-interestadual IN FRAME {&FRAME-NAME}.
            UNDO, RETURN "adm-error".
        END.
     END.

     
    
    /*:T Ponha na pi-validate todas as valida‡äes */
    /*:T NÆo gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
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

/* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

    FIND natur-oper
        WHERE natur-oper.nat-operacao = INPUT FRAME {&FRAME-NAME} esp-natur-frete.nat-operacao NO-LOCK NO-ERROR.
    IF AVAIL natur-oper THEN
        ASSIGN c-desc-natur:SCREEN-VALUE IN FRAME {&FRAME-NAME} = natur-oper.denominacao.
    ELSE
        ASSIGN c-desc-natur:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

    FIND natur-oper
        WHERE natur-oper.nat-operacao = INPUT FRAME {&FRAME-NAME} esp-natur-frete.nat-estadual NO-LOCK NO-ERROR.
    IF AVAIL natur-oper THEN
        ASSIGN c-desc-natur-estadual:SCREEN-VALUE IN FRAME {&FRAME-NAME} = natur-oper.denominacao.
    ELSE
        ASSIGN c-desc-natur-estadual:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

    FIND natur-oper
        WHERE natur-oper.nat-operacao = INPUT FRAME {&FRAME-NAME} esp-natur-frete.nat-interestadual NO-LOCK NO-ERROR.
    IF AVAIL natur-oper THEN
        ASSIGN c-desc-natur-interestadual:SCREEN-VALUE IN FRAME {&FRAME-NAME} = natur-oper.denominacao.
    ELSE
        ASSIGN c-desc-natur-interestadual:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "nat-operacao" "esp-natur-frete" "nat-operacao"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "esp-natur-frete"}

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

