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
{include/i-prgvrs.i ESCM104B-V01 12.01.00.001}

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
&Scoped-define EXTERNAL-TABLES es-acordo-faixa
&Scoped-define FIRST-EXTERNAL-TABLE es-acordo-faixa


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es-acordo-faixa.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es-acordo-faixa.desc-faixa ~
es-acordo-faixa.limite-inicial es-acordo-faixa.limite-final 
&Scoped-define ENABLED-TABLES es-acordo-faixa
&Scoped-define FIRST-ENABLED-TABLE es-acordo-faixa
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS es-acordo-faixa.cod-area ~
es-acordo-faixa.tp-docto es-acordo-faixa.num-faixa ~
es-acordo-faixa.desc-faixa es-acordo-faixa.limite-inicial ~
es-acordo-faixa.limite-final 
&Scoped-define DISPLAYED-TABLES es-acordo-faixa
&Scoped-define FIRST-DISPLAYED-TABLE es-acordo-faixa
&Scoped-Define DISPLAYED-OBJECTS c-desc-area c-desc-docto 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS es-acordo-faixa.num-faixa 
&Scoped-define ADM-MODIFY-FIELDS es-acordo-faixa.limite-inicial ~
es-acordo-faixa.limite-final 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
cod-area|y|y|mgesp.es-acordo-faixa.cod-area
tp-docto||y|mgesp.es-acordo-faixa.tp-docto
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "cod-area",
     Keys-Supplied = "cod-area,tp-docto"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE c-desc-area AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38.86 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-docto AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36.14 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 77 BY 3.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 77 BY 2.46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     es-acordo-faixa.cod-area AT ROW 1.17 COL 18 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     c-desc-area AT ROW 1.17 COL 26.14 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     es-acordo-faixa.tp-docto AT ROW 2.17 COL 18 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 3.43 BY .88
     c-desc-docto AT ROW 2.17 COL 21.57 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     es-acordo-faixa.num-faixa AT ROW 3.17 COL 18 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 3.43 BY .88
     es-acordo-faixa.desc-faixa AT ROW 3.17 COL 21.57 COLON-ALIGNED NO-LABEL WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 36.14 BY .88
     es-acordo-faixa.limite-inicial AT ROW 4.5 COL 18 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     es-acordo-faixa.limite-final AT ROW 5.5 COL 18 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 4.29 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgesp.es-acordo-faixa
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
         HEIGHT             = 5.75
         WIDTH              = 77.
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

/* SETTINGS FOR FILL-IN c-desc-area IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc-docto IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN es-acordo-faixa.cod-area IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN es-acordo-faixa.limite-final IN FRAME f-main
   3                                                                    */
/* SETTINGS FOR FILL-IN es-acordo-faixa.limite-inicial IN FRAME f-main
   3                                                                    */
/* SETTINGS FOR FILL-IN es-acordo-faixa.num-faixa IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN es-acordo-faixa.tp-docto IN FRAME f-main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

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
    WHEN 'cod-area':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = es-acordo-faixa
           &WHERE = "WHERE es-acordo-faixa.cod-area eq INTEGER(key-value)"
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
  {src/adm/template/row-list.i "es-acordo-faixa"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es-acordo-faixa"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.

    find es-acordo-area-docto no-lock where
         rowid(es-acordo-area-docto) = v-row-parent no-error.
    if avail es-acordo-area-docto then do:

        assign es-acordo-faixa.cod-area:screen-value in frame {&frame-name} = string(es-acordo-area-docto.cod-area)
               es-acordo-faixa.tp-docto:screen-value in frame {&frame-name} = string(es-acordo-area-docto.tp-docto).
        
        find es-acordo-area no-lock where 
             es-acordo-area.cod-area = input frame {&frame-name} es-acordo-faixa.cod-area no-error.
        if avail es-acordo-area then
            assign c-desc-area:screen-value in frame {&frame-name} = es-acordo-area.descricao.
        else
            assign c-desc-area:screen-value in frame {&frame-name} = "".
    
        find es-tipo-docto no-lock where 
             es-tipo-docto.tp-docto = input frame {&frame-name} es-acordo-faixa.tp-docto no-error.
        if avail es-tipo-docto then
            assign c-desc-docto:screen-value in frame {&frame-name} = es-tipo-docto.desc-docto.
        else
            assign c-desc-docto:screen-value in frame {&frame-name} = "".

        find last es-acordo-faixa no-lock where 
                  es-acordo-faixa.cod-area = es-acordo-area-docto.cod-area and 
                  es-acordo-faixa.tp-docto = es-acordo-area-docto.tp-docto no-error.
        if avail es-acordo-faixa then
            assign es-acordo-faixa.num-faixa:screen-value in frame {&frame-name} = string(es-acordo-faixa.num-faixa + 1).     
        else
            assign es-acordo-faixa.num-faixa:screen-value in frame {&frame-name} = "1".

    end.

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


    if adm-new-record = yes then do:

        if can-find(es-acordo-faixa where
                    es-acordo-faixa.cod-area  = input frame {&frame-name} es-acordo-faixa.cod-area   and
                    es-acordo-faixa.tp-docto  = input frame {&frame-name} es-acordo-faixa.tp-docto   and
                    es-acordo-faixa.num-faixa = input frame {&frame-name} es-acordo-faixa.num-faixa) then do:

            run utp/ut-msgs.p (input "show":U, 
                               input 7, 
                               input "Faixa").

            apply 'entry' to es-acordo-faixa.num-faixa in frame {&frame-name}.

            return 'ADM-ERROR'.

        end.
    end.

    if input frame {&frame-name} es-acordo-faixa.limite-final < input frame {&frame-name} es-acordo-faixa.limite-inicial then do:
        run utp/ut-msgs.p (input "show",
                           input 17006,
                           input "Limite Final Inv lido":U +
                                 "~~" +
                                 "Limite Final deve ser maior que o Limite Inicial.").

        apply 'entry' to es-acordo-faixa.limite-final in frame {&frame-name}.

        return 'ADM-ERROR':U.
    end.

    if input frame {&frame-name} es-acordo-faixa.limite-final <= 0 then do:
        run utp/ut-msgs.p (INPUT "show",
                           INPUT 17006,
                           INPUT "Limite Final Inv lido":U +
                                 "~~" +
                                 "Limite Final deve ser maior que 0.").

        apply 'entry' to es-acordo-faixa.limite-final in frame {&frame-name}.

        return 'ADM-ERROR':U.
    end.

    if can-find(first es-acordo-faixa where
                      es-acordo-faixa.cod-area        = input frame {&frame-name} es-acordo-faixa.cod-area        and
                      es-acordo-faixa.tp-docto        = input frame {&frame-name} es-acordo-faixa.tp-docto        and
                      es-acordo-faixa.num-faixa      <> input frame {&frame-name} es-acordo-faixa.num-faixa       and
                    ((es-acordo-faixa.limite-inicial <= input frame {&frame-name} es-acordo-faixa.limite-inicial  and
                      es-acordo-faixa.limite-final   >= input frame {&frame-name} es-acordo-faixa.limite-inicial) or
                     (es-acordo-faixa.limite-inicial <= input frame {&frame-name} es-acordo-faixa.limite-final    and
                      es-acordo-faixa.limite-final   >= input frame {&frame-name} es-acordo-faixa.limite-final))) then do:

        run utp/ut-msgs.p (input "show",
                           input 17006,
                           input "Faixa Inv lida" +
                                 "~~" +
                                 "Faixa j  existente!").

        apply 'entry' to es-acordo-faixa.limite-inicial in frame {&frame-name}.

        return 'ADM-ERROR':U.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* Dispatch standard ADM method.*/
    run dispatch in this-procedure ( input 'create-record':U ). 
    
    find es-acordo-area-docto where 
         rowid (es-acordo-area-docto) = v-row-parent no-lock no-error.
    if available es-acordo-area-docto then do:
        assign es-acordo-faixa.cod-area = es-acordo-area-docto.cod-area
               es-acordo-faixa.tp-docto = es-acordo-area-docto.tp-docto.
    end.


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
    

    find es-acordo-area no-lock where 
         es-acordo-area.cod-area = input frame {&frame-name} es-acordo-faixa.cod-area no-error.
    if avail es-acordo-area then
        assign c-desc-area:screen-value in frame {&frame-name} = es-acordo-area.descricao.
    else
        assign c-desc-area:screen-value in frame {&frame-name} = "".

    find es-tipo-docto no-lock where 
         es-tipo-docto.tp-docto = input frame {&frame-name} es-acordo-faixa.tp-docto no-error.
    if avail es-tipo-docto then
        assign c-desc-docto:screen-value in frame {&frame-name} = es-tipo-docto.desc-docto.
    else
        assign c-desc-docto:screen-value in frame {&frame-name} = "".

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
  {src/adm/template/sndkycas.i "cod-area" "es-acordo-faixa" "cod-area"}
  {src/adm/template/sndkycas.i "tp-docto" "es-acordo-faixa" "tp-docto"}

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
  {src/adm/template/snd-list.i "es-acordo-faixa"}

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

