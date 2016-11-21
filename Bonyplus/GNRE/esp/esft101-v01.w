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
{include/i-prgvrs.i ESFT101-V01 9.99.99.999}

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
&Scoped-define EXTERNAL-TABLES es-param-gnre
&Scoped-define FIRST-EXTERNAL-TABLE es-param-gnre


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es-param-gnre.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es-param-gnre.cod-certificado ~
es-param-gnre.des-appserver es-param-gnre.url-ws-env ~
es-param-gnre.url-ws-cons es-param-gnre.url-ws-conf es-param-gnre.proxy-ip ~
es-param-gnre.proxy-porta es-param-gnre.ambiente es-param-gnre.proxy-user ~
es-param-gnre.proxy-pass 
&Scoped-define ENABLED-TABLES es-param-gnre
&Scoped-define FIRST-ENABLED-TABLE es-param-gnre
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold RECT-1 
&Scoped-Define DISPLAYED-FIELDS es-param-gnre.cod-estabel ~
es-param-gnre.cod-certificado es-param-gnre.des-appserver ~
es-param-gnre.url-ws-env es-param-gnre.url-ws-cons ~
es-param-gnre.url-ws-conf es-param-gnre.proxy-ip es-param-gnre.proxy-porta ~
es-param-gnre.ambiente es-param-gnre.proxy-user es-param-gnre.proxy-pass 
&Scoped-define DISPLAYED-TABLES es-param-gnre
&Scoped-define FIRST-DISPLAYED-TABLE es-param-gnre
&Scoped-Define DISPLAYED-OBJECTS c-desc-est 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS es-param-gnre.cod-estabel 
&Scoped-define ADM-MODIFY-FIELDS es-param-gnre.cod-certificado ~
es-param-gnre.des-appserver es-param-gnre.url-ws-env ~
es-param-gnre.url-ws-cons es-param-gnre.url-ws-conf es-param-gnre.proxy-ip ~
es-param-gnre.proxy-porta es-param-gnre.ambiente es-param-gnre.proxy-user ~
es-param-gnre.proxy-pass 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
cod-estabel|y|y|mgesp.es-param-gnre.cod-estabel
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "cod-estabel",
     Keys-Supplied = "cod-estabel"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE c-desc-est AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26 BY 5.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 1.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 11.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     es-param-gnre.cod-estabel AT ROW 1.17 COL 24 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 7 BY .88
     c-desc-est AT ROW 1.17 COL 31.14 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     es-param-gnre.cod-certificado AT ROW 2.75 COL 21 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 31.14 BY .88
     es-param-gnre.des-appserver AT ROW 3.75 COL 21 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 61.14 BY .88
     es-param-gnre.url-ws-env AT ROW 4.75 COL 21 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 61.14 BY .88
     es-param-gnre.url-ws-cons AT ROW 5.75 COL 21 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 61.14 BY .88
     es-param-gnre.url-ws-conf AT ROW 6.75 COL 21 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 61.14 BY .88
     es-param-gnre.proxy-ip AT ROW 7.75 COL 21 COLON-ALIGNED WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 16.14 BY .88
     es-param-gnre.proxy-porta AT ROW 8.75 COL 21 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .88
     es-param-gnre.ambiente AT ROW 9 COL 64 NO-LABEL WIDGET-ID 12
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Nenhum", 0,
"Produá∆o", 1,
"Homologaá∆o", 2
          SIZE 16 BY 3.75
     es-param-gnre.proxy-user AT ROW 9.75 COL 21 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 27 BY .88
     es-param-gnre.proxy-pass AT ROW 10.75 COL 21 COLON-ALIGNED WIDGET-ID 30 PASSWORD-FIELD 
          VIEW-AS FILL-IN 
          SIZE 21.14 BY .88
     "Ambiente" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 8 COL 59.29 WIDGET-ID 18
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 2.5 COL 1
     RECT-1 AT ROW 8.33 COL 58.29 WIDGET-ID 16
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgesp.es-param-gnre
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
         HEIGHT             = 13
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

/* SETTINGS FOR RADIO-SET es-param-gnre.ambiente IN FRAME f-main
   3                                                                    */
/* SETTINGS FOR FILL-IN c-desc-est IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN es-param-gnre.cod-certificado IN FRAME f-main
   3                                                                    */
/* SETTINGS FOR FILL-IN es-param-gnre.cod-estabel IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN es-param-gnre.des-appserver IN FRAME f-main
   3                                                                    */
/* SETTINGS FOR FILL-IN es-param-gnre.proxy-ip IN FRAME f-main
   3                                                                    */
/* SETTINGS FOR FILL-IN es-param-gnre.proxy-pass IN FRAME f-main
   3                                                                    */
/* SETTINGS FOR FILL-IN es-param-gnre.proxy-porta IN FRAME f-main
   3                                                                    */
/* SETTINGS FOR FILL-IN es-param-gnre.proxy-user IN FRAME f-main
   3                                                                    */
/* SETTINGS FOR FILL-IN es-param-gnre.url-ws-conf IN FRAME f-main
   3                                                                    */
/* SETTINGS FOR FILL-IN es-param-gnre.url-ws-cons IN FRAME f-main
   3                                                                    */
/* SETTINGS FOR FILL-IN es-param-gnre.url-ws-env IN FRAME f-main
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

&Scoped-define SELF-NAME es-param-gnre.cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-param-gnre.cod-estabel V-table-Win
ON F5 OF es-param-gnre.cod-estabel IN FRAME f-main /* Estabelecimento */
DO:

        {include~/zoomvar.i &prog-zoom=adzoom/z01ad107.w
                            &campo=es-param-gnre.cod-estabel
                            &campozoom=cod-estabel
                            &campo2=c-desc-est
                            &campozoom2=nome}

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-param-gnre.cod-estabel V-table-Win
ON LEAVE OF es-param-gnre.cod-estabel IN FRAME f-main /* Estabelecimento */
DO:

         {include/leave.i &tabela=estabelec
                          &atributo-ref=nome
                          &variavel-ref=c-desc-est
                          &where="estabelec.cod-estabel = input frame f-main es-param-gnre.cod-estabel"}
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-param-gnre.cod-estabel V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-param-gnre.cod-estabel IN FRAME f-main /* Estabelecimento */
do:

    apply 'f5' to self.
  
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

es-param-gnre.cod-estabel:load-mouse-pointer("image~\lupa.cur":U) in frame {&frame-name}.

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
    WHEN 'cod-estabel':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = es-param-gnre
           &WHERE = "WHERE es-param-gnre.cod-estabel eq key-value"
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
  {src/adm/template/row-list.i "es-param-gnre"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es-param-gnre"}

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
    
    /*:T Ponha na pi-validate todas as validaá‰es */
    /*:T N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */

    if adm-new-record then do:

        if can-find(es-param-gnre where
                    es-param-gnre.cod-estabel = input frame {&frame-name} es-param-gnre.cod-estabel) then do:

            run utp/ut-msgs.p (input 'show',
                               input 17006,
                               input 'J† existe registro para o Estabelecimento informada!').

            apply 'entry' to es-param-gnre.cod-estabel in frame {&frame-name}.
            return 'ADM-ERROR'.

        end.
    end.

    if input frame {&frame-name} es-param-gnre.ambiente <> 0 then do:

        if input frame {&frame-name} es-param-gnre.des-appserver = "" then do:
            
            run utp/ut-msgs.p (input 'show',
                               input 17006,
                               input 'AppServer Inv†lido!~~AppServer deve ser informado').
    
            apply 'entry' to es-param-gnre.des-appserver in frame {&frame-name}.
            return 'ADM-ERROR'.

        end.

        if input frame {&frame-name} es-param-gnre.url-ws-env = "" then do:
            
            run utp/ut-msgs.p (input 'show',
                               input 17006,
                               input 'WS Envio Inv†lido!~~WS Envio deve ser informado').
    
            apply 'entry' to es-param-gnre.url-ws-env in frame {&frame-name}.
            return 'ADM-ERROR'.

        end.

        if input frame {&frame-name} es-param-gnre.url-ws-cons = "" then do:
            
            run utp/ut-msgs.p (input 'show',
                               input 17006,
                               input 'WS Consulta Inv†lido!~~WS Consulta deve ser informado').
    
            apply 'entry' to es-param-gnre.url-ws-cons in frame {&frame-name}.
            return 'ADM-ERROR'.

        end.

        if input frame {&frame-name} es-param-gnre.url-ws-conf = "" then do:
            
            run utp/ut-msgs.p (input 'show',
                               input 17006,
                               input 'WS Config UF Inv†lido!~~WS Config UF deve ser informado').
    
            apply 'entry' to es-param-gnre.url-ws-conf in frame {&frame-name}.
            return 'ADM-ERROR'.

        end.

        if input frame {&frame-name} es-param-gnre.cod-certificado = "" then do:

            run utp/ut-msgs.p (input 'show',
                               input 17006,
                               input 'Certificado Inv†lido!~~Certificado deve ser informado').
    
            apply 'entry' to es-param-gnre.cod-certificado in frame {&frame-name}.
            return 'ADM-ERROR'.

        end.

    end.

    if not can-find(estabelec where
                    estabelec.cod-estabel = input frame {&frame-name} es-param-gnre.cod-estabel) then do:

        run utp/ut-msgs.p (input 'show',
                           input 17006,
                           input 'Estabelecimento Inv†lido!~~C¢digo Estabelecimento n∆o localizado').

        apply 'entry' to es-param-gnre.cod-estabel in frame {&frame-name}.
        return 'ADM-ERROR'.

    end.        
    
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
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


    if avail es-param-gnre then do:

        find estabelec no-lock where
             estabelec.cod-estabel = es-param-gnre.cod-estabel no-error.
        if avail estabelec then
            assign c-desc-est:screen-value in frame {&frame-name} = estabelec.nome.

    end.


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-rowid-param V-table-Win 
PROCEDURE pi-rowid-param :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def output param p-row-param-gnre as rowid no-undo.


if avail es-param-gnre then
    assign p-row-param-gnre = rowid(es-param-gnre).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*:T------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: N∆o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
  ainda n∆o foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /*:T Validaá∆o de dicion†rio */
    
/*:T    Segue um exemplo de validaá∆o de programa */
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
  {src/adm/template/sndkycas.i "cod-estabel" "es-param-gnre" "cod-estabel"}

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
  {src/adm/template/snd-list.i "es-param-gnre"}

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

