&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgesp            PROGRESS
*/
&Scoped-define WINDOW-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer unid_negoc for emscad.unid_negoc.

{include/i-prgvrs.i ESFT101A 12.01.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
def input param p-row-param as rowid no-undo.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE JanelaDetalhe
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 c-cod-ser-docto c-cod-espec-docto ~
c-cod-plano-cta-ctbl c-cod-cta-ctbl c-cod-plano-ccusto c-cod-ccusto ~
c-cod-tip-fluxo-financ c-cod-unid-negoc bt-ok bt-cancelar bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS c-cod-ser-docto c-cod-espec-docto ~
c-cod-plano-cta-ctbl c-cod-cta-ctbl c-cod-plano-ccusto c-cod-ccusto ~
c-cod-tip-fluxo-financ c-cod-unid-negoc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-window AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE c-cod-ccusto AS CHARACTER FORMAT "x(11)" 
     LABEL "Centro Custo" 
     VIEW-AS FILL-IN 
     SIZE 15.14 BY .88.

DEFINE VARIABLE c-cod-cta-ctbl AS CHARACTER FORMAT "x(20)" 
     LABEL "Conta Cont bil" 
     VIEW-AS FILL-IN 
     SIZE 21.14 BY .88.

DEFINE VARIABLE c-cod-espec-docto AS CHARACTER FORMAT "x(3)" 
     LABEL "Esp‚cie Documento" 
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .88.

DEFINE VARIABLE c-cod-plano-ccusto AS CHARACTER FORMAT "x(8)" 
     LABEL "Plano Centros Custo" 
     VIEW-AS FILL-IN 
     SIZE 12.14 BY .88.

DEFINE VARIABLE c-cod-plano-cta-ctbl AS CHARACTER FORMAT "x(8)" 
     LABEL "Plano Contas" 
     VIEW-AS FILL-IN 
     SIZE 12.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-ser-docto AS CHARACTER FORMAT "x(3)" 
     LABEL "S‚rie Documento" 
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-tip-fluxo-financ AS CHARACTER FORMAT "x(12)" 
     LABEL "Tipo Fluxo Financ" 
     VIEW-AS FILL-IN 
     SIZE 16.14 BY .88.

DEFINE VARIABLE c-cod-unid-negoc LIKE es-param-gnre.cod-unid-negoc
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 77.86 BY 1.38
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     c-cod-ser-docto AT ROW 1.25 COL 25.86 COLON-ALIGNED HELP
          "S‚rie Documento" WIDGET-ID 12
     c-cod-espec-docto AT ROW 2.25 COL 25.86 COLON-ALIGNED HELP
          "Esp‚cie Documento" WIDGET-ID 6
     c-cod-plano-cta-ctbl AT ROW 3.25 COL 25.86 COLON-ALIGNED HELP
          "Plano Contas" WIDGET-ID 10
     c-cod-cta-ctbl AT ROW 4.25 COL 25.86 COLON-ALIGNED HELP
          "Conta Cont bil" WIDGET-ID 4
     c-cod-plano-ccusto AT ROW 5.25 COL 25.86 COLON-ALIGNED HELP
          "Plano Centros Custo" WIDGET-ID 8
     c-cod-ccusto AT ROW 6.25 COL 25.86 COLON-ALIGNED HELP
          "Centro Custo" WIDGET-ID 2
     c-cod-tip-fluxo-financ AT ROW 7.25 COL 25.86 COLON-ALIGNED HELP
          "Tipo Fluxo Financ" WIDGET-ID 14
     c-cod-unid-negoc AT ROW 8.25 COL 26 COLON-ALIGNED HELP
          "Unid Neg¢cio" WIDGET-ID 16
     bt-ok AT ROW 9.63 COL 3
     bt-cancelar AT ROW 9.63 COL 14
     bt-ajuda AT ROW 9.63 COL 69
     RECT-1 AT ROW 9.42 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 9.92 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: JanelaDetalhe
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert Custom SmartWindow title>"
         HEIGHT             = 10.08
         WIDTH              = 80
         MAX-HEIGHT         = 21.13
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.13
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-window 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-window.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-window
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN c-cod-unid-negoc IN FRAME F-Main
   LIKE = mgesp.es-param-gnre.cod-unid-negoc EXP-SIZE                   */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* <insert Custom SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* <insert Custom SmartWindow title> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-window
ON CHOOSE OF bt-ajuda IN FRAME F-Main /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-window
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:

    if not can-find(espec_docto_financ where
                    espec_docto_financ.cod_espec_docto = c-cod-espec-docto:screen-value in frame {&frame-name}) then do:

        run utp/ut-msgs.p (input 'show',
                           input 17006,
                           input 'Esp‚cie Documento Inv lida!':U).

        apply 'entry' to c-cod-espec-docto in frame {&frame-name}.
        return no-apply.

    end.

    if not can-find(plano_cta_ctbl where
                    plano_cta_ctbl.cod_plano_cta_ctbl = c-cod-plano-cta-ctbl:screen-value in frame {&frame-name}) then do:

        run utp/ut-msgs.p (input 'show',
                           input 17006,
                           input 'Plano de Contas Inv lido!':U).

        apply 'entry' to c-cod-plano-cta-ctbl in frame {&frame-name}.
        return no-apply.

    end.

    if not can-find(first plano_ccusto where
                          plano_ccusto.cod_plano_ccusto = c-cod-plano-ccusto:screen-value in frame {&frame-name}) then do:

        run utp/ut-msgs.p (input 'show',
                           input 17006,
                           input 'Plano de Centros de Custo Inv lido!':U).

        apply 'entry' to c-cod-plano-ccusto in frame {&frame-name}.
        return no-apply.            

    end.

    if not can-find(first emscad.ccusto where
                          ccusto.cod_plano_ccusto = c-cod-plano-ccusto:screen-value in frame {&frame-name} and
                          ccusto.cod_ccusto       = c-cod-ccusto:screen-value in frame {&frame-name})      then do:

        run utp/ut-msgs.p (input 'show',
                           input 17006,
                           input 'Centro de Custo Inv lido!':U).

        apply 'entry' to c-cod-ccusto in frame {&frame-name}.
        return no-apply.  

    end.

    if not can-find(cta_ctbl where
                    cta_ctbl.cod_plano_cta_ctbl = c-cod-plano-cta-ctbl:screen-value in frame {&frame-name} and
                    cta_ctbl.cod_cta_ctbl       = c-cod-cta-ctbl:screen-value in frame {&frame-name})      then do:

        run utp/ut-msgs.p (input 'show',
                           input 17006,
                           input 'Conta Cont bil Inv lida!':U).

        apply 'entry' to c-cod-cta-ctbl in frame {&frame-name}.
        return no-apply.  

    end.

    if not can-find(tip_fluxo_financ where
                    tip_fluxo_financ.cod_tip_fluxo_financ = c-cod-tip-fluxo-financ:screen-value in frame {&frame-name}) then do:

        run utp/ut-msgs.p (input 'show',
                           input 17006,
                           input 'Tipo Fluxo Cont bil Inv lido!':U).

        apply 'entry' to c-cod-cta-ctbl in frame {&frame-name}.
        return no-apply.

    end.

    if not can-find(unid_negoc where
                    unid_negoc.cod_unid_negoc = c-cod-unid-negoc:screen-value in frame {&frame-name}) then do:

        run utp/ut-msgs.p (input 'show',
                           input 17006,
                           input 'Unidade de Neg¢cio Inv lida!':U).

        apply 'entry' to c-cod-unid-negoc in frame {&frame-name}.
        return no-apply.

    end.

    find es-param-gnre exclusive-lock where
         rowid(es-param-gnre) = p-row-param no-error.
    if avail es-param-gnre then
        assign es-param-gnre.cod-ser-docto        = c-cod-ser-docto:screen-value in frame {&frame-name}       
               es-param-gnre.cod-espec-docto      = c-cod-espec-docto:screen-value in frame {&frame-name}            
               es-param-gnre.cod-plano-cta-ctbl   = c-cod-plano-cta-ctbl:screen-value in frame {&frame-name}         
               es-param-gnre.cod-cta-ctbl         = c-cod-cta-ctbl:screen-value in frame {&frame-name}               
               es-param-gnre.cod-plano-ccusto     = c-cod-plano-ccusto:screen-value in frame {&frame-name}           
               es-param-gnre.cod-ccusto           = c-cod-ccusto:screen-value in frame {&frame-name}                 
               es-param-gnre.cod-tip-fluxo-financ = c-cod-tip-fluxo-financ:screen-value in frame {&frame-name}
               es-param-gnre.cod-unid-negoc       = c-cod-unid-negoc:screen-value in frame {&frame-name}.


  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-window  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-window  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-window  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
  THEN DELETE WIDGET w-window.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-window  _DEFAULT-ENABLE
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
  DISPLAY c-cod-ser-docto c-cod-espec-docto c-cod-plano-cta-ctbl c-cod-cta-ctbl 
          c-cod-plano-ccusto c-cod-ccusto c-cod-tip-fluxo-financ 
          c-cod-unid-negoc 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE RECT-1 c-cod-ser-docto c-cod-espec-docto c-cod-plano-cta-ctbl 
         c-cod-cta-ctbl c-cod-plano-ccusto c-cod-ccusto c-cod-tip-fluxo-financ 
         c-cod-unid-negoc bt-ok bt-cancelar bt-ajuda 
      WITH FRAME F-Main IN WINDOW w-window.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-window.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-window 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-window 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-window 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}
  
  {utp/ut9000.i "ESFT101A" "12.01.00.001"}


  find es-param-gnre no-lock where
       rowid(es-param-gnre) = p-row-param no-error.
  if avail es-param-gnre then
      assign c-cod-ser-docto        = es-param-gnre.cod-ser-docto
             c-cod-espec-docto      = es-param-gnre.cod-espec-docto
             c-cod-plano-cta-ctbl   = es-param-gnre.cod-plano-cta-ctbl
             c-cod-cta-ctbl         = es-param-gnre.cod-cta-ctbl
             c-cod-plano-ccusto     = es-param-gnre.cod-plano-ccusto
             c-cod-ccusto           = es-param-gnre.cod-ccusto
             c-cod-tip-fluxo-financ = es-param-gnre.cod-tip-fluxo-financ.


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-window  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this JanelaDetalhe, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-window 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

