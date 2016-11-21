&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgcad            PROGRESS
          movdis           PROGRESS
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
{include/i-prgvrs.i ESWM003A 2.06.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def input-output param p-emb-ini like esp-resumo-wms.nr-embarque no-undo.
def input-output param p-emb-fim like esp-resumo-wms.nr-embarque no-undo.
def input-output param p-res-ini like esp-resumo-wms.nr-resumo   no-undo.
def input-output param p-res-fim like esp-resumo-wms.nr-resumo   no-undo.
def input-output param p-ped-ini like ped-venda.nr-pedcli        no-undo.
def input-output param p-ped-fim like ped-venda.nr-pedcli        no-undo.
def input-output param p-cli-ini like emitente.cod-emitente      no-undo.
def input-output param p-cli-fim like emitente.cod-emitente      no-undo.
def input-output param p-dat-ini like esp-resumo-wms.dt-envio    no-undo.
def input-output param p-dat-fim like esp-resumo-wms.dt-envio    no-undo.
def input-output param p-rd-opc-wis AS INTEGER                   no-undo.
def input-output param p-rd-opc-fis AS INTEGER                   no-undo.

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
&Scoped-Define ENABLED-OBJECTS RECT-1 IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 ~
IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 IMAGE-9 IMAGE-10 c-ini-emb c-fim-emb ~
c-ini-res c-fim-res c-ini-ped c-fim-ped c-ini-cli c-fim-cli c-ini-dat ~
c-fim-dat i-rd-opc-wis i-rd-opc-fis bt-ok bt-cancelar bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS c-ini-emb c-fim-emb c-ini-res c-fim-res ~
c-ini-ped c-fim-ped c-ini-cli c-fim-cli c-ini-dat c-fim-dat i-rd-opc-wis ~
i-rd-opc-fis 

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

DEFINE VARIABLE c-fim-cli AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE VARIABLE c-fim-dat AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE c-fim-emb AS INTEGER FORMAT ">>>>>>9":U INITIAL 9999999 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-fim-ped AS CHARACTER FORMAT "X(12)":U INITIAL "ZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 16.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-fim-res AS INTEGER FORMAT ">>>>>>9":U INITIAL 9999999 
     VIEW-AS FILL-IN 
     SIZE 10.29 BY .88 NO-UNDO.

DEFINE VARIABLE c-ini-cli LIKE emitente.cod-emitente
     LABEL "C¢d. Cliente" 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE VARIABLE c-ini-dat LIKE esp-resumo-wms.dt-envio
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-ini-emb LIKE esp-resumo-wms.nr-embarque
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-ini-ped LIKE ped-venda.nr-pedcli
     LABEL "Ped Cliente":R17 
     VIEW-AS FILL-IN 
     SIZE 16.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-ini-res LIKE esp-resumo-wms.nr-resumo
     VIEW-AS FILL-IN 
     SIZE 10.29 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE VARIABLE i-rd-opc-fis AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Confirmado", 1,
"NÆo Confirmado", 2,
"Todos", 3
     SIZE 45 BY .88 NO-UNDO.

DEFINE VARIABLE i-rd-opc-wis AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Confirmado", 1,
"NÆo Confirmado", 2,
"Todos", 3
     SIZE 45 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 77.86 BY 1.38
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     c-ini-emb AT ROW 2 COL 15 COLON-ALIGNED HELP
          "" WIDGET-ID 4
     c-fim-emb AT ROW 2 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     c-ini-res AT ROW 3 COL 15 COLON-ALIGNED HELP
          "" WIDGET-ID 12
     c-fim-res AT ROW 3 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     c-ini-ped AT ROW 4 COL 15 COLON-ALIGNED HELP
          "N£mero do pedido do cliente" WIDGET-ID 44
          LABEL "Ped Cliente":R17
     c-fim-ped AT ROW 4 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 42
     c-ini-cli AT ROW 5 COL 15 COLON-ALIGNED HELP
          "" WIDGET-ID 36
          LABEL "C¢d. Cliente"
     c-fim-cli AT ROW 5 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     c-ini-dat AT ROW 6 COL 15 COLON-ALIGNED HELP
          "" WIDGET-ID 26
     c-fim-dat AT ROW 6 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     i-rd-opc-wis AT ROW 7.08 COL 17 NO-LABEL WIDGET-ID 28
     i-rd-opc-fis AT ROW 8.08 COL 17 NO-LABEL WIDGET-ID 50
     bt-ok AT ROW 9.38 COL 3
     bt-cancelar AT ROW 9.38 COL 14
     bt-ajuda AT ROW 9.38 COL 69
     "FIS:" VIEW-AS TEXT
          SIZE 4.14 BY .88 AT ROW 8.08 COL 12.86 WIDGET-ID 54
     "WIS:" VIEW-AS TEXT
          SIZE 5 BY .88 AT ROW 7.08 COL 12 WIDGET-ID 32
     RECT-1 AT ROW 9.17 COL 2
     IMAGE-1 AT ROW 2 COL 34.43 WIDGET-ID 6
     IMAGE-2 AT ROW 2 COL 47.29 WIDGET-ID 8
     IMAGE-3 AT ROW 3 COL 34.43 WIDGET-ID 14
     IMAGE-4 AT ROW 3 COL 47.29 WIDGET-ID 16
     IMAGE-5 AT ROW 6 COL 34.43 WIDGET-ID 22
     IMAGE-6 AT ROW 6 COL 47.29 WIDGET-ID 24
     IMAGE-7 AT ROW 5 COL 34.43 WIDGET-ID 38
     IMAGE-8 AT ROW 5 COL 47.29 WIDGET-ID 40
     IMAGE-9 AT ROW 4 COL 34.43 WIDGET-ID 46
     IMAGE-10 AT ROW 4 COL 47.29 WIDGET-ID 48
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 9.67 WIDGET-ID 100.


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
         HEIGHT             = 9.67
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
/* SETTINGS FOR FILL-IN c-ini-cli IN FRAME F-Main
   LIKE = mgcad.emitente.cod-emitente EXP-LABEL EXP-SIZE                */
/* SETTINGS FOR FILL-IN c-ini-dat IN FRAME F-Main
   LIKE = mgcpb.esp-resumo-wms.dt-envio EXP-SIZE                        */
/* SETTINGS FOR FILL-IN c-ini-emb IN FRAME F-Main
   LIKE = mgcpb.esp-resumo-wms.nr-embarque EXP-SIZE                     */
/* SETTINGS FOR FILL-IN c-ini-ped IN FRAME F-Main
   LIKE = movdis.ped-venda.nr-pedcli EXP-LABEL                          */
/* SETTINGS FOR FILL-IN c-ini-res IN FRAME F-Main
   LIKE = mgcpb.esp-resumo-wms.nr-resumo EXP-SIZE                       */
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

    assign p-emb-ini    = input frame {&frame-name} c-ini-emb
           p-emb-fim    = input frame {&frame-name} c-fim-emb
           p-res-ini    = input frame {&frame-name} c-ini-res
           p-res-fim    = input frame {&frame-name} c-fim-res
           p-cli-ini    = input frame {&frame-name} c-ini-cli
           p-cli-fim    = input frame {&frame-name} c-fim-cli
           p-ped-ini    = input frame {&frame-name} c-ini-ped
           p-ped-fim    = input frame {&frame-name} c-fim-ped
           p-dat-ini    = input frame {&frame-name} c-ini-dat
           p-dat-fim    = input frame {&frame-name} c-fim-dat
           p-rd-opc-wis = input frame {&frame-name} i-rd-opc-wis
           p-rd-opc-fis = input frame {&frame-name} i-rd-opc-fis.

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
  DISPLAY c-ini-emb c-fim-emb c-ini-res c-fim-res c-ini-ped c-fim-ped c-ini-cli 
          c-fim-cli c-ini-dat c-fim-dat i-rd-opc-wis i-rd-opc-fis 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE RECT-1 IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 
         IMAGE-9 IMAGE-10 c-ini-emb c-fim-emb c-ini-res c-fim-res c-ini-ped 
         c-fim-ped c-ini-cli c-fim-cli c-ini-dat c-fim-dat i-rd-opc-wis 
         i-rd-opc-fis bt-ok bt-cancelar bt-ajuda 
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
  
  {utp/ut9000.i "ESWM003A" "2.06.00.001"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  if p-emb-ini <> 0 then
      assign c-ini-emb:screen-value in frame {&frame-name} = string(p-emb-ini).

  if p-emb-fim <> 0 then
      assign c-fim-emb:screen-value in frame {&frame-name} = string(p-emb-fim).

  if p-res-ini <> 0 then
      assign c-ini-res:screen-value in frame {&frame-name} = string(p-res-ini).

  if p-res-fim <> 0 then
      assign c-fim-res:screen-value in frame {&frame-name} = string(p-res-fim).

  if p-cli-ini <> 0 then
      assign c-ini-cli:screen-value in frame {&frame-name} = string(p-cli-ini).

  if p-cli-fim <> 0 then
      assign c-fim-cli:screen-value in frame {&frame-name} = string(p-cli-fim).

  if p-ped-ini <> "" then
      assign c-ini-ped:screen-value in frame {&frame-name} = string(p-ped-ini).

  if p-ped-fim <> "" then
      assign c-fim-ped:screen-value in frame {&frame-name} = string(p-ped-fim).

  if p-dat-ini = ? then
      assign c-ini-dat:screen-value in frame {&frame-name} = string(08/24/2015).
  else
      assign c-ini-dat:screen-value in frame {&frame-name} = string(p-dat-ini).

  if p-dat-fim = ? then
      assign c-fim-dat:screen-value in frame {&frame-name} = string(today).
  else
      assign c-fim-dat:screen-value in frame {&frame-name} = string(p-dat-fim).
             
  if p-rd-opc-wis <> 0 then
      assign i-rd-opc-wis:screen-value in frame {&frame-name} = string(p-rd-opc-wis).

  if p-rd-opc-fis <> 0 then
      assign i-rd-opc-fis:screen-value in frame {&frame-name} = string(p-rd-opc-fis).



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

