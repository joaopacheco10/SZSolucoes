&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgesp            PROGRESS
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
{include/i-prgvrs.i ESFT100A 12.01.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

def input-output param p-cod-est-ini like estabelec.cod-estabel    no-undo.
def input-output param p-cod-est-fim like estabelec.cod-estabel    no-undo.
def input-output param p-serie-ini   like nota-fiscal.serie        no-undo.
def input-output param p-serie-fim   like nota-fiscal.serie        no-undo.
def input-output param p-nota-ini    like nota-fiscal.nr-nota-fis  no-undo.
def input-output param p-nota-fim    like nota-fiscal.nr-nota-fis  no-undo.
def input-output param p-emis-ini    like nota-fiscal.dt-emis-nota no-undo.
def input-output param p-emis-fim    like nota-fiscal.dt-emis-nota no-undo.
def input-output param p-uf-ini      like nota-fiscal.estado       no-undo.
def input-output param p-uf-fim      like nota-fiscal.estado       no-undo.
def input-output param p-nome-ini    like nota-fiscal.nome-ab-cli  no-undo.
def input-output param p-nome-fim    like nota-fiscal.nome-ab-cli  no-undo.
def output       param p-bt-ok       as log                        no-undo.

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
&Scoped-Define ENABLED-OBJECTS RECT-1 IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 ~
IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 IMAGE-9 IMAGE-10 IMAGE-11 IMAGE-12 ~
c-ini-est c-fim-est c-ini-ser c-fim-ser c-ini-nota c-fim-nota c-ini-emis ~
c-fim-emis c-ini-uf c-fim-uf c-ini-nome c-fim-nome bt-ok bt-cancelar ~
bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS c-ini-est c-fim-est c-ini-ser c-fim-ser ~
c-ini-nota c-fim-nota c-ini-emis c-fim-emis c-ini-uf c-fim-uf c-ini-nome ~
c-fim-nome 

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

DEFINE VARIABLE c-fim-emis AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-fim-est AS CHARACTER FORMAT "X(3)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 7.43 BY .88 NO-UNDO.

DEFINE VARIABLE c-fim-nome AS CHARACTER FORMAT "X(12)":U INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 16.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-fim-nota AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-fim-ser AS CHARACTER FORMAT "X(5)":U INITIAL "ZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-fim-uf AS CHARACTER FORMAT "x(4)":U INITIAL "ZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 8.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-ini-emis LIKE nota-fiscal.dt-emis-nota
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-ini-est LIKE es-nota-fiscal-gnre.cod-estabel
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-ini-nome LIKE nota-fiscal.nome-ab-cli
     VIEW-AS FILL-IN 
     SIZE 16.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-ini-nota LIKE es-nota-fiscal-gnre.nr-nota-fis
     VIEW-AS FILL-IN 
     SIZE 17.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-ini-ser LIKE es-nota-fiscal-gnre.serie
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-ini-uf LIKE nota-fiscal.estado
     VIEW-AS FILL-IN 
     SIZE 8.14 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
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

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 77.86 BY 1.38
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     c-ini-est AT ROW 1.25 COL 17 COLON-ALIGNED HELP
          "" WIDGET-ID 4
     c-fim-est AT ROW 1.25 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     c-ini-ser AT ROW 2.25 COL 17 COLON-ALIGNED HELP
          "" WIDGET-ID 12
     c-fim-ser AT ROW 2.25 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     c-ini-nota AT ROW 3.25 COL 17 COLON-ALIGNED HELP
          "" WIDGET-ID 20
     c-fim-nota AT ROW 3.25 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     c-ini-emis AT ROW 4.25 COL 17 COLON-ALIGNED HELP
          "Data de emiss∆o da nota fiscal" WIDGET-ID 28
     c-fim-emis AT ROW 4.25 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     c-ini-uf AT ROW 5.25 COL 17 COLON-ALIGNED HELP
          "Unidade da federaá∆o" WIDGET-ID 36
     c-fim-uf AT ROW 5.25 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     c-ini-nome AT ROW 6.29 COL 17 COLON-ALIGNED HELP
          "Nome abreviado do cliente/fornecedor" WIDGET-ID 44
     c-fim-nome AT ROW 6.29 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 42
     bt-ok AT ROW 7.71 COL 3
     bt-cancelar AT ROW 7.71 COL 14
     bt-ajuda AT ROW 7.71 COL 69
     RECT-1 AT ROW 7.5 COL 2
     IMAGE-1 AT ROW 1.25 COL 37.72 WIDGET-ID 6
     IMAGE-2 AT ROW 1.25 COL 41.72 WIDGET-ID 8
     IMAGE-3 AT ROW 2.25 COL 37.72 WIDGET-ID 14
     IMAGE-4 AT ROW 2.25 COL 41.72 WIDGET-ID 16
     IMAGE-5 AT ROW 3.25 COL 37.72 WIDGET-ID 22
     IMAGE-6 AT ROW 3.25 COL 41.72 WIDGET-ID 24
     IMAGE-7 AT ROW 4.25 COL 37.72 WIDGET-ID 30
     IMAGE-8 AT ROW 4.25 COL 41.72 WIDGET-ID 32
     IMAGE-9 AT ROW 5.25 COL 37.72 WIDGET-ID 38
     IMAGE-10 AT ROW 5.25 COL 41.72 WIDGET-ID 40
     IMAGE-11 AT ROW 6.29 COL 37.72 WIDGET-ID 48
     IMAGE-12 AT ROW 6.29 COL 41.72 WIDGET-ID 46
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 7.96 WIDGET-ID 100.


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
         HEIGHT             = 7.96
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
/* SETTINGS FOR FILL-IN c-ini-emis IN FRAME F-Main
   LIKE = movdis.nota-fiscal.dt-emis-nota EXP-SIZE                      */
/* SETTINGS FOR FILL-IN c-ini-est IN FRAME F-Main
   LIKE = mgesp.es-nota-fiscal-gnre.cod-estabel EXP-SIZE                */
/* SETTINGS FOR FILL-IN c-ini-nome IN FRAME F-Main
   LIKE = movdis.nota-fiscal.nome-ab-cli EXP-SIZE                       */
/* SETTINGS FOR FILL-IN c-ini-nota IN FRAME F-Main
   LIKE = mgesp.es-nota-fiscal-gnre.nr-nota-fis EXP-SIZE                */
/* SETTINGS FOR FILL-IN c-ini-ser IN FRAME F-Main
   LIKE = mgesp.es-nota-fiscal-gnre.serie EXP-SIZE                      */
/* SETTINGS FOR FILL-IN c-ini-uf IN FRAME F-Main
   LIKE = movdis.nota-fiscal.estado EXP-SIZE                            */
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

    assign p-bt-ok = no.

    apply "close":U to this-procedure.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:

    assign p-cod-est-ini = input frame {&frame-name} c-ini-est
           p-cod-est-fim = input frame {&frame-name} c-fim-est
           p-serie-ini   = input frame {&frame-name} c-ini-ser
           p-serie-fim   = input frame {&frame-name} c-fim-ser
           p-nota-ini    = input frame {&frame-name} c-ini-nota
           p-nota-fim    = input frame {&frame-name} c-fim-nota
           p-emis-ini    = input frame {&frame-name} c-ini-emis
           p-emis-fim    = input frame {&frame-name} c-fim-emis
           p-uf-ini      = input frame {&frame-name} c-ini-uf
           p-uf-fim      = input frame {&frame-name} c-fim-uf
           p-nome-ini    = input frame {&frame-name} c-ini-nome
           p-nome-fim    = input frame {&frame-name} c-fim-nome
           p-bt-ok       = yes.

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
  DISPLAY c-ini-est c-fim-est c-ini-ser c-fim-ser c-ini-nota c-fim-nota 
          c-ini-emis c-fim-emis c-ini-uf c-fim-uf c-ini-nome c-fim-nome 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE RECT-1 IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 
         IMAGE-9 IMAGE-10 IMAGE-11 IMAGE-12 c-ini-est c-fim-est c-ini-ser 
         c-fim-ser c-ini-nota c-fim-nota c-ini-emis c-fim-emis c-ini-uf 
         c-fim-uf c-ini-nome c-fim-nome bt-ok bt-cancelar bt-ajuda 
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
  
  {utp/ut9000.i "ESFT100A" "12.01.00.001"}

  assign c-ini-est  = p-cod-est-ini
         c-fim-est  = p-cod-est-fim
         c-ini-ser  = p-serie-ini  
         c-fim-ser  = p-serie-fim  
         c-ini-nota = p-nota-ini   
         c-fim-nota = p-nota-fim   
         c-ini-emis = p-emis-ini   
         c-fim-emis = p-emis-fim.


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

