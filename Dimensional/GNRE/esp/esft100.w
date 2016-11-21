&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESFT100 12.01.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def temp-table tt-nota-fiscal no-undo like nota-fiscal
    field lg-selec   as log label 'X':C5
    field vl-icms-st as dec label 'Vl. ICMS ST'
    field vl-fcp     as dec label 'Vl. FCP'.

def var c-est-ini as char no-undo.
def var c-est-fim as char no-undo.
def var c-ser-ini as char no-undo.
def var c-ser-fim as char no-undo.
def var c-not-ini as char no-undo.
def var c-not-fim as char no-undo.
def var c-dt-ini  as date no-undo.
def var c-dt-fim  as date no-undo.
def var c-uf-ini  as char no-undo.
def var c-uf-fim  as char no-undo.
def var c-nom-ini as char no-undo.
def var c-nom-fim as char no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button bt-filtro bt-refresh 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-livre AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-programa 
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-livre MENUBAR
       SUB-MENU  mi-programa    LABEL "&Nome-do-Programa"
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_esft100-b01 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-filtro 
     IMAGE-UP FILE "image/toolbar/im-fil.bmp":U
     IMAGE-DOWN FILE "image/toolbar/ii-fil.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-fil.bmp":U
     LABEL "Filtro" 
     SIZE 4 BY 1.13.

DEFINE BUTTON bt-refresh 
     IMAGE-UP FILE "image/toolbar/im-relo.bmp":U
     IMAGE-DOWN FILE "image/toolbar/ii-relo.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-relo.bmp":U
     LABEL "Button 1" 
     SIZE 4 BY 1.13.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 111 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-filtro AT ROW 1.17 COL 4 WIDGET-ID 2
     bt-refresh AT ROW 1.17 COL 10 WIDGET-ID 4
     rt-button AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 111.14 BY 24.29 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-livre ASSIGN
         HIDDEN             = YES
         TITLE              = "Template Livre <Insira complemento>"
         HEIGHT             = 24.33
         WIDTH              = 111.29
         MAX-HEIGHT         = 26.08
         MAX-WIDTH          = 161.43
         VIRTUAL-HEIGHT     = 26.08
         VIRTUAL-WIDTH      = 161.43
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-livre:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-livre 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-livre.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-livre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Template Livre <Insira complemento> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Template Livre <Insira complemento> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-filtro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-filtro w-livre
ON CHOOSE OF bt-filtro IN FRAME f-cad /* Filtro */
DO:

    def var l-ok as log no-undo.

    current-window:sensitive = no.

    run esp/esft100a.w (input-output c-est-ini,
                        input-output c-est-fim,
                        input-output c-ser-ini,
                        input-output c-ser-fim,
                        input-output c-not-ini,
                        input-output c-not-fim,
                        input-output c-dt-ini, 
                        input-output c-dt-fim, 
                        input-output c-uf-ini, 
                        input-output c-uf-fim,
                        input-output c-nom-ini,
                        input-output c-nom-fim,
                        output l-ok).

    current-window:sensitive = yes.

    if l-ok then do:

        run pi-rec-selec in h_esft100-b01 (input c-est-ini,
                                           input c-est-fim,
                                           input c-ser-ini,
                                           input c-ser-fim,
                                           input c-not-ini,
                                           input c-not-fim,
                                           input c-dt-ini, 
                                           input c-dt-fim, 
                                           input c-uf-ini, 
                                           input c-uf-fim,
                                           input c-nom-ini,
                                           input c-nom-fim).

        /*run pi-rec-selec in h_esft100-b02 (input c-est-ini,
                                           input c-est-fim,
                                           input c-ser-ini,
                                           input c-ser-fim,
                                           input c-not-ini,
                                           input c-not-fim,
                                           input c-dt-ini, 
                                           input c-dt-fim, 
                                           input c-uf-ini, 
                                           input c-uf-fim,
                                           input c-nom-ini,
                                           input c-nom-fim).

        run pi-rec-selec in h_esft100-b03 (input c-est-ini,
                                           input c-est-fim,
                                           input c-ser-ini,
                                           input c-ser-fim,
                                           input c-not-ini,
                                           input c-not-fim,
                                           input c-dt-ini, 
                                           input c-dt-fim, 
                                           input c-uf-ini, 
                                           input c-uf-fim,
                                           input c-nom-ini,
                                           input c-nom-fim).*/
    end.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-refresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-refresh w-livre
ON CHOOSE OF bt-refresh IN FRAME f-cad /* Button 1 */
DO:

    run pi-rec-selec in h_esft100-b01 (input c-est-ini,
                                       input c-est-fim,
                                       input c-ser-ini,
                                       input c-ser-fim,
                                       input c-not-ini,
                                       input c-not-fim,
                                       input c-dt-ini, 
                                       input c-dt-fim, 
                                       input c-uf-ini, 
                                       input c-uf-fim,
                                       input c-nom-ini,
                                       input c-nom-fim).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-livre
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-livre
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-livre
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-programa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-programa w-livre
ON MENU-DROP OF MENU mi-programa /* Nome-do-Programa */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-livre
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-livre
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-livre  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.13 , 95.57 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Outros UFs|R. de Janeiro|SÆo Paulo|Esp¡rito Sto' + ',
                     FOLDER-TAB-TYPE = 1':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 2.54 , 1.72 ) NO-ERROR.
       RUN set-size IN h_folder ( 22.75 , 109.57 ) NO-ERROR.

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             bt-filtro:HANDLE IN FRAME f-cad , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
             bt-refresh:HANDLE IN FRAME f-cad , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esp/esft100-b01.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_esft100-b01 ).
       RUN set-position IN h_esft100-b01 ( 3.96 , 5.00 ) NO-ERROR.
       /* Size in UIB:  ( 20.13 , 103.00 ) */

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_esft100-b01 ,
             h_folder , 'AFTER':U ).
    END. /* Page 1 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-livre  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-livre  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
  THEN DELETE WIDGET w-livre.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-livre  _DEFAULT-ENABLE
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
  ENABLE rt-button bt-filtro bt-refresh 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-livre 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  run pi-before-initialize.

  {include/win-size.i}

  {utp/ut9000.i "ESFT100" "12.01.00.001"}

  assign c-est-ini = ''
         c-est-fim = 'ZZZ'
         c-ser-ini = ''
         c-ser-fim = 'ZZZZZ'
         c-not-ini = ''
         c-not-fim = 'ZZZZZZZZZZZZZZZZ'
         c-dt-ini  = today
         c-dt-fim  = today
         c-uf-ini  = ''
         c-uf-fim  = 'ZZZZ'
         c-nom-ini = ''
         c-nom-fim = 'ZZZZZZZZZZZZ'.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  run select-page in this-procedure (input 1).

  run disable-folder-page in h_folder (input 2).
  run disable-folder-page in h_folder (input 3).
  run disable-folder-page in h_folder (input 4).

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-livre  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-livre 
PROCEDURE state-changed :
/*:T -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

