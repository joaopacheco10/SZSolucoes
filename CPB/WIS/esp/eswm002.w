&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgcpb            PROGRESS
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

def buffer prog_dtsul   for emsfnd.prog_dtsul.
def buffer usuar_mestre for emsfnd.usuar_mestre.
def buffer procedimento for emsfnd.procedimento.
def buffer modul_dtsul for emsfnd.modul_dtsul.
{include/i-prgvrs.i ESWM002 2.11.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def var i-hra-ini as integer no-undo.
def var i-hra-fim as integer no-undo.

def var c-des-msg as char format "x(40)" no-undo.
def var c-status  as char no-undo.
def var c-pedido  as char no-undo.
def var c-codcli  as int  no-undo.
def var c-nome    as char no-undo.

def var c-dir    as char no-undo.
def var c-dir-in as char no-undo.
def var dt-geracao-ini as datetime format "99/99/9999 hh:mm:ss" no-undo.
def var dt-geracao-fin as datetime format "99/99/9999 hh:mm:ss" no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-erros

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES esp-erro-wms esp-fila-wms

/* Definitions for BROWSE br-erros                                      */
&Scoped-define FIELDS-IN-QUERY-br-erros esp-erro-wms.data-hora-erro ~
esp-erro-wms.mensagem 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-erros 
&Scoped-define QUERY-STRING-br-erros FOR EACH esp-erro-wms ~
      WHERE esp-erro-wms.id = esp-fila-wms.id NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-erros OPEN QUERY br-erros FOR EACH esp-erro-wms ~
      WHERE esp-erro-wms.id = esp-fila-wms.id NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-erros esp-erro-wms
&Scoped-define FIRST-TABLE-IN-QUERY-br-erros esp-erro-wms


/* Definitions for BROWSE br-msgs                                       */
&Scoped-define FIELDS-IN-QUERY-br-msgs esp-fila-wms.cod-trans fnc-status() @ c-status /* c-status */ esp-fila-wms.data-hora-criacao esp-fila-wms.data-hora-integracao esp-fila-wms.chave fnc-pedido() @ c-pedido fnc-codcli() @ c-codcli fnc-nome() @ c-nome esp-fila-wms.id   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-msgs   
&Scoped-define SELF-NAME br-msgs
&Scoped-define QUERY-STRING-br-msgs FOR EACH esp-fila-wms no-lock                             WHERE (esp-fila-wms.cod-trans = cb-mensagem OR cb-mensagem = "Todas")                               AND esp-fila-wms.data-hora-criacao >= dt-geracao-ini                               AND esp-fila-wms.data-hora-criacao <= dt-geracao-fin                             BY esp-fila-wms.id              INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-msgs OPEN QUERY {&SELF-NAME} FOR EACH esp-fila-wms no-lock                             WHERE (esp-fila-wms.cod-trans = cb-mensagem OR cb-mensagem = "Todas")                               AND esp-fila-wms.data-hora-criacao >= dt-geracao-ini                               AND esp-fila-wms.data-hora-criacao <= dt-geracao-fin                             BY esp-fila-wms.id              INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-msgs esp-fila-wms
&Scoped-define FIRST-TABLE-IN-QUERY-br-msgs esp-fila-wms


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-erros}~
    ~{&OPEN-QUERY-br-msgs}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-erros bt-next bt-prev c-pesquisa ~
bt-atualizar cb-mensagem cb-Status dt-Inicial dt-final hr-Inicial hr-final ~
rt-button RECT-1 RECT-10 IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 RECT-11 br-msgs 
&Scoped-Define DISPLAYED-OBJECTS c-pesquisa cb-mensagem cb-Status ~
dt-Inicial dt-final hr-Inicial hr-final 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnc-codcli w-livre 
FUNCTION fnc-codcli RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnc-nome w-livre 
FUNCTION fnc-nome RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnc-pedido w-livre 
FUNCTION fnc-pedido RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnc-status w-livre 
FUNCTION fnc-status RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn_desc_msg w-livre 
FUNCTION fn_desc_msg RETURNS CHARACTER
  ( input p-cod-trans as int  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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
       SUB-MENU  mi-programa    LABEL "&Arquivo"      
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-atualizar AUTO-GO 
     IMAGE-UP FILE "image/im-relo.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-relo.bmp":U
     LABEL "Atualizar" 
     SIZE 4 BY 1.25 TOOLTIP "Atualizar"
     FONT 4.

DEFINE BUTTON bt-next AUTO-GO 
     IMAGE-UP FILE "image/toolbar/im-abx1.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-relo.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Pr¢ximo" 
     SIZE 5 BY 1.25 TOOLTIP "Pr¢ximo"
     FONT 4.

DEFINE BUTTON bt-prev AUTO-GO 
     IMAGE-UP FILE "image/toolbar/im-acm1.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-relo.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Anterior" 
     SIZE 5 BY 1.25 TOOLTIP "Anterior"
     FONT 4.

DEFINE BUTTON bt-reproc AUTO-GO 
     IMAGE-UP FILE "image/toolbar/im-reini.bmp":U
     IMAGE-DOWN FILE "image/toolbar/ii-reini.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-reini.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 4 BY 1.25 TOOLTIP "Reenviar WIS".

DEFINE VARIABLE cb-mensagem AS CHARACTER FORMAT "X(256)":U INITIAL "Todas" 
     LABEL "Mensagem" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "Todas","Item","Emitente","Transportadora","NFEntrada","NFRecFisico","Embarque","Movimenta‡Æo","Requisi‡Æo" 
     DROP-DOWN-LIST
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE cb-Status AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Status" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Todas",0,
                     "NÆo Processada",1,
                     "Processada",2,
                     "Erro",3
     DROP-DOWN-LIST
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE c-pesquisa AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY .88 NO-UNDO.

DEFINE VARIABLE dt-final AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE dt-Inicial AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Inicial" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE hr-final AS CHARACTER FORMAT "99:99:99":U INITIAL "235959" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE hr-Inicial AS CHARACTER FORMAT "99:99:99":U INITIAL "000000" 
     LABEL "Hora Inicial" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FONT 1 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
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

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 2.75.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 45 BY 2.71.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27.29 BY 2.71.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 145 BY 1.46
     BGCOLOR 17 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-erros FOR 
      esp-erro-wms SCROLLING.

DEFINE QUERY br-msgs FOR 
      esp-fila-wms SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-erros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-erros w-livre _STRUCTURED
  QUERY br-erros NO-LOCK DISPLAY
      esp-erro-wms.data-hora-erro FORMAT "99/99/9999 HH:MM:SS":U
      esp-erro-wms.mensagem FORMAT "x(1000)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 146 BY 4.25
         TITLE "Erros".

DEFINE BROWSE br-msgs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-msgs w-livre _FREEFORM
  QUERY br-msgs NO-LOCK DISPLAY
      esp-fila-wms.cod-trans width 15 label "Mensagem"
      fnc-status() @ c-status COLUMN-LABEL "Status" FORMAT "X(20)":U
/*       c-status column-label "St" width 50 */
      esp-fila-wms.data-hora-criacao FORMAT "99/99/9999 hh:mm":U width 16
      esp-fila-wms.data-hora-integracao FORMAT "99/99/9999 hh:mm":U width 16
      esp-fila-wms.chave FORMAT "x(50)":U WIDTH 25
      fnc-pedido() @ c-pedido COLUMN-LABEL "Pedido" FORMAT "X(20)":U
      fnc-codcli() @ c-codcli column-label "Cliente" format ">>>>>>>>9":U
      fnc-nome() @ c-nome column-label "Nome Abrev" format "x(12)":U width 20
      esp-fila-wms.id FORMAT ">>>>>>>>>9":U WIDTH 6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 146 BY 17.5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     br-erros AT ROW 23.25 COL 1 WIDGET-ID 400
     bt-reproc AT ROW 3.5 COL 65.57 WIDGET-ID 68
     bt-next AT ROW 4.17 COL 135 HELP
          "Pr¢ximo" WIDGET-ID 66
     bt-prev AT ROW 4.17 COL 129 HELP
          "Anterior" WIDGET-ID 64
     c-pesquisa AT ROW 3.25 COL 119 COLON-ALIGNED NO-LABEL WIDGET-ID 60
     bt-atualizar AT ROW 1.08 COL 3 HELP
          "Atualizar" WIDGET-ID 52
     cb-mensagem AT ROW 3 COL 11.14 COLON-ALIGNED WIDGET-ID 6
     cb-Status AT ROW 4.08 COL 11 COLON-ALIGNED WIDGET-ID 4
     dt-Inicial AT ROW 3.25 COL 83 COLON-ALIGNED WIDGET-ID 22
     dt-final AT ROW 3.25 COL 104 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     hr-Inicial AT ROW 4.25 COL 83 COLON-ALIGNED WIDGET-ID 26
     hr-final AT ROW 4.25 COL 104 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     br-msgs AT ROW 5.75 COL 1 WIDGET-ID 300
     "Sele‡Æo:" VIEW-AS TEXT
          SIZE 7.14 BY .67 AT ROW 2.46 COL 2.86 WIDGET-ID 8
     "Gera‡Æo:" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 2.5 COL 75 WIDGET-ID 30
     "Pesquisa" VIEW-AS TEXT
          SIZE 9 BY .67 AT ROW 2.5 COL 121 WIDGET-ID 62
     rt-button AT ROW 1 COL 1.57
     RECT-1 AT ROW 2.75 COL 2 WIDGET-ID 2
     RECT-10 AT ROW 2.79 COL 74 WIDGET-ID 28
     IMAGE-1 AT ROW 3.25 COL 98 WIDGET-ID 32
     IMAGE-2 AT ROW 3.25 COL 102 WIDGET-ID 34
     IMAGE-3 AT ROW 4.25 COL 98 WIDGET-ID 36
     IMAGE-4 AT ROW 4.25 COL 102 WIDGET-ID 38
     RECT-11 AT ROW 2.79 COL 119.72 WIDGET-ID 58
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 146.43 BY 26.67 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-livre
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-livre ASSIGN
         HIDDEN             = YES
         TITLE              = "Template Livre <Insira complemento>"
         HEIGHT             = 26.67
         WIDTH              = 146.43
         MAX-HEIGHT         = 29.46
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 29.46
         VIRTUAL-WIDTH      = 182.86
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
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB br-erros 1 f-cad */
/* BROWSE-TAB br-msgs RECT-11 f-cad */
/* SETTINGS FOR BUTTON bt-reproc IN FRAME f-cad
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-erros
/* Query rebuild information for BROWSE br-erros
     _TblList          = "mgcpb.esp-erro-wms"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "mgcpb.esp-erro-wms.id = esp-fila-wms.id"
     _FldNameList[1]   = mgcpb.esp-erro-wms.data-hora-erro
     _FldNameList[2]   > mgcpb.esp-erro-wms.mensagem
"esp-erro-wms.mensagem" ? "x(1000)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE br-erros */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-msgs
/* Query rebuild information for BROWSE br-msgs
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH esp-fila-wms no-lock
                            WHERE (esp-fila-wms.cod-trans = cb-mensagem OR cb-mensagem = "Todas")
                              AND esp-fila-wms.data-hora-criacao >= dt-geracao-ini
                              AND esp-fila-wms.data-hora-criacao <= dt-geracao-fin
                            BY esp-fila-wms.id              INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-msgs */
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


&Scoped-define BROWSE-NAME br-erros
&Scoped-define SELF-NAME br-erros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-erros w-livre
ON MOUSE-SELECT-DBLCLICK OF br-erros IN FRAME f-cad /* Erros */
DO:
        message esp-erro-wms.mensagem view-as alert-box.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-msgs
&Scoped-define SELF-NAME br-msgs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-msgs w-livre
ON MOUSE-SELECT-CLICK OF br-msgs IN FRAME f-cad
DO:

    if avail esp-fila-wms                     and
       esp-fila-wms.data-hora-integracao <> ? then do:
    
        assign bt-reproc:sensitive in frame {&frame-name} = true.
    
    end.
    else
        assign bt-reproc:sensitive in frame {&frame-name} = false.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-msgs w-livre
ON ROW-DISPLAY OF br-msgs IN FRAME f-cad
DO:
/*
    if esp-fila-wms.ind-status = 1 then
        assign c-status:bgcolor in browse br-msgs = 14.

    if esp-fila-wms.ind-status = 2 then
        assign c-status:bgcolor in browse br-msgs = 10.

    if esp-fila-wms.ind-status = 3 then
        assign c-status:bgcolor in browse br-msgs = 12.

    if esp-fila-wms.cod-trans > 97 then
        assign c-status:bgcolor in browse br-msgs = 15.
        */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-msgs w-livre
ON VALUE-CHANGED OF br-msgs IN FRAME f-cad
DO:
    br-msgs:fetch-selected-row(1).

       
   {&OPEN-QUERY-br-erros}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-atualizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-atualizar w-livre
ON CHOOSE OF bt-atualizar IN FRAME f-cad /* Atualizar */
DO:
  assign input frame {&frame-name} cb-mensagem cb-status dt-inicial dt-final hr-inicial hr-final.
  
  assign i-hra-ini = int(substr(hr-inicial,1,2)) * 3600 +
                     int(substr(hr-inicial,3,2)) * 60   +
                     int(substr(hr-inicial,5,2))
         i-hra-fim = int(substr(hr-final,1,2)) * 3600 +
                     int(substr(hr-final,3,2)) * 60   +
                     int(substr(hr-final,5,2))
         dt-geracao-ini = datetime(string(dt-inicial,"99/99/9999") + " " + string(hr-inicial,"99:99:99"))
         dt-geracao-fin = datetime(string(dt-final,"99/99/9999") + " " + string(hr-final,"99:99:99")).
                     
        
  {&open-query-br-msgs}

  if br-msgs:num-iterations > 0 then do:
      apply 'mouse-select-click' to br-msgs.
      apply 'value-changed' to br-msgs.
  end.      
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-next w-livre
ON CHOOSE OF bt-next IN FRAME f-cad /* Pr¢ximo */
DO:
  run pi-find (2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-prev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-prev w-livre
ON CHOOSE OF bt-prev IN FRAME f-cad /* Anterior */
DO:
  run pi-find(1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-reproc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-reproc w-livre
ON CHOOSE OF bt-reproc IN FRAME f-cad /* Button 1 */
DO:

    if avail esp-fila-wms then do:
        if esp-fila-wms.data-hora-integracao <> ? then do:
        
            run utp/ut-msgs.p (input "show",
                               input 27100,
                               input "Deseja reenviar o registro " + string(esp-fila-wms.id) + " ?~~" + 
                                     "O registro com a chave " + esp-fila-wms.chave + " ser  enviado novamente para Interface" + CHR(10) + 
                                     "Deseja continuar?").

            if return-value = "yes" then do:
                find current esp-fila-wms exclusive-lock no-error.
                
                assign esp-fila-wms.data-hora-integracao = ?.
                
                find current esp-fila-wms no-lock no-error.
                
                apply 'choose' to bt-atualizar.
            end.        
        end.        
    end.
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
ON MENU-DROP OF MENU mi-programa /* Arquivo */
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


&Scoped-define BROWSE-NAME br-erros
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

ON FIND OF esp-fila-wms DO:
    DEFINE VARIABLE i-cont AS INTEGER INITIAL 1 NO-UNDO.
    DEFINE VARIABLE l-eswm002 AS LOGICAL NO-UNDO.
    
    DO WHILE PROGRAM-NAME(i-cont) <> ?:        
        IF PROGRAM-NAME(i-cont) MATCHES("*eswm002*") THEN DO:
            ASSIGN l-eswm002 =YES.
            LEAVE.
        END.
        ASSIGN i-cont = i-cont + 1.
     END.
    
    IF l-eswm002 THEN DO with frame {&frame-name}:
        
        IF cb-status:input-value = 1 AND esp-fila-wms.data-hora-integracao <> ? THEN DO:
            RETURN ERROR.
        END.
        ELSE 
            IF cb-status:input-value = 2 AND esp-fila-wms.data-hora-integracao = ? THEN DO:
                RETURN ERROR.
            END.
            ELSE 
                IF cb-status:input-value = 3
                AND not (esp-fila-wms.data-hora-integracao = ?
                AND CAN-FIND (FIRST esp-erro-wms
                              WHERE esp-erro-wms.id = esp-fila-wms.id)) THEN DO:
                    RETURN ERROR.
                END.
                
    END.
    RETURN .
    
 END.
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
       RUN set-position IN h_p-exihel ( 1.08 , 130.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             c-pesquisa:HANDLE IN FRAME f-cad , 'AFTER':U ).
    END. /* Page 0 */

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
  DISPLAY c-pesquisa cb-mensagem cb-Status dt-Inicial dt-final hr-Inicial 
          hr-final 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE br-erros bt-next bt-prev c-pesquisa bt-atualizar cb-mensagem cb-Status 
         dt-Inicial dt-final hr-Inicial hr-final rt-button RECT-1 RECT-10 
         IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 RECT-11 br-msgs 
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

  {utp/ut9000.i "ESWM002" "2.06.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  assign dt-inicial:screen-value in frame {&FRAME-NAME} = string(today)
         dt-final:screen-value in frame {&FRAME-NAME} = string(today).
  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-find w-livre 
PROCEDURE pi-find :
define input param p-direction as int no-undo.

def var h-query  as handle no-undo.
def var h-buffer as handle no-undo.

br-msgs:fetch-selected-row(1) in frame {&FRAME-NAME} no-error .

assign h-query = br-msgs:query in frame {&FRAME-NAME}
       h-buffer = h-query:get-buffer-handle(1)
       input frame {&FRAME-NAME} c-pesquisa.

if p-direction = 2 then do: /*next*/
    do while h-query:get-next(no-lock):
        if index(h-buffer:buffer-field("chave"):buffer-value,c-pesquisa) > 0  then do:
            h-query:reposition-to-rowid(h-buffer:rowid).
            leave.
        end.
    end.
end.
if p-direction = 1 then do: /*prev*/
    do while h-query:get-prev(no-lock):
        if index(h-buffer:buffer-field("chave"):buffer-value,c-pesquisa) > 0  then do:
            h-query:reposition-to-rowid(h-buffer:rowid).
            leave.
        end.
    end.
end.


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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "esp-fila-wms"}
  {src/adm/template/snd-list.i "esp-erro-wms"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnc-codcli w-livre 
FUNCTION fnc-codcli RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    def var c-cod-emitente like emitente.cod-emitente no-undo.
    
    assign c-cod-emitente = 0.
    
    if esp-fila-wms.cod-trans = "Embarque" then do:        
        for first pre-fatur no-lock where
                  pre-fatur.cdd-embarq = int(entry(1,esp-fila-wms.chave,"|")) and
                  pre-fatur.nr-resumo  = int(entry(2,esp-fila-wms.chave,"|")): end.
        
        if avail pre-fatur then do:                
            find emitente no-lock where
                 emitente.nome-abrev = pre-fatur.nome-abrev no-error.
            if avail emitente then
                assign c-cod-emitente = emitente.cod-emitente.
        end.
    end.                                           
        
    return c-cod-emitente.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnc-nome w-livre 
FUNCTION fnc-nome RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    def var c-nome-abrev like emitente.nome-abrev no-undo.
    
    assign c-nome-abrev = "".
    
    if esp-fila-wms.cod-trans = "Embarque" then do:        
        for first pre-fatur no-lock where
                  pre-fatur.cdd-embarq = int(entry(1,esp-fila-wms.chave,"|")) and
                  pre-fatur.nr-resumo  = int(entry(2,esp-fila-wms.chave,"|")): end.
        
        if avail pre-fatur then
            assign c-nome-abrev = pre-fatur.nome-abrev.

    end.
        
    return c-nome-abrev.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnc-pedido w-livre 
FUNCTION fnc-pedido RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    def var c-nr-pedcli like ped-venda.nr-pedcli no-undo.
    
    assign c-nr-pedcli = "".
    
    if esp-fila-wms.cod-trans = "Embarque" then do:        
        for first pre-fatur no-lock where
                  pre-fatur.cdd-embarq = int(entry(1,esp-fila-wms.chave,"|")) and
                  pre-fatur.nr-resumo  = int(entry(2,esp-fila-wms.chave,"|")): end.
        
        if avail pre-fatur then                  
            assign c-nr-pedcli = pre-fatur.nr-pedcli.                      
    
    end.
        
    return c-nr-pedcli.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnc-status w-livre 
FUNCTION fnc-status RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    def var c-msg-funcao as char no-undo.

    IF esp-fila-wms.data-hora-integracao = ?
    AND NOT CAN-FIND (FIRST esp-erro-wms
                   WHERE esp-erro-wms.id = esp-fila-wms.id) THEN DO:
            ASSIGN c-msg-funcao = "NÆo Processada".
        END.
        ELSE 
            IF esp-fila-wms.data-hora-integracao <> ? THEN DO:
                ASSIGN c-msg-funcao = "Processada".
            END.
            ELSE
                IF (cb-status:input-value in frame {&frame-name} = 3 
                OR cb-status:input-value in frame {&frame-name} = 0 )
                AND esp-fila-wms.data-hora-integracao = ? 
                AND CAN-FIND (FIRST esp-erro-wms
                        WHERE esp-erro-wms.id = esp-fila-wms.id) THEN DO:
                    ASSIGN c-msg-funcao = "Erro".
                END.
    
    RETURN c-msg-funcao.
          
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn_desc_msg w-livre 
FUNCTION fn_desc_msg RETURNS CHARACTER
  ( input p-cod-trans as int  ) :

  

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

