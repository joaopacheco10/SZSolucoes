&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME cw-conrelaciona
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS cw-conrelaciona 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESFT102 12.01.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
def new global shared var g-row-es-lote-gnre as rowid no-undo.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def buffer bf-es-lote-gnre for es-lote-gnre.

def var p-table as rowid.

def var h-env-lote as handle no-undo.
def var h-con-lote as handle no-undo.
def var h-server   as handle no-undo.

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
&Scoped-Define ENABLED-OBJECTS rt-button bt-del bt-envia bt-consulta ~
bt-integr-ap 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnConectar cw-conrelaciona 
FUNCTION fnConectar returns logical
  (p-cod-estabel as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnDesconectar cw-conrelaciona 
FUNCTION fnDesconectar returns character
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR cw-conrelaciona AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-arquivo 
       MENU-ITEM mi-primeiro    LABEL "&Primeiro"      ACCELERATOR "CTRL-HOME"
       MENU-ITEM mi-anterior    LABEL "An&terior"      ACCELERATOR "CTRL-CURSOR-LEFT"
       MENU-ITEM mi-proximo     LABEL "Pr&¢ximo"       ACCELERATOR "CTRL-CURSOR-RIGHT"
       MENU-ITEM mi-ultimo      LABEL "&Èltimo"        ACCELERATOR "CTRL-END"
       MENU-ITEM mi-va-para     LABEL "&V† para"       ACCELERATOR "CTRL-T"
       MENU-ITEM mi-pesquisa    LABEL "Pes&quisa"      ACCELERATOR "CTRL-F5"
       RULE
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU mi-ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-cadastro MENUBAR
       SUB-MENU  mi-arquivo     LABEL "&Arquivo"      
       SUB-MENU  mi-ajuda       LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_esft101-q01 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_esft102-b01 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_esft102-b02 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_esft102-v01 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navega AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-consulta 
     IMAGE-UP FILE "image/toolbar/im-f-ibt.bmp":U
     IMAGE-DOWN FILE "image/toolbar/ii-f-ibt.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-f-ibt.bmp":U
     LABEL "Consultar" 
     SIZE 4 BY 1.13 TOOLTIP "Consultar Lote no Portal GNRE".

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "image/toolbar/im-lixo.bmp":U
     IMAGE-DOWN FILE "image/toolbar/ii-lixo.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-lixo.bmp":U
     LABEL "Button 3" 
     SIZE 4 BY 1.13.

DEFINE BUTTON bt-envia 
     IMAGE-UP FILE "image/toolbar/im-autom.bmp":U
     IMAGE-DOWN FILE "image/toolbar/ii-autom.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-autom.bmp":U
     LABEL "Enviar" 
     SIZE 4 BY 1.13 TOOLTIP "Enviar Lote para o Portal GNRE".

DEFINE BUTTON bt-integr-ap 
     IMAGE-UP FILE "image/toolbar/im-pcust.bmp":U
     IMAGE-DOWN FILE "image/toolbar/ii-pcust.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-pcust.bmp":U
     LABEL "Button 1" 
     SIZE 4 BY 1.13 TOOLTIP "Integrar Lote com EMS5".

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-del AT ROW 1.17 COL 30.29 WIDGET-ID 8
     bt-envia AT ROW 1.17 COL 37.71 HELP
          "Enviar Lote para o Portal GNRE" WIDGET-ID 2
     bt-consulta AT ROW 1.17 COL 42.43 HELP
          "Consultar Lote no Portal GNRE" WIDGET-ID 4
     bt-integr-ap AT ROW 1.17 COL 47.14 HELP
          "Integrar Lote com EMS5" WIDGET-ID 6
     rt-button AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17.04 WIDGET-ID 100.


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
  CREATE WINDOW cw-conrelaciona ASSIGN
         HIDDEN             = YES
         TITLE              = "Consulta Relacionamentos"
         HEIGHT             = 17.04
         WIDTH              = 90
         MAX-HEIGHT         = 18.17
         MAX-WIDTH          = 90
         VIRTUAL-HEIGHT     = 18.17
         VIRTUAL-WIDTH      = 90
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-cadastro:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB cw-conrelaciona 
/* ************************* Included-Libraries *********************** */


{src/adm/method/containr.i}
{include/w-conrel.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW cw-conrelaciona
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(cw-conrelaciona)
THEN cw-conrelaciona:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME cw-conrelaciona
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cw-conrelaciona cw-conrelaciona
ON END-ERROR OF cw-conrelaciona /* Consulta Relacionamentos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cw-conrelaciona cw-conrelaciona
ON WINDOW-CLOSE OF cw-conrelaciona /* Consulta Relacionamentos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-consulta cw-conrelaciona
ON CHOOSE OF bt-consulta IN FRAME f-cad /* Consultar */
DO:

    def var l-xml-lote        as longchar no-undo.
    def var l-xml-lote-ret    as longchar no-undo.
    def var c-cod-estabel-aux as char     no-undo.

    def var r-row-lote as rowid no-undo.

    run pi-get-rowid in h_esft102-v01 (output r-row-lote).

    find es-lote-gnre no-lock where
         rowid(es-lote-gnre) = r-row-lote no-error.

    if avail es-lote-gnre then do:
        if es-lote-gnre.idi-situacao = 2 then do:
            if not valid-handle(h-con-lote) then
                run adapters/esaxsft002.p persistent set h-con-lote.
        
            assign l-xml-lote = ''.
        
            find first es-nota-fiscal-gnre no-lock where
                       es-nota-fiscal-gnre.cod-lote = es-lote-gnre.cod-lote no-error.
            if avail es-nota-fiscal-gnre then
                assign c-cod-estabel-aux = es-nota-fiscal-gnre.cod-estabel.
        
        
            run pi-gera-xml in h-con-lote (input es-lote-gnre.cod-lote,
                                           input c-cod-estabel-aux,
                                           output l-xml-lote).
        
            if l-xml-lote <> '' then do:
        
                fnConectar(c-cod-estabel-aux).
        
                if h-server:connected() then
                    run service/consultaLote.p on h-server (input es-lote-gnre.num-recibo, /*l-xml-lote,*/
                                                            input c-cod-estabel-aux,
                                                            output l-xml-lote-ret).
        
                fnDesconectar().
        
                if l-xml-lote-ret <> '' then
                    run pi-le-retorno in h-con-lote (input es-lote-gnre.cod-lote,
                                                     input l-xml-lote-ret).                

                find current es-lote-gnre no-lock no-error.
                if es-lote-gnre.idi-situacao = 3 then do:

                    run esp/esftapi100.p (rowid(es-lote-gnre)).  

                    run utp/ut-msgs.p (input 'show',
                                       input 15825,
                                       input 'Lote ' + es-lote-gnre.cod-lote + ' consultado com sucesso!').
                    
                end.
                else do:
                    run utp/ut-msgs.p (input 'show',
                                       input 17006,
                                       input 'Lote ' + es-lote-gnre.cod-lote + ' com divergància, verificar a aba Erros!').
                end.

                if valid-handle(h_esft102-v01) then
                    run local-display-fields in h_esft102-v01.

                if valid-handle(h_esft102-b01) then
                    run adm-open-query-cases in h_esft102-b01.

                if valid-handle(h_esft102-b02) then
                    run adm-open-query-cases in h_esft102-b02.
            end.
        end.
        else do:

            run utp/ut-msgs.p (input 'show',
                               input 17006,
                               input 'Lote n∆o disponivel para envio').
            return no-apply.

        end.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del cw-conrelaciona
ON CHOOSE OF bt-del IN FRAME f-cad /* Button 3 */
DO:

    def var l-xml-lote        as longchar no-undo.
    def var l-xml-lote-ret    as longchar no-undo.
    def var c-cod-estabel-aux as char     no-undo.

    def var r-row-lote as rowid no-undo.

    run pi-get-rowid in h_esft102-v01 (output r-row-lote).

    find es-lote-gnre no-lock where
         rowid(es-lote-gnre) = r-row-lote no-error.

    if avail es-lote-gnre then do:

        if es-lote-gnre.idi-situacao <> 1 then do:

            run utp/ut-msgs.p (input 'show',
                               input 17006,
                               input 'Lote GNRE n∆o pode ser eliminada!~~Situaá∆o do Lote n∆o permite alteraá‰es':U).

            return no-apply.

        end.
        else do:

            run utp/ut-msgs.p (input "show":U,
                               input  27100,
                               input 'Eliminar Lote GNRE?~~Confirma a eliminaá∆o do Lote GNRE selecionado').

            if return-value = "YES":U then do:

                for each es-nota-fiscal-gnre exclusive-lock where
                         es-nota-fiscal-gnre.cod-lote = es-lote-gnre.cod-lote:

                    delete es-nota-fiscal-gnre.

                end.

                for each es-lote-gnre-erro exclusive-lock where
                         es-lote-gnre-erro.cod-lote = es-lote-gnre.cod-lote:
                    
                    delete es-lote-gnre-erro.

                end.

                find current es-lote-gnre exclusive-lock no-error.
                if avail es-lote-gnre then
                    delete es-lote-gnre.
                
                if valid-handle(h_esft101-q01) then do:
                    
                    find first bf-es-lote-gnre no-lock no-error.
                    if avail bf-es-lote-gnre then
                        run pi-reposiciona-query in h_esft101-q01 (input rowid(bf-es-lote-gnre)).

                end.

                if valid-handle(h_esft102-v01) then
                    run local-display-fields in h_esft102-v01.

                if valid-handle(h_esft102-b01) then
                    run adm-open-query-cases in h_esft102-b01.

                if valid-handle(h_esft102-b02) then
                    run adm-open-query-cases in h_esft102-b02.
            end.
        end.
    end.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-envia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-envia cw-conrelaciona
ON CHOOSE OF bt-envia IN FRAME f-cad /* Enviar */
DO:

    def var l-xml-lote        as longchar no-undo.
    def var l-xml-lote-ret    as longchar no-undo.
    def var c-cod-estabel-aux as char     no-undo.

    def var r-row-lote as rowid no-undo.

    run pi-get-rowid in h_esft102-v01 (output r-row-lote).

    find es-lote-gnre no-lock where
         rowid(es-lote-gnre) = r-row-lote no-error.

    if avail es-lote-gnre then do:
        if es-lote-gnre.idi-situacao = 1 then do:
            if not valid-handle(h-env-lote) then
                run adapters/esaxsft001.p persistent set h-env-lote.    
        
            assign l-xml-lote = ''.
        
            find first es-nota-fiscal-gnre no-lock where
                       es-nota-fiscal-gnre.cod-lote = es-lote-gnre.cod-lote no-error.
            if avail es-nota-fiscal-gnre then
                assign c-cod-estabel-aux = es-nota-fiscal-gnre.cod-estabel.
        
            run pi-gera-xml in h-env-lote (input es-lote-gnre.cod-lote,
                                           output l-xml-lote).
        
            if l-xml-lote <> '' then do:
        
                fnConectar(c-cod-estabel-aux).
        
                if h-server:connected() then
                    run service/enviaLote.p on h-server (input l-xml-lote,
                                                         input c-cod-estabel-aux,
                                                         output l-xml-lote-ret).
        
                fnDesconectar().
        
                if l-xml-lote-ret <> '' then
                    run pi-le-retorno in h-env-lote (input l-xml-lote-ret,
                                                     input es-lote-gnre.cod-lote).

                if valid-handle(h_esft102-v01) then
                    run local-display-fields in h_esft102-v01.

                if valid-handle(h_esft102-b01) then
                    run adm-open-query-cases in h_esft102-b01.

                if valid-handle(h_esft102-b02) then
                    run adm-open-query-cases in h_esft102-b02.

                find current es-lote-gnre no-lock no-error.
                if es-lote-gnre.idi-situacao = 2 then do:

                    run utp/ut-msgs.p (input 'show',
                                       input 15825,
                                       input 'Lote ' + es-lote-gnre.cod-lote + ' enviado com sucesso!').
                    
                end.
                else do:
                    run utp/ut-msgs.p (input 'show',
                                       input 17006,
                                       input 'Lote ' + es-lote-gnre.cod-lote + ' com divergància, verificar a aba Erros!').
                end.
            end.
        end.
        else do:

            run utp/ut-msgs.p (input 'show',
                               input 17006,
                               input 'Lote n∆o disponivel para envio').
            return no-apply.
        end.
    end.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-integr-ap
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-integr-ap cw-conrelaciona
ON CHOOSE OF bt-integr-ap IN FRAME f-cad /* Button 1 */
DO:

    def var r-row-lote as rowid no-undo.

    run pi-get-rowid in h_esft102-v01 (output r-row-lote).

    find es-lote-gnre no-lock where
         rowid(es-lote-gnre) = r-row-lote no-error.

    if avail es-lote-gnre            and
       es-lote-gnre.idi-situacao = 3 then
        run esp/esftapi100.p (rowid(es-lote-gnre)).  


    if valid-handle(h_esft102-v01) then
        run local-display-fields in h_esft102-v01.

    if valid-handle(h_esft102-b01) then
        run adm-open-query-cases in h_esft102-b01.

    if valid-handle(h_esft102-b02) then
        run adm-open-query-cases in h_esft102-b02.

    find current es-lote-gnre no-lock no-error.
    if es-lote-gnre.idi-situacao = 3 then do:

        run utp/ut-msgs.p (input 'show',
                           input 15825,
                           input 'Lote ' + es-lote-gnre.cod-lote + ' Integrado com sucesso!').
        
    end.
    else do:
        run utp/ut-msgs.p (input 'show',
                           input 17006,
                           input 'Lote ' + es-lote-gnre.cod-lote + ' com divergància, verificar a aba Erros!').
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-anterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-anterior cw-conrelaciona
ON CHOOSE OF MENU-ITEM mi-anterior /* Anterior */
DO:
  RUN pi-anterior IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-arquivo cw-conrelaciona
ON MENU-DROP OF MENU mi-arquivo /* Arquivo */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas cw-conrelaciona
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo cw-conrelaciona
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir cw-conrelaciona
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-pesquisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-pesquisa cw-conrelaciona
ON CHOOSE OF MENU-ITEM mi-pesquisa /* Pesquisa */
DO:
  RUN pi-pesquisa IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-primeiro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-primeiro cw-conrelaciona
ON CHOOSE OF MENU-ITEM mi-primeiro /* Primeiro */
DO:
  RUN pi-primeiro IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-proximo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-proximo cw-conrelaciona
ON CHOOSE OF MENU-ITEM mi-proximo /* Pr¢ximo */
DO:
  RUN pi-proximo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair cw-conrelaciona
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre cw-conrelaciona
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-ultimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-ultimo cw-conrelaciona
ON CHOOSE OF MENU-ITEM mi-ultimo /* Èltimo */
DO:
  RUN pi-ultimo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-va-para
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-va-para cw-conrelaciona
ON CHOOSE OF MENU-ITEM mi-va-para /* V† para */
DO:
  RUN pi-vapara IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK cw-conrelaciona 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects cw-conrelaciona  _ADM-CREATE-OBJECTS
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
             INPUT  'panel/p-navega.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navega ).
       RUN set-position IN h_p-navega ( 1.17 , 1.57 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 24.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.17 , 74.72 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esp/esft102-v01.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_esft102-v01 ).
       RUN set-position IN h_esft102-v01 ( 2.75 , 1.72 ) NO-ERROR.
       /* Size in UIB:  ( 4.75 , 88.57 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Notas Fiscais|Erros' + ',
                     FOLDER-TAB-TYPE = 1':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 7.75 , 1.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 10.25 , 90.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esp/esft102-q01.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'ProgPesquisa = esp/esft102-z01.w,
                     ProgVaPara = esp/esft102-g01.w,
                     ProgIncMod = ,
                     Implantar = no':U ,
             OUTPUT h_esft101-q01 ).
       RUN set-position IN h_esft101-q01 ( 1.00 , 58.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.63 , 7.72 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Links to SmartViewer h_esft102-v01. */
       RUN add-link IN adm-broker-hdl ( h_esft101-q01 , 'Record':U , h_esft102-v01 ).

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Links to SmartQuery h_esft101-q01. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , h_esft101-q01 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'Navigation':U , h_esft101-q01 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'State':U , h_esft101-q01 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navega ,
             bt-del:HANDLE IN FRAME f-cad , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             bt-integr-ap:HANDLE IN FRAME f-cad , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_esft102-v01 ,
             h_p-exihel , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
             h_esft102-v01 , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esp/esft102-b01.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_esft102-b01 ).
       RUN set-position IN h_esft102-b01 ( 9.88 , 8.00 ) NO-ERROR.
       /* Size in UIB:  ( 7.50 , 75.00 ) */

       /* Links to SmartBrowser h_esft102-b01. */
       RUN add-link IN adm-broker-hdl ( h_esft101-q01 , 'Record':U , h_esft102-b01 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_esft102-b01 ,
             h_folder , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esp/esft102-b02.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_esft102-b02 ).
       RUN set-position IN h_esft102-b02 ( 9.63 , 8.00 ) NO-ERROR.
       /* Size in UIB:  ( 7.50 , 75.00 ) */

       /* Links to SmartBrowser h_esft102-b02. */
       RUN add-link IN adm-broker-hdl ( h_esft101-q01 , 'Record':U , h_esft102-b02 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_esft102-b02 ,
             h_folder , 'AFTER':U ).
    END. /* Page 2 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available cw-conrelaciona  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI cw-conrelaciona  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(cw-conrelaciona)
  THEN DELETE WIDGET cw-conrelaciona.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI cw-conrelaciona  _DEFAULT-ENABLE
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
  ENABLE rt-button bt-del bt-envia bt-consulta bt-integr-ap 
      WITH FRAME f-cad IN WINDOW cw-conrelaciona.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW cw-conrelaciona.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy cw-conrelaciona 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit cw-conrelaciona 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize cw-conrelaciona 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}
 
  run pi-before-initialize.

  {utp/ut9000.i "ESFT102" "12.01.00.001"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  if  g-row-es-lote-gnre <> ? then
      run pi-reposiciona-query in h_esft101-q01 (input g-row-es-lote-gnre).

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records cw-conrelaciona  _ADM-SEND-RECORDS
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed cw-conrelaciona 
PROCEDURE state-changed :
/* -----------------------------------------------------------
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnConectar cw-conrelaciona 
FUNCTION fnConectar returns logical
  (p-cod-estabel as char) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    fnDesconectar().

    find es-param-gnre no-lock where
         es-param-gnre.cod-estabel = p-cod-estabel no-error.
    if avail es-param-gnre then do:   
        create server h-server.
        h-server:connect(es-param-gnre.des-appserver).
    end.

    return h-server:connected().   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnDesconectar cw-conrelaciona 
FUNCTION fnDesconectar returns character
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    if valid-handle(h-server) and
       h-server:connected()   then
        h-server:disconnect().

    return "".   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

