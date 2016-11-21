&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/*:T *******************************************************************************
** Copyright DATasUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATasUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESFT105 12.01.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local var Definitions ---                                       */

{dibo/bodi317sd.i1}

def temp-table tt-wt-fat-ser-lote undo like wt-fat-ser-lote.

def temp-table tt-itens-devol-lote no-undo like tt-itens-devol
    field lote        as character format "x(10)"
    field cod-depos   as char
    field cod-estabel as character format "x(03)".

def temp-table tt-notas-origem-devol no-undo
    field nro-docto  like item-doc-est.nro-docto 
    field dt-emissao like docum-est.dt-emissao.

def temp-table tt-item-lote no-undo
    field it-codigo    like item.it-codigo
    field lote         like saldo-estoq.lote
    field cod-depos    like saldo-estoq.cod-depos
    field cod-estabel  like estabelec.cod-estabel
    field quantidade   as decimal
    field quant-saldo  as decimal
    field serie-docto  like item-doc-est.serie-docto 
    field cod-emitente like item-doc-est.cod-emitente
    field nro-docto    like item-doc-est.nro-docto   
    field nat-operacao like item-doc-est.nat-operacao
    field sequencia    like item-doc-est.sequencia
    index ch-it-lote is primary unique it-codigo lote
    index ch-docum  serie-docto nro-docto nat-operacao sequencia it-codigo
    index ch-saldo quant-saldo.

def temp-table RowErrors no-undo 
    field ErrorSequence    as integer 
    field ErrorNumber      as integer 
    field ErrorDescription as character 
    field ErrorParameters  as character 
    field ErrorType        as character 
    field ErrorHelp        as character 
    field ErrorSubType     as character.

def temp-table tt-notas-geradas no-undo 
    field rw-nota-fiscal as   rowid 
    field nr-nota        like nota-fiscal.nr-nota-fis
    field seq-wt-docto   like wt-docto.seq-wt-docto.


def temp-table tt-saldo no-undo like saldo-estoq
    field lg-selec  as log format '*/'
    field desc-item like item.desc-item
    field qt-devol  like saldo-estoq.qtidade-atu
    field qt-saldo  like saldo-estoq.qtidade-atu.

def temp-table tt-devol no-undo like saldo-estoq
    field desc-item like item.desc-item
    field qt-saldo  like saldo-estoq.qtidade-atu
    field qt-devol  like saldo-estoq.qtidade-atu.

def var p-cod-estabel   like saldo-estoq.cod-estabel  no-undo.
def var p-it-cod-ini    like saldo-estoq.it-codigo    no-undo.
def var p-it-cod-fim    like saldo-estoq.it-codigo    no-undo.
def var p-cod-depos-ini like saldo-estoq.cod-depos    no-undo.
def var p-cod-depos-fim like saldo-estoq.cod-depos    no-undo.
def var p-lote-ini      like saldo-estoq.lote         no-undo.
def var p-lote-fim      like saldo-estoq.lote         no-undo.
def var p-dt-valid-ini  like saldo-estoq.dt-vali-lote no-undo.
def var p-dt-valid-fim  like saldo-estoq.dt-vali-lote no-undo.

def var c-des-inf-compl as char no-undo.

def var h-acomp              as handle                     no-undo.
def var c-nro-docto          as character                  no-undo.
def var c-dados-orig         as character format "X(2000)" no-undo.
def var h-bodi317            as handle                     no-undo.
def var h-bodi317ef          as handle                     no-undo.
def var h-bodi317pr          as handle                     no-undo.
def var h-bodi317sd          as handle                     no-undo.
def var h-bodi317im1bra      as handle                     no-undo.
def var h-bodi317va          as handle                     no-undo.
def var h-bodi317in          as handle                     no-undo.
def var i-seq-wt-docto       as integer                    no-undo.
def var i-seq-wt-it-docto    as integer                    no-undo.
def var l-proc-ok-aux        as logical                    no-undo.
def var c-ultimo-metodo-exec as character                  no-undo.
def var hShowMsg             as handle                     no-undo.
def var de-qtd-devol         as decimal                    no-undo.
def var de-saldo-item        as decimal                    no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-aloc-devol

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-devol tt-saldo

/* Definitions for BROWSE br-aloc-devol                                 */
&Scoped-define FIELDS-IN-QUERY-br-aloc-devol tt-devol.cod-estabel tt-devol.it-codigo tt-devol.desc-item tt-devol.cod-depos tt-devol.lote tt-devol.dt-vali-lote tt-devol.qt-devol   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-aloc-devol   
&Scoped-define SELF-NAME br-aloc-devol
&Scoped-define QUERY-STRING-br-aloc-devol FOR EACH tt-devol
&Scoped-define OPEN-QUERY-br-aloc-devol OPEN QUERY {&SELF-NAME} FOR EACH tt-devol.
&Scoped-define TABLES-IN-QUERY-br-aloc-devol tt-devol
&Scoped-define FIRST-TABLE-IN-QUERY-br-aloc-devol tt-devol


/* Definitions for BROWSE br-it-selec                                   */
&Scoped-define FIELDS-IN-QUERY-br-it-selec tt-saldo.lg-selec tt-saldo.cod-estabel tt-saldo.it-codigo tt-saldo.desc-item tt-saldo.cod-depos tt-saldo.lote tt-saldo.dt-vali-lote tt-saldo.qt-devol tt-saldo.qt-saldo tt-saldo.qt-aloc-ped   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-it-selec tt-saldo.qt-devol   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-it-selec tt-saldo
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-it-selec tt-saldo
&Scoped-define SELF-NAME br-it-selec
&Scoped-define QUERY-STRING-br-it-selec FOR EACH tt-saldo
&Scoped-define OPEN-QUERY-br-it-selec OPEN QUERY {&SELF-NAME} FOR EACH tt-saldo.
&Scoped-define TABLES-IN-QUERY-br-it-selec tt-saldo
&Scoped-define FIRST-TABLE-IN-QUERY-br-it-selec tt-saldo


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-aloc-devol}~
    ~{&OPEN-QUERY-br-it-selec}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-calc rt-button RECT-1 br-it-selec ~
bt-marca bt-todos bt-nenhum br-aloc-devol c-est-orig c-est-dest ~
c-cod-depos-dest c-cfop-devol bt-carrega bt-refresh bt-inf-comp bt-selec 
&Scoped-Define DISPLAYED-OBJECTS c-est-orig c-est-dest c-cod-depos-dest ~
c-cfop-devol 

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
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-calc 
     IMAGE-UP FILE "image/im-calc.bmp":U
     IMAGE-DOWN FILE "image/ii-calc.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-calc.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Calcular" 
     SIZE 7 BY 1.5 TOOLTIP "Gerar Devoluá∆o".

DEFINE BUTTON bt-carrega 
     IMAGE-UP FILE "image/im-carg.bmp":U
     IMAGE-DOWN FILE "image/im-carg2.bmp":U
     IMAGE-INSENSITIVE FILE "image/im-carg2.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 3" 
     SIZE 7 BY 1.5 TOOLTIP "Alocar Itens".

DEFINE BUTTON bt-inf-comp 
     LABEL "Inf. Compl." 
     SIZE 15 BY 1.13.

DEFINE BUTTON bt-marca 
     LABEL "Marcar" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bt-nenhum 
     LABEL "Nenhum" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bt-refresh 
     IMAGE-UP FILE "image/toolbar/im-ordrh.bmp":U
     IMAGE-DOWN FILE "image/toolbar/ii-ordrh.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-ordrh.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 2" 
     SIZE 4 BY 1.13.

DEFINE BUTTON bt-selec 
     IMAGE-UP FILE "image/toolbar/im-ran.bmp":U
     IMAGE-DOWN FILE "image/toolbar/ii-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-ran.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 4 BY 1.13.

DEFINE BUTTON bt-todos 
     LABEL "Todos" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE c-cfop-devol AS CHARACTER FORMAT "X(256)":U 
     LABEL "Natureza" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-depos-dest AS CHARACTER FORMAT "X(256)":U 
     LABEL "Dep¢sito Destino" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE c-est-dest AS CHARACTER FORMAT "X(5)":U 
     LABEL "Estab Destino" 
     VIEW-AS FILL-IN 
     SIZE 7.29 BY .88 NO-UNDO.

DEFINE VARIABLE c-est-orig AS CHARACTER FORMAT "X(5)":U 
     LABEL "Estab Origem" 
     VIEW-AS FILL-IN 
     SIZE 7.29 BY .88 NO-UNDO.

DEFINE VARIABLE d-peso-item AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Peso Bruto Total" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE d-qtd-item AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Qtd Itens" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE d-vl-item AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Vl Itens" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE d-vol-item AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Volumes Itens" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 24.43 BY 11.5.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 145 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-aloc-devol FOR 
      tt-devol SCROLLING.

DEFINE QUERY br-it-selec FOR 
      tt-saldo SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-aloc-devol
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-aloc-devol w-livre _FREEFORM
  QUERY br-aloc-devol DISPLAY
      tt-devol.cod-estabel
tt-devol.it-codigo
tt-devol.desc-item
tt-devol.cod-depos     format 'x(5)'
tt-devol.lote
tt-devol.dt-vali-lote
tt-devol.qt-devol     column-label 'Qtd Devoluá∆o':U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 119.43 BY 11.5
         TITLE "Alocaá‰es Devoluá∆o" FIT-LAST-COLUMN.

DEFINE BROWSE br-it-selec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-it-selec w-livre _FREEFORM
  QUERY br-it-selec DISPLAY
      tt-saldo.lg-selec       column-label '*':C
tt-saldo.cod-estabel
tt-saldo.it-codigo
tt-saldo.desc-item
tt-saldo.cod-depos      format 'x(5)'
tt-saldo.lote
tt-saldo.dt-vali-lote
tt-saldo.qt-devol       column-label 'Devoluá∆o':U
tt-saldo.qt-saldo       column-label 'Saldo'
tt-saldo.qt-aloc-ped
enable
tt-saldo.qt-devol
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 144.43 BY 12
         TITLE "Itens da Seleá∆o" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-calc AT ROW 26 COL 130.57 WIDGET-ID 36
     br-it-selec AT ROW 2.63 COL 1.57 WIDGET-ID 200
     bt-marca AT ROW 14.79 COL 1.43 WIDGET-ID 2
     bt-todos AT ROW 14.79 COL 17 WIDGET-ID 4
     bt-nenhum AT ROW 14.79 COL 32.57 WIDGET-ID 32
     d-vl-item AT ROW 15 COL 54.57 COLON-ALIGNED WIDGET-ID 12
     d-qtd-item AT ROW 15 COL 76.72 COLON-ALIGNED WIDGET-ID 6
     d-vol-item AT ROW 15 COL 102.86 COLON-ALIGNED WIDGET-ID 8
     d-peso-item AT ROW 15 COL 130 COLON-ALIGNED WIDGET-ID 10
     br-aloc-devol AT ROW 16.25 COL 1.57 WIDGET-ID 300
     c-est-orig AT ROW 19.38 COL 134.14 COLON-ALIGNED WIDGET-ID 14
     c-est-dest AT ROW 20.38 COL 134.14 COLON-ALIGNED WIDGET-ID 16
     c-cod-depos-dest AT ROW 21.38 COL 135.43 COLON-ALIGNED WIDGET-ID 24
     c-cfop-devol AT ROW 22.38 COL 133.43 COLON-ALIGNED WIDGET-ID 20
     bt-carrega AT ROW 16.5 COL 130.57 WIDGET-ID 34
     bt-refresh AT ROW 1.17 COL 7.72 WIDGET-ID 30
     bt-inf-comp AT ROW 24.5 COL 126.29 WIDGET-ID 38
     bt-selec AT ROW 1.17 COL 2.57 WIDGET-ID 28
     rt-button AT ROW 1 COL 1.29
     RECT-1 AT ROW 16.25 COL 121.57 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 145.72 BY 26.92 WIDGET-ID 100.


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
         HEIGHT             = 26.92
         WIDTH              = 145.72
         MAX-HEIGHT         = 26.92
         MAX-WIDTH          = 160.86
         VIRTUAL-HEIGHT     = 26.92
         VIRTUAL-WIDTH      = 160.86
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
/* BROWSE-TAB br-it-selec RECT-1 f-cad */
/* BROWSE-TAB br-aloc-devol d-peso-item f-cad */
/* SETTINGS FOR FILL-IN d-peso-item IN FRAME f-cad
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       d-peso-item:HIDDEN IN FRAME f-cad           = TRUE.

/* SETTINGS FOR FILL-IN d-qtd-item IN FRAME f-cad
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       d-qtd-item:HIDDEN IN FRAME f-cad           = TRUE.

/* SETTINGS FOR FILL-IN d-vl-item IN FRAME f-cad
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       d-vl-item:HIDDEN IN FRAME f-cad           = TRUE.

/* SETTINGS FOR FILL-IN d-vol-item IN FRAME f-cad
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       d-vol-item:HIDDEN IN FRAME f-cad           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-aloc-devol
/* Query rebuild information for BROWSE br-aloc-devol
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-devol.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-aloc-devol */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-it-selec
/* Query rebuild information for BROWSE br-it-selec
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-saldo.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-it-selec */
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


&Scoped-define BROWSE-NAME br-it-selec
&Scoped-define SELF-NAME br-it-selec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-it-selec w-livre
ON MOUSE-SELECT-DBLCLICK OF br-it-selec IN FRAME f-cad /* Itens da Seleá∆o */
DO:

    if avail tt-saldo then do:
        if tt-saldo.lg-selec then
            assign tt-saldo.lg-selec = no
                   tt-saldo.lg-selec:screen-value in browse br-it-selec = ''.
        else
            assign tt-saldo.lg-selec = yes
                   tt-saldo.lg-selec:screen-value in browse br-it-selec = '*'.
    end.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-calc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-calc w-livre
ON CHOOSE OF bt-calc IN FRAME f-cad /* Calcular */
do:

    if not can-find(estabelec where
                    estabelec.cod-estabel = input frame {&frame-name} c-est-orig) then do:
        run utp/ut-msgs.p (input 'show',
                           input 17006,
                           input 'Estabelecimento Origem Inv†lido!':U).
        apply 'entry' to c-est-orig in frame {&frame-name}.
        return no-apply.
    end.

    if not can-find(estabelec where
                    estabelec.cod-estabel = input frame {&frame-name} c-est-dest) then do:
        run utp/ut-msgs.p (input 'show',
                           input 17006,
                           input 'Estabelecimento Destino Inv†lido!':U).
        apply 'entry' to c-est-dest in frame {&frame-name}.
        return no-apply.
    end.

    if not can-find(natur-oper where
                    natur-oper.nat-operacao = input frame {&frame-name} c-cfop-devol) then do:
        run utp/ut-msgs.p (input 'show',
                           input 17006,
                           input 'Natureza Inv†lida!':U).
        apply 'entry' to c-cfop-devol in frame {&frame-name}.
        return no-apply.
    end.

    if not can-find(deposito where
                    deposito.cod-depos = input frame {&frame-name} c-cod-depos-dest) then do:
        run utp/ut-msgs.p (input 'show',
                           input 17006,
                           input 'Dep¢sito Destino Inv†lido!':U).
        apply 'entry' to c-cod-depos-dest in frame {&frame-name}.
        return no-apply.
    end.

    if can-find(first tt-devol where
                      tt-devol.qt-devol = 0) then do:
        run utp/ut-msgs.p (input 'show',
                           input 17006,
                           input 'Quantidade Devoluá∆o deve ser maior que 0 (zero)!':U).
        return no-apply.
    end.

    find estabelec no-lock where
         estabelec.cod-estabel = input frame {&frame-name} c-est-dest no-error.

    find emitente no-lock where
         emitente.cod-emitente = estabelec.cod-emitente no-error.

    empty temp-table tt-item-lote.
    empty temp-table tt-notas-geradas.
    
    for each tt-devol:
    
        find tt-item-lote where 
             tt-item-lote.it-codigo = tt-devol.it-codigo and   
             tt-item-lote.lote      = tt-devol.lote      no-error.
        if not avail tt-item-lote then do:
            create tt-item-lote.
            assign tt-item-lote.it-codigo    = tt-devol.it-codigo
                   tt-item-lote.cod-estabel  = input frame {&frame-name} c-est-orig
                   tt-item-lote.cod-emitente = emitente.cod-emitente
                   tt-item-lote.quantidade   = tt-devol.qt-devol
                   tt-item-lote.quant-saldo  = tt-devol.qt-devol
                   tt-item-lote.lote         = tt-devol.lote
                   tt-item-lote.cod-depos    = tt-devol.cod-depos.
        end.
        else
            assign tt-item-lote.quantidade  = tt-item-lote.quantidade + tt-devol.qt-devol
                   tt-item-lote.quant-saldo = tt-item-lote.quant-saldo + tt-devol.qt-devol.
    end.
    
    run pi-gera-devolucao.
    
    find first tt-notas-geradas no-lock no-error.
    if avail tt-notas-geradas then do:
        find nota-fiscal exclusive-lock where 
             rowid(nota-fiscal) = tt-notas-geradas.rw-nota-fiscal no-error.
        if avail nota-fiscal then do:
            for each tt-notas-origem-devol:
                assign c-nro-docto  = tt-notas-origem-devol.nro-docto
                       c-dados-orig = c-dados-orig + string(tt-notas-origem-devol.nro-docto) + " - " + string(tt-notas-origem-devol.dt-emissao) + " | ".
            end.
    
            assign nota-fiscal.observ-nota             = nota-fiscal.observ-nota + " DEVOLUCAO REF: " + c-dados-orig
                   substring(nota-fiscal.char-2,400,3) = "105".
    
            if not can-find(es-devolucao where 
                            es-devolucao.cod-estabel-saida = nota-fiscal.cod-estabel and 
                            es-devolucao.serie             = nota-fiscal.serie       and 
                            es-devolucao.nr-nota-fis       = nota-fiscal.nr-nota-fis and 
                            es-devolucao.nr-pedido         = 0)                      then do:
                create es-devolucao.
                assign es-devolucao.cod-estabel-saida = nota-fiscal.cod-estabel
                       es-devolucao.nome-ab-cli       = nota-fiscal.nome-ab-cli
                       es-devolucao.serie             = nota-fiscal.serie
                       es-devolucao.nr-nota-fis       = nota-fiscal.nr-nota-fis
                       es-devolucao.nat-operacao      = "120102"
                       es-devolucao.data              = today
                       es-devolucao.hora              = time
                       es-devolucao.serie-docto       = "1"
                       es-devolucao.nro-docto         = c-nro-docto
                       es-devolucao.cod-emitente      = emitente.cod-emitente
                       es-devolucao.cod-estabel-devol = input frame {&frame-name} c-est-dest
                       es-devolucao.nr-pedido         = 0
                       es-devolucao.situacao          = 1
                       es-devolucao.cod-depos         = input frame {&frame-name} c-cod-depos-dest.
            end.
    
            run utp/ut-msgs.p(input "show",
                              input 15263,
                              input string(tt-notas-geradas.nr-nota) + "~~" + 
                                    string(nota-fiscal.cod-estabel) + "~~" +  
                                    string(nota-fiscal.serie)).
        end.
    end.

    apply 'choose' to bt-refresh in frame {&frame-name}.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-carrega
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-carrega w-livre
ON CHOOSE OF bt-carrega IN FRAME f-cad /* Button 3 */
DO:

    if not can-find(first tt-saldo where
                          tt-saldo.lg-selec = yes) then do:

        run utp/ut-msgs.p (input 'show',
                           input 17006,
                           input 'Nenhum Saldo Selecionado!').

        return no-apply.
        
    end.

    find first tt-saldo where
               tt-saldo.lg-selec = yes no-error.
    if avail tt-saldo then
        assign c-est-orig:screen-value in frame {&frame-name} = tt-saldo.cod-estabel.

    run pi-carrega-devol.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-inf-comp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inf-comp w-livre
ON CHOOSE OF bt-inf-comp IN FRAME f-cad /* Inf. Compl. */
DO:

    assign current-window:sensitive = no.

    run esp/esft105b.w (output c-des-inf-compl).

    assign current-window:sensitive = yes.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-marca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-marca w-livre
ON CHOOSE OF bt-marca IN FRAME f-cad /* Marcar */
DO:

    if browse br-it-selec:num-selected-rows <> 0 then do:

        get current br-it-selec.

        if avail tt-saldo then
            if tt-saldo.lg-selec then
                assign tt-saldo.lg-selec = no
                       tt-saldo.lg-selec:screen-value in browse br-it-selec = ''.
            else
                assign tt-saldo.lg-selec = yes
                       tt-saldo.lg-selec:screen-value in browse br-it-selec = '*'.
    end.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-nenhum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-nenhum w-livre
ON CHOOSE OF bt-nenhum IN FRAME f-cad /* Nenhum */
DO:


    for each tt-saldo:

        assign tt-saldo.lg-selec = no.

    end.

    {&open-query-br-it-selec}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-refresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-refresh w-livre
ON CHOOSE OF bt-refresh IN FRAME f-cad /* Button 2 */
DO:

    run pi-carrega-saldo.
    run pi-carrega-devol.

    assign c-est-orig:screen-value in frame {&frame-name}        = ''
           c-cfop-devol:screen-value in frame {&frame-name}      = ''
           c-est-dest:screen-value in frame {&frame-name}        = ''
           c-cod-depos-dest:screen-value in frame {&frame-name}  = ''
           c-des-inf-compl                                       = ''.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-selec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-selec w-livre
ON CHOOSE OF bt-selec IN FRAME f-cad /* Button 1 */
DO:

    def var p-ok as log no-undo.

    assign current-window:sensitive = no.

    run esp/esft105a.w (input-output p-cod-estabel,
                        input-output p-it-cod-ini,     
                        input-output p-it-cod-fim,     
                        input-output p-cod-depos-ini,  
                        input-output p-cod-depos-fim,  
                        input-output p-lote-ini,       
                        input-output p-lote-fim,       
                        input-output p-dt-valid-ini,   
                        input-output p-dt-valid-fim,
                        output p-ok). 

    assign current-window:sensitive = yes.

    if p-ok then do:
        
        run pi-carrega-saldo.
        run pi-carrega-devol.

        assign c-est-orig:screen-value in frame {&frame-name}        = ''
               c-cfop-devol:screen-value in frame {&frame-name}      = ''
               c-est-dest:screen-value in frame {&frame-name}        = ''
               c-cod-depos-dest:screen-value in frame {&frame-name}  = ''
               c-des-inf-compl                                       = ''.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-todos w-livre
ON CHOOSE OF bt-todos IN FRAME f-cad /* Todos */
DO:


    for each tt-saldo:

        assign tt-saldo.lg-selec = yes.

    end.

    {&open-query-br-it-selec}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-cfop-devol
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cfop-devol w-livre
ON F5 OF c-cfop-devol IN FRAME f-cad /* Natureza */
DO:

    {include/zoomvar.i &prog-zoom=inzoom/z01in245.w
                       &campo=c-cfop-devol
                       &campozoom=nat-operacao
                       &frame=f-cad
                                              
    }
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cfop-devol w-livre
ON MOUSE-SELECT-DBLCLICK OF c-cfop-devol IN FRAME f-cad /* Natureza */
DO:

    apply 'f5' to self.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-cod-depos-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cod-depos-dest w-livre
ON F5 OF c-cod-depos-dest IN FRAME f-cad /* Dep¢sito Destino */
DO:

    {include/zoomvar.i &prog-zoom=inzoom/z01in084.w
                       &campo=c-cod-depos-dest
                       &campozoom=cod-depos
                       &frame=f-cad}
                        
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cod-depos-dest w-livre
ON MOUSE-SELECT-DBLCLICK OF c-cod-depos-dest IN FRAME f-cad /* Dep¢sito Destino */
DO:

    apply 'f5' to self.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-est-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-est-dest w-livre
ON F5 OF c-est-dest IN FRAME f-cad /* Estab Destino */
do:
    
    {include/zoomvar.i &prog-zoom=adzoom/z01ad107.w
                       &campo=c-est-dest
                       &campozoom=cod-estabel
                       &frame=f-cad}.
  
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-est-dest w-livre
ON MOUSE-SELECT-DBLCLICK OF c-est-dest IN FRAME f-cad /* Estab Destino */
do:

    apply 'f5' to self.
  
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-est-orig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-est-orig w-livre
ON F5 OF c-est-orig IN FRAME f-cad /* Estab Origem */
do:

    {include/zoomvar.i &prog-zoom=adzoom/z01ad107.w
                       &campo=c-est-orig
                       &campozoom=cod-estabel
                       &frame=f-cad}.
  
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-est-orig w-livre
ON MOUSE-SELECT-DBLCLICK OF c-est-orig IN FRAME f-cad /* Estab Origem */
do:

    apply 'f5' to self.
  
end.

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


&Scoped-define BROWSE-NAME br-aloc-devol
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */
on 'leave':U of tt-saldo.qt-devol in browse br-it-selec do:

    if avail tt-saldo then do:

        if input browse br-it-selec tt-saldo.qt-devol = 0 then do:            
    
            run utp/ut-msgs.p (input 'show',
                               input 17006,
                               input 'Quantidade Devoluá∆o n∆o pode ser igual a 0!').

            assign tt-saldo.qt-devol:screen-value in browse br-it-selec = string(tt-saldo.qt-saldo).

            return no-apply.
    
        end.

        if input browse br-it-selec tt-saldo.qt-devol > tt-saldo.qt-saldo then do:            
    
            run utp/ut-msgs.p (input 'show',
                               input 17006,
                               input 'Quantidade Devoluá∆o n∆o pode ser maior que o Saldo!').

            assign tt-saldo.qt-devol:screen-value in browse br-it-selec = string(tt-saldo.qt-saldo).

            return no-apply.
    
        end.
    end.
    
end.


c-est-orig:load-mouse-pointer("image/lupa.cur").
c-est-dest:load-mouse-pointer("image/lupa.cur").
c-cfop-devol:load-mouse-pointer("image/lupa.cur").
c-cod-depos-dest:load-mouse-pointer("image/lupa.cur").

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
       RUN set-position IN h_p-exihel ( 1.17 , 128.86 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             br-it-selec:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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
  DISPLAY c-est-orig c-est-dest c-cod-depos-dest c-cfop-devol 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE bt-calc rt-button RECT-1 br-it-selec bt-marca bt-todos bt-nenhum 
         br-aloc-devol c-est-orig c-est-dest c-cod-depos-dest c-cfop-devol 
         bt-carrega bt-refresh bt-inf-comp bt-selec 
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

  {utp/ut9000.i "ESFT105" "12.01.00.001"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  assign p-cod-estabel   = ''
         p-it-cod-ini    = ''
         p-it-cod-fim    = 'ZZZZZZZZZZZZZZZZ'
         p-cod-depos-ini = ''
         p-cod-depos-fim = 'ZZZ'
         p-lote-ini      = ''
         p-lote-fim      = 'ZZZZZZZZZZZZZZZZZZZZ'
         p-dt-valid-ini  = today
         p-dt-valid-fim  = 12/31/9999.

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-devol w-livre 
PROCEDURE pi-carrega-devol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

empty temp-table tt-devol.

for each tt-saldo where
         tt-saldo.lg-selec = yes:

    find saldo-estoq no-lock where
         saldo-estoq.cod-depos   = tt-saldo.cod-depos   and
         saldo-estoq.cod-estabel = tt-saldo.cod-estabel and
         saldo-estoq.cod-localiz = tt-saldo.cod-localiz and
         saldo-estoq.lote        = tt-saldo.lote        and
         saldo-estoq.it-codigo   = tt-saldo.it-codigo   and
         saldo-estoq.cod-refer   = tt-saldo.cod-refer   no-error.
    if avail saldo-estoq then do:

        create tt-devol.
        buffer-copy saldo-estoq to tt-devol.
        assign tt-devol.desc-item = tt-saldo.desc-item
               tt-devol.qt-devol  = tt-saldo.qt-devol
               tt-devol.qt-saldo  = tt-saldo.qt-saldo.

    end.
end.

{&open-query-br-aloc-devol}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-saldo w-livre 
PROCEDURE pi-carrega-saldo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

empty temp-table tt-saldo.

for each saldo-estoq no-lock where
         saldo-estoq.cod-estabel   = p-cod-estabel   and
         saldo-estoq.it-codigo    >= p-it-cod-ini    and
         saldo-estoq.it-codigo    <= p-it-cod-fim    and
         saldo-estoq.cod-refer     = ''              and
         saldo-estoq.cod-depos    >= p-cod-depos-ini and
         saldo-estoq.cod-depos    <= p-cod-depos-fim and
         saldo-estoq.lote         >= p-lote-ini      and
         saldo-estoq.lote         <= p-lote-fim      and
         saldo-estoq.cod-localiz   = ""              and
         saldo-estoq.dt-vali-lote >= p-dt-valid-ini  and
         saldo-estoq.dt-vali-lote <= p-dt-valid-fim:

    if (saldo-estoq.qtidade-atu - saldo-estoq.qt-alocada - 
        saldo-estoq.qt-aloc-ped - saldo-estoq.qt-aloc-prod) > 0 then do:

        find item no-lock where
             item.it-codigo = saldo-estoq.it-codigo no-error.

        create tt-saldo.
        buffer-copy saldo-estoq to tt-saldo.
        assign tt-saldo.desc-item = if avail item then item.desc-item else ''
               tt-saldo.qt-saldo  = saldo-estoq.qtidade-atu - saldo-estoq.qt-alocada - saldo-estoq.qt-aloc-ped - saldo-estoq.qt-aloc-prod
               tt-saldo.qt-devol  = tt-saldo.qt-saldo.

    end.
end.

{&open-query-br-it-selec}


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-item-devol w-livre 
PROCEDURE pi-cria-item-devol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    empty temp-table tt-itens-devol.
    empty temp-table tt-notas-origem-devol.

    for each tt-item-lote:
        for each item-doc-est use-index item where 
                 item-doc-est.it-codigo = tt-item-lote.it-codigo no-lock,
            each esp-docum-est use-index documento where 
                 esp-docum-est.serie-docto  = item-doc-est.serie-docto  and 
                 esp-docum-est.nro-docto    = item-doc-est.nro-docto    and 
                 esp-docum-est.cod-emitente = emitente.cod-emitente     and 
                 esp-docum-est.nat-operacao = item-doc-est.nat-operacao and 
                 esp-docum-est.cod-estabel  = tt-item-lote.cod-estabel  no-lock:

              find docum-est no-lock where 
                   docum-est.serie-docto  = esp-docum-est.serie-docto  and 
                   docum-est.nro-docto    = esp-docum-est.nro-docto    and 
                   docum-est.cod-emitente = esp-docum-est.cod-emitente and 
                   docum-est.nat-operacao = esp-docum-est.nat-operacao no-error.
                   
              if docum-est.dt-emissao < 02/01/2015 then
                  next.
              
              find first tt-itens-devol where 
                         tt-itens-devol.serie-docto  = item-doc-est.serie-docto  and 
                         tt-itens-devol.cod-emitente = item-doc-est.cod-emitente and 
                         tt-itens-devol.nro-docto    = item-doc-est.nro-docto    and 
                         tt-itens-devol.nat-operacao = item-doc-est.nat-operacao and 
                         tt-itens-devol.sequencia    = item-doc-est.sequencia    and 
                         tt-itens-devol.it-codigo    = item-doc-est.it-codigo    no-error.
              if avail tt-itens-devol then
                  next.

              find item no-lock where 
                   item.it-codigo = item-doc-est.it-codigo no-error.

              assign de-qtd-devol = 0.
              for each devol-forn no-lock where 
                       devol-forn.serie-docto  = item-doc-est.serie-docto  and 
                       devol-forn.nro-docto    = item-doc-est.nro-docto    and 
                       devol-forn.cod-emitente = item-doc-est.cod-emitente and 
                       devol-forn.nat-operacao = item-doc-est.nat-operacao and 
                       devol-forn.sequencia    = item-doc-est.sequencia:
                  assign de-qtd-devol = de-qtd-devol + devol-forn.qt-devol.
              end.

              assign de-saldo-item = item-doc-est.quantidade - de-qtd-devol.

              if de-saldo-item <= 0 then 
                  next.

              if de-saldo-item >= tt-item-lote.quant-saldo then do:
                  create tt-itens-devol.
                  assign tt-itens-devol.serie-docto       = item-doc-est.serie-docto
                         tt-itens-devol.cod-emitente      = item-doc-est.cod-emitente
                         tt-itens-devol.nro-docto         = item-doc-est.nro-docto
                         tt-itens-devol.nat-operacao      = item-doc-est.nat-operacao
                         tt-itens-devol.sequencia         = item-doc-est.sequencia
                         tt-itens-devol.it-codigo         = item-doc-est.it-codigo
                         tt-itens-devol.cod-refer         = item-doc-est.cod-refer
                         tt-itens-devol.desc-nar          = if item.tipo-contr = 4 then substr(item.narrativa,1,60) else item.desc-item
                         tt-itens-devol.quantidade        = item-doc-est.quantidade
                         tt-itens-devol.preco-total       = item-doc-est.preco-total[1]
                         tt-itens-devol.selecionado       = yes
                         tt-itens-devol.qt-ja-devolvida   = de-qtd-devol
                         tt-itens-devol.qt-a-devolver-inf = tt-item-lote.quant-saldo
                         tt-itens-devol.qt-a-devolver     = tt-item-lote.quant-saldo
                         tt-item-lote.quant-saldo         = tt-item-lote.quant-saldo - tt-itens-devol.qt-a-devolver-inf
                         tt-item-lote.serie-docto         = item-doc-est.serie-docto
                         tt-item-lote.cod-emitente        = item-doc-est.cod-emitente
                         tt-item-lote.nro-docto           = item-doc-est.nro-docto
                         tt-item-lote.nat-operacao        = item-doc-est.nat-operacao
                         tt-item-lote.sequencia           = item-doc-est.sequencia.

                    create tt-itens-devol-lote.
                    buffer-copy tt-itens-devol to tt-itens-devol-lote.
                    assign tt-itens-devol-lote.lote        = tt-item-lote.lote
                           tt-itens-devol-lote.cod-depos   = tt-item-lote.cod-depos
                           tt-itens-devol-lote.cod-estabel = tt-item-lote.cod-estabel.

                    find tt-notas-origem-devol
                        where tt-notas-origem-devol.nro-docto = item-doc-est.nro-docto no-error.
                    if not avail tt-notas-origem-devol then do:
                        find docum-est use-index documento no-lock where 
                             docum-est.serie-docto  = item-doc-est.serie-docto  and 
                             docum-est.nro-docto    = item-doc-est.nro-docto    and 
                             docum-est.cod-emitente = item-doc-est.cod-emitente and 
                             docum-est.nat-operacao = item-doc-est.nat-operacao no-error.

                        create tt-notas-origem-devol.
                        assign tt-notas-origem-devol.nro-docto  = item-doc-est.nro-docto
                               tt-notas-origem-devol.dt-emissao = docum-est.dt-emissao.
                    end.
                    leave.
              end.
              else do:
                  create tt-itens-devol.
                  assign tt-itens-devol.serie-docto       = item-doc-est.serie-docto
                         tt-itens-devol.cod-emitente      = item-doc-est.cod-emitente
                         tt-itens-devol.nro-docto         = item-doc-est.nro-docto
                         tt-itens-devol.nat-operacao      = item-doc-est.nat-operacao
                         tt-itens-devol.sequencia         = item-doc-est.sequencia
                         tt-itens-devol.it-codigo         = item-doc-est.it-codigo
                         tt-itens-devol.cod-refer         = item-doc-est.cod-refer
                         tt-itens-devol.desc-nar          = if item.tipo-contr = 4 then substr(item.narrativa,1,60) else item.desc-item
                         tt-itens-devol.quantidade        = item-doc-est.quantidade
                         tt-itens-devol.preco-total       = item-doc-est.preco-total[1]
                         tt-itens-devol.selecionado       = yes
                         tt-itens-devol.qt-ja-devolvida   = de-qtd-devol
                         tt-itens-devol.qt-a-devolver-inf = de-saldo-item
                         tt-itens-devol.qt-a-devolver     = tt-item-lote.quant-saldo
                         tt-item-lote.quant-saldo         = tt-item-lote.quant-saldo - de-saldo-item
                         tt-item-lote.serie-docto         = item-doc-est.serie-docto
                         tt-item-lote.cod-emitente        = item-doc-est.cod-emitente
                         tt-item-lote.nro-docto           = item-doc-est.nro-docto
                         tt-item-lote.nat-operacao        = item-doc-est.nat-operacao
                         tt-item-lote.sequencia           = item-doc-est.sequencia.

                   create tt-itens-devol-lote.
                   buffer-copy tt-itens-devol to tt-itens-devol-lote.
                   assign tt-itens-devol-lote.lote        = tt-item-lote.lote
                          tt-itens-devol-lote.cod-depos   = tt-item-lote.cod-depos
                          tt-itens-devol-lote.cod-estabel = tt-item-lote.cod-estabel.

                   find tt-notas-origem-devol
                        where tt-notas-origem-devol.nro-docto = item-doc-est.nro-docto no-error.
                   if not avail tt-notas-origem-devol then do:
                       find docum-est use-index documento no-lock where 
                            docum-est.serie-docto  = item-doc-est.serie-docto  and 
                            docum-est.nro-docto    = item-doc-est.nro-docto    and 
                            docum-est.cod-emitente = item-doc-est.cod-emitente and 
                            docum-est.nat-operacao = item-doc-est.nat-operacao no-error.
                       create tt-notas-origem-devol.
                       assign tt-notas-origem-devol.nro-docto  = item-doc-est.nro-docto
                              tt-notas-origem-devol.dt-emissao = docum-est.dt-emissao.
                   end.
              end.
              if tt-item-lote.quant-saldo = 0 then
                  leave.
        end.
    end.

    for each tt-item-lote where 
             tt-item-lote.quant-saldo > 0:
        for each item-doc-est use-index item no-lock where 
                 item-doc-est.it-codigo = tt-item-lote.it-codigo,
            each esp-docum-est use-index documento no-lock where 
                 esp-docum-est.serie-docto  = item-doc-est.serie-docto  and 
                 esp-docum-est.nro-docto    = item-doc-est.nro-docto    and  
                 esp-docum-est.cod-emitente = emitente.cod-emitente     and 
                 esp-docum-est.nat-operacao = item-doc-est.nat-operacao and 
                 esp-docum-est.cod-estabel  = tt-item-lote.cod-estabel:
            
            find first tt-itens-devol where 
                       tt-itens-devol.serie-docto  = item-doc-est.serie-docto  and 
                       tt-itens-devol.cod-emitente = item-doc-est.cod-emitente and 
                       tt-itens-devol.nro-docto    = item-doc-est.nro-docto    and 
                       tt-itens-devol.nat-operacao = item-doc-est.nat-operacao and 
                       tt-itens-devol.sequencia    = item-doc-est.sequencia    and 
                       tt-itens-devol.it-codigo    = item-doc-est.it-codigo    no-error.
            if avail tt-itens-devol then
                next.
                
            find item no-lock where 
                 item.it-codigo = item-doc-est.it-codigo no-error.


            assign de-qtd-devol = 0.
            for each devol-forn no-lock where 
                     devol-forn.serie-docto  = item-doc-est.serie-docto  and 
                     devol-forn.nro-docto    = item-doc-est.nro-docto    and 
                     devol-forn.cod-emitente = item-doc-est.cod-emitente and 
                     devol-forn.nat-operacao = item-doc-est.nat-operacao and 
                     devol-forn.sequencia    = item-doc-est.sequencia:
                assign de-qtd-devol = de-qtd-devol + devol-forn.qt-devol.
            end.

            assign de-saldo-item = item-doc-est.quantidade - de-qtd-devol.

            if de-saldo-item <= 0 then
                next.

            if de-saldo-item >= tt-item-lote.quant-saldo then do:
                create tt-itens-devol.
                assign tt-itens-devol.serie-docto       = item-doc-est.serie-docto
                       tt-itens-devol.cod-emitente      = item-doc-est.cod-emitente
                       tt-itens-devol.nro-docto         = item-doc-est.nro-docto
                       tt-itens-devol.nat-operacao      = item-doc-est.nat-operacao
                       tt-itens-devol.sequencia         = item-doc-est.sequencia
                       tt-itens-devol.it-codigo         = item-doc-est.it-codigo
                       tt-itens-devol.cod-refer         = item-doc-est.cod-refer
                       tt-itens-devol.desc-nar          = if item.tipo-contr = 4 then substr(item.narrativa,1,60) else item.desc-item
                       tt-itens-devol.quantidade        = item-doc-est.quantidade
                       tt-itens-devol.preco-total       = item-doc-est.preco-total[1]
                       tt-itens-devol.selecionado       = yes
                       tt-itens-devol.qt-ja-devolvida   = de-qtd-devol
                       tt-itens-devol.qt-a-devolver-inf = tt-item-lote.quant-saldo
                       tt-itens-devol.qt-a-devolver     = tt-item-lote.quant-saldo
                       tt-item-lote.quant-saldo         = tt-item-lote.quant-saldo - tt-itens-devol.qt-a-devolver-inf
                       tt-item-lote.serie-docto         = item-doc-est.serie-docto
                       tt-item-lote.cod-emitente        = item-doc-est.cod-emitente
                       tt-item-lote.nro-docto           = item-doc-est.nro-docto
                       tt-item-lote.nat-operacao        = item-doc-est.nat-operacao
                       tt-item-lote.sequencia           = item-doc-est.sequencia.

                  create tt-itens-devol-lote.
                  buffer-copy tt-itens-devol to tt-itens-devol-lote.
                  assign tt-itens-devol-lote.lote        = tt-item-lote.lote
                         tt-itens-devol-lote.cod-depos   = tt-item-lote.cod-depos
                         tt-itens-devol-lote.cod-estabel = tt-item-lote.cod-estabel.

                  find tt-notas-origem-devol
                      where tt-notas-origem-devol.nro-docto = item-doc-est.nro-docto no-error.
                  if not avail tt-notas-origem-devol then do:
                      find docum-est use-index documento
                        where docum-est.serie-docto  = item-doc-est.serie-docto
                          and docum-est.nro-docto    = item-doc-est.nro-docto
                          and docum-est.cod-emitente = item-doc-est.cod-emitente
                          and docum-est.nat-operacao = item-doc-est.nat-operacao no-lock no-error.

                      create tt-notas-origem-devol.
                      assign tt-notas-origem-devol.nro-docto  = item-doc-est.nro-docto
                             tt-notas-origem-devol.dt-emissao = docum-est.dt-emissao.
                  end.
                  leave.
            end.
            else do:
                create tt-itens-devol.
                assign tt-itens-devol.serie-docto       = item-doc-est.serie-docto
                       tt-itens-devol.cod-emitente      = item-doc-est.cod-emitente
                       tt-itens-devol.nro-docto         = item-doc-est.nro-docto
                       tt-itens-devol.nat-operacao      = item-doc-est.nat-operacao
                       tt-itens-devol.sequencia         = item-doc-est.sequencia
                       tt-itens-devol.it-codigo         = item-doc-est.it-codigo
                       tt-itens-devol.cod-refer         = item-doc-est.cod-refer
                       tt-itens-devol.desc-nar          = if item.tipo-contr = 4 then substr(item.narrativa,1,60) else item.desc-item
                       tt-itens-devol.quantidade        = item-doc-est.quantidade
                       tt-itens-devol.preco-total       = item-doc-est.preco-total[1]
                       tt-itens-devol.selecionado       = yes
                       tt-itens-devol.qt-ja-devolvida   = de-qtd-devol
                       tt-itens-devol.qt-a-devolver-inf = de-saldo-item
                       tt-itens-devol.qt-a-devolver     = tt-item-lote.quant-saldo
                       tt-item-lote.quant-saldo         = tt-item-lote.quant-saldo - de-saldo-item
                       tt-item-lote.serie-docto         = item-doc-est.serie-docto
                       tt-item-lote.cod-emitente        = item-doc-est.cod-emitente
                       tt-item-lote.nro-docto           = item-doc-est.nro-docto
                       tt-item-lote.nat-operacao        = item-doc-est.nat-operacao
                       tt-item-lote.sequencia           = item-doc-est.sequencia.

                 create tt-itens-devol-lote.
                 buffer-copy tt-itens-devol to tt-itens-devol-lote.
                 assign tt-itens-devol-lote.lote        = tt-item-lote.lote
                        tt-itens-devol-lote.cod-depos   = tt-item-lote.cod-depos
                        tt-itens-devol-lote.cod-estabel = tt-item-lote.cod-estabel.

                 find tt-notas-origem-devol
                      where tt-notas-origem-devol.nro-docto = item-doc-est.nro-docto no-error.
                 if not avail tt-notas-origem-devol then do:
                     find docum-est use-index documento
                       where docum-est.serie-docto  = item-doc-est.serie-docto
                         and docum-est.nro-docto    = item-doc-est.nro-docto
                         and docum-est.cod-emitente = item-doc-est.cod-emitente
                         and docum-est.nat-operacao = item-doc-est.nat-operacao no-lock no-error.
                     create tt-notas-origem-devol.
                     assign tt-notas-origem-devol.nro-docto  = item-doc-est.nro-docto
                            tt-notas-origem-devol.dt-emissao = docum-est.dt-emissao.
                 end.
            end.

            if tt-item-lote.quant-saldo = 0 then 
                leave.
        end.
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-devolucao w-livre 
PROCEDURE pi-gera-devolucao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def var l-cria as log no-undo.
    
    BLOCO-A:
    do transaction on error undo, retry:
        if not valid-handle(h-bodi317in) then do:
            run dibo/bodi317in.p persistent set h-bodi317in.
            run inicializaBOS in h-bodi317in(output h-bodi317pr,
                                             output h-bodi317sd,
                                             output h-bodi317im1bra,
                                             output h-bodi317va).
        end.

        if not avail emitente then do:
            find estabelec no-lock where
                 estabelec.cod-estabel = input frame {&frame-name} c-est-dest no-error.
        
            find emitente no-lock where
                 emitente.cod-emitente = estabelec.cod-emitente no-error.
        end.

        run criaWtdocto in h-bodi317sd (input c-seg-usuario,
                                        input input frame {&frame-name} c-est-orig,
                                        input "1", /* SÇrie 1 fixa */
                                        input 1,
                                        input emitente.nome-abrev,
                                        input "",
                                        input  4, /* 1 - Sistema - 2 - Manual - 4 - Complementar Marc. (nota-fiscal.ind-tip-nota) */ 
                                        input  9999,
                                        input  today,
                                        input  0,
                                        input  input frame {&frame-name} c-cfop-devol,
                                        input  0,
                                        output i-seq-wt-docto,
                                        output l-proc-ok-aux).

        run devolveErrosbodi317sd in h-bodi317sd(output c-ultimo-metodo-exec,
                                                 output table RowErrors).
    
        find RowErrors no-lock no-error.
        if avail RowErrors and RowErrors.ErrorNumber <> 15047 then do:
            if valid-handle(h-acomp) then
                run pi-finalizar in h-acomp.
    
            {method/showmessage.i1}
            {method/showmessage.i2 &Modal="yes"}
        end.
        if not l-proc-ok-aux then do:
            if valid-handle(h-bodi317in) then 
                run finalizaBOS in h-bodi317in.
            undo BLOCO-A, leave.
        end.
    
        run emptyRowErrors in h-bodi317in.

        run pi-cria-item-devol.

        find first tt-item-lote
            where tt-item-lote.quant-saldo > 0 no-error.
        if avail tt-item-lote then do:
             run utp/ut-msgs.p(input "show",
                               input 17006,
                               input "Item devolvido n∆o possui notas com saldos!" + "~~" + "Item:" + string(tt-item-lote.it-codigo) + " n∆o possui notas de entradas com saldo para realizar devoluá∆o").
             undo BLOCO-A,leave.
        end.
    
        run emptyRowErrors in h-bodi317in.
    
        run geraWtItdoctoPartindodoTtItensDevol in h-bodi317sd(input  i-seq-wt-docto,
                                                               input  table tt-itens-devol,
                                                               output l-proc-ok-aux).
    
        for each wt-it-docto exclusive-lock where 
                 wt-it-docto.seq-wt-docto    = i-seq-wt-docto:
    
            if wt-it-docto.quantidade[1] - trunc(wt-it-docto.quantidade[1], 0) > 0 then do:
                assign wt-it-docto.quantidade[1] = trunc(wt-it-docto.quantidade[1],0)
                       wt-it-docto.quantidade[2] = trunc(wt-it-docto.quantidade[2],0).
            end.
        end.
    
        run devolveErrosbodi317sd in h-bodi317sd(output c-ultimo-metodo-exec,
                                                 output table RowErrors).

        find RowErrors no-lock no-error.
        if avail RowErrors and RowErrors.ErrorNumber <> 15047 then do:
            if valid-handle(h-acomp) then
                run pi-finalizar in h-acomp.
            {method/showmessage.i1}
            {method/showmessage.i2 &Modal="yes"}
        end.

        if not l-proc-ok-aux then do:
            if valid-handle(h-bodi317in) then
                run finalizaBOS in h-bodi317in.
            undo BLOCO-A, leave.
        end.
    
        for each wt-fat-ser-lote exclusive-lock where 
                 wt-fat-ser-lote.seq-wt-docto = i-seq-wt-docto:
            create tt-wt-fat-ser-lote.
            buffer-copy wt-fat-ser-lote to tt-wt-fat-ser-lote.
            delete wt-fat-ser-lote.
        end.
        
        for each tt-wt-fat-ser-lote where 
                 tt-wt-fat-ser-lote.seq-wt-docto = i-seq-wt-docto:
    
            find first wt-it-docto no-lock where 
                       wt-it-docto.seq-wt-docto    = tt-wt-fat-ser-lote.seq-wt-docto    and 
                       wt-it-docto.seq-wt-it-docto = tt-wt-fat-ser-lote.seq-wt-it-docto no-error.
            if avail wt-it-docto then do:
                find first tt-itens-devol-lote where 
                           tt-itens-devol-lote.serie-docto       = wt-it-docto.serie-comp    and 
                           tt-itens-devol-lote.nro-docto         = wt-it-docto.nro-comp      and 
                           tt-itens-devol-lote.nat-operacao      = wt-it-docto.nat-comp      and 
                           tt-itens-devol-lote.sequencia         = wt-it-docto.seq-comp      and 
                           tt-itens-devol-lote.it-codigo         = wt-it-docto.it-codigo     and 
                           tt-itens-devol-lote.qt-a-devolver-inf = wt-it-docto.quantidade[1] no-error.
                if avail tt-itens-devol-lote then do:

                    find first saldo-estoq no-lock where 
                               saldo-estoq.cod-estabel = tt-itens-devol-lote.cod-estabel and 
                               saldo-estoq.cod-depos   = tt-itens-devol-lote.cod-depos   and 
                               saldo-estoq.it-codigo   = tt-itens-devol-lote.it-codigo   and 
                               saldo-estoq.cod-refer   = ""                              and 
                               saldo-estoq.lote        = tt-itens-devol-lote.lote        no-error.
                    if not avail saldo-estoq or (avail saldo-estoq
                                                 and saldo-estoq.qtidade-atu - (saldo-estoq.qt-alocada  +
                                                     saldo-estoq.qt-aloc-ped +
                                                     saldo-estoq.qt-aloc-prod) < tt-itens-devol-lote.qt-a-devolver-inf) then do:
    
                        run utp/ut-msgs.p(input "show",
                                          input 17006,
                                          input "Item sem saldo para atender qtd solicitada!" + "~~" + 
                                                "Item sem saldo:" + tt-itens-devol-lote.it-codigo + 
                                                "Lote:" + tt-itens-devol-lote.lote + 
                                                "Dep:" + tt-itens-devol-lote.cod-depos).
                        return "NOK".                                                                                                   
                    end.    
                    
                    find RowErrors no-lock no-error.
                    if avail RowErrors                and 
                       RowErrors.ErrorNumber <> 15047 then do:
                        if valid-handle(h-acomp) then
                            run pi-finalizar in h-acomp.

                        {method/showmessage.i1}
                        {method/showmessage.i2 &Modal="yes"}
                    end.
    
                    if return-value = "NOK" then
                        undo BLOCO-A,leave.
    
                    assign l-cria = no.
    
                    if not can-find(first wt-fat-ser-lote where 
                                          wt-fat-ser-lote.seq-wt-docto    = wt-it-docto.seq-wt-docto    and   
                                          wt-fat-ser-lote.seq-wt-it-docto = wt-it-docto.seq-wt-it-docto and   
                                          wt-fat-ser-lote.it-codigo       = saldo-estoq.it-codigo       and   
                                          wt-fat-ser-lote.cod-depos       = saldo-estoq.cod-depos       and   
                                          wt-fat-ser-lote.cod-localiz     = saldo-estoq.cod-localiz     and   
                                          wt-fat-ser-lote.lote            = saldo-estoq.lote) then
                        assign l-cria = yes.
    
                    run criaAlteraWtFatSerLote in h-bodi317sd (input l-cria,
                                                               input  wt-it-docto.seq-wt-docto,
                                                               input  wt-it-docto.seq-wt-it-docto,
                                                               input  saldo-estoq.it-codigo,
                                                               input  saldo-estoq.cod-depos,  
                                                               input  saldo-estoq.cod-localiz,
                                                               input  saldo-estoq.lote,  
                                                               input  tt-itens-devol-lote.qt-a-devolver-inf,
                                                               input  tt-itens-devol-lote.qt-a-devolver-inf,
                                                               input  saldo-estoq.dt-vali-lote,
                                                               output l-proc-ok-aux).
                end.
            end.
            else do:
                run utp/ut-msgs.p(input "show",
                                  input 17006,
                                  input "wt-it-docto n∆o criada para o item!" + "~~" + "wt-it-docto n∆o criada para o item:" + tt-item-lote.it-codigo).
                undo BLOCO-A,leave.
            end.
        end.
    
        for each wt-it-docto exclusive-lock where
                 wt-it-docto.seq-wt-docto = i-seq-wt-docto:
    
            if not can-find(first wt-fat-ser-lote where
                                  wt-fat-ser-lote.seq-wt-docto    = wt-it-docto.seq-wt-docto     and
                                  wt-fat-ser-lote.seq-wt-it-docto = wt-it-docto.seq-wt-it-docto) then do:
    
                delete wt-it-docto.
            end.
        end.
    
        run geraWtFatRepre in h-bodi317sd(input i-seq-wt-docto,
                                          output l-proc-ok-aux).
        /* FinalizaÁ„o das BOS utilizada no c·lculo */
        run finalizaBOS in h-bodi317in.
        
        /* ReinicializaÁ„o das BOS para C·lculo */
        run dibo/bodi317in.p persistent set h-bodi317in.
        run inicializaBOS in h-bodi317in(output h-bodi317pr,
                                         output h-bodi317sd,     
                                         output h-bodi317im1bra,
                                         output h-bodi317va).
        
        run emptyRowErrors           in h-bodi317in.
        
        run inicializaAcompanhamento in h-bodi317pr.
        
        run confirmaCalculo          in h-bodi317pr(input  i-seq-wt-docto,
                                                    output l-proc-ok-aux).
        run finalizaAcompanhamento   in h-bodi317pr.
    
        run devolveErrosbodi317pr    in h-bodi317pr (output c-ultimo-metodo-exec,
                                                     output table RowErrors).
        
        find RowErrors no-lock no-error.
        if avail RowErrors and RowErrors.ErrorNumber <> 15047 then do:
            if valid-handle(h-acomp) then
                run pi-finalizar in h-acomp.
            {method/showmessage.i1}
            {method/showmessage.i2 &Modal="yes"}
        end.
        if not l-proc-ok-aux then do:
            if valid-handle(h-bodi317in) then
                run finalizaBOS in h-bodi317in.
            undo BLOCO-A, leave.
        end.
        
        /* Efetivar nota */
        if not valid-handle(h-bodi317ef) then
            run dibo/bodi317ef.p persistent set h-bodi317ef.
    
        run emptyRowErrors           in h-bodi317in.
        run inicializaAcompanhamento in h-bodi317ef.
        run limpaTtnotasGeradas      in h-bodi317ef (output l-proc-ok-aux).
        run setaHandlesBOS           in h-bodi317ef(h-bodi317pr,     h-bodi317sd, 
                                                    h-bodi317im1bra, h-bodi317va).
    
        run efetivanota              in h-bodi317ef(input  i-seq-wt-docto,
                                                    input  yes,
                                                    output l-proc-ok-aux).
        run finalizaAcompanhamento   in h-bodi317ef.
        run devolveErrosbodi317ef    in h-bodi317ef(output c-ultimo-metodo-exec,
                                                    output table RowErrors).
    
        find RowErrors no-lock no-error.
        if avail RowErrors                and 
           RowErrors.ErrorNumber <> 15047 then do:
            if valid-handle(h-acomp) then
                run pi-finalizar in h-acomp.
            {method/showmessage.i1}
            {method/showmessage.i2 &Modal="yes"}
        end.
    
        if valid-handle(h-bodi317in) then
            run finalizaBOS in h-bodi317in.
    
        if not l-proc-ok-aux then do:
            undo BLOCO-A, leave.
        end.

        if valid-handle(h-acomp) then
            run pi-finalizar in h-acomp.
    
        run buscaTTnotasGeradas in h-bodi317ef(output l-proc-ok-aux,
                                               output table tt-notas-geradas).
    
    end. /* Fim transacao */
        
    return "OK".

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
  {src/adm/template/snd-list.i "tt-saldo"}
  {src/adm/template/snd-list.i "tt-devol"}

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

