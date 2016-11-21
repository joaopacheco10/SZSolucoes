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
{include/i-prgvrs.i ESFT104 12.01.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local var Definitions ---                                       */

def temp-table tt-notas-geradas no-undo
    field rw-nota-fiscal as   rowid
    field nr-nota        like nota-fiscal.nr-nota-fis
    field seq-wt-docto   like wt-docto.seq-wt-docto.

def temp-table RowErrors no-undo
    field ErrorSequence    as integer
    field ErrorNumber      as integer
    field ErrorDescription as character
    field ErrorParameters  as character
    field ErrorType        as character
    field ErrorHelp        as character
    field ErrorSubType     as character.

{cdp/cd0667.i}

def temp-table tt-saldo no-undo like saldo-estoq
    field lg-selec  as log format '*/'
    field desc-item like item.desc-item
    field qt-saldo  like saldo-estoq.qtidade-atu
    field qt-transf like saldo-estoq.qtidade-atu.

def temp-table tt-transf no-undo like saldo-estoq
    field desc-item like item.desc-item
    field qt-saldo  like saldo-estoq.qtidade-atu
    field qt-transf like saldo-estoq.qtidade-atu.

def temp-table tt-item-entr-st no-undo like item-entr-st.

def temp-table tt-it-nota-fisc no-undo like it-nota-fisc
    field r-rowid as rowid.

def temp-table tt-docum-est no-undo like docum-est
    field r-rowid as rowid.

def temp-table tt-rat-lote-esp no-undo like rat-lote
    index documento is primary serie-docto nro-docto cod-emitente nat-operacao sequencia
    index ch-item it-codigo .


def buffer b-estabelec for estabelec.


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

def var c-cod-estabel  as character               no-undo.
def var c-nome-abrev   like ped-venda.nome-abrev  no-undo.
def var c-serie-bo     as char                    no-undo.
def var c-nat-operacao like emitente.nat-operacao no-undo.

def var de-qt-mercad         as dec     no-undo.           
def var de-qt-volumes        as dec     no-undo.  
def var i-seq-wt-docto       as int     no-undo.
def var c-ultimo-metodo-exec as char    no-undo.    
def var i-seq-nft            as integer no-undo.
def var l-proc-ok-aux        as log     no-undo.
def var h-bodi317in          as handle  no-undo.
def var h-bodi317pr          as handle  no-undo.
def var h-bodi317sd          as handle  no-undo.
def var h-bodi317im1bra      as handle  no-undo.
def var h-bodi317va          as handle  no-undo.
def var de-vl-preori-ped     as decimal no-undo.
def var i-seq-wt-it-docto    as integer no-undo.
def var l-eliminou-lote      as logical no-undo.
def var h-bodi317ef          as handle  no-undo.
def var h-boin090            as handle  no-undo.
def var h-boin176            as handle  no-undo.
def var hShowMsg             as handle  no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-it-selec

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-saldo tt-transf

/* Definitions for BROWSE br-it-selec                                   */
&Scoped-define FIELDS-IN-QUERY-br-it-selec tt-saldo.lg-selec tt-saldo.cod-estabel tt-saldo.it-codigo tt-saldo.desc-item tt-saldo.cod-depos tt-saldo.lote tt-saldo.dt-vali-lote tt-saldo.qt-transf tt-saldo.qt-saldo tt-saldo.qt-aloc-ped   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-it-selec tt-saldo.qt-transf   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-it-selec tt-saldo
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-it-selec tt-saldo
&Scoped-define SELF-NAME br-it-selec
&Scoped-define QUERY-STRING-br-it-selec FOR EACH tt-saldo
&Scoped-define OPEN-QUERY-br-it-selec OPEN QUERY {&SELF-NAME} FOR EACH tt-saldo.
&Scoped-define TABLES-IN-QUERY-br-it-selec tt-saldo
&Scoped-define FIRST-TABLE-IN-QUERY-br-it-selec tt-saldo


/* Definitions for BROWSE br-it-transf                                  */
&Scoped-define FIELDS-IN-QUERY-br-it-transf tt-transf.cod-estabel tt-transf.it-codigo tt-transf.desc-item tt-transf.cod-depos tt-transf.lote tt-transf.dt-vali-lote tt-transf.qt-transf   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-it-transf   
&Scoped-define SELF-NAME br-it-transf
&Scoped-define QUERY-STRING-br-it-transf FOR EACH tt-transf
&Scoped-define OPEN-QUERY-br-it-transf OPEN QUERY {&SELF-NAME} FOR EACH tt-transf.
&Scoped-define TABLES-IN-QUERY-br-it-transf tt-transf
&Scoped-define FIRST-TABLE-IN-QUERY-br-it-transf tt-transf


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-it-selec}~
    ~{&OPEN-QUERY-br-it-transf}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button RECT-1 bt-calc br-it-selec ~
bt-marca bt-todos bt-nenhum br-it-transf c-est-orig c-cfop-saida c-est-dest ~
c-cfop-ent c-serie c-cod-depos bt-carrega bt-inf-comp bt-refresh bt-selec 
&Scoped-Define DISPLAYED-OBJECTS c-est-orig c-cfop-saida c-est-dest ~
c-cfop-ent c-serie c-cod-depos 

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

DEFINE VARIABLE c-cfop-ent AS CHARACTER FORMAT "X(256)":U 
     LABEL "Natureza Entrada" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE c-cfop-saida AS CHARACTER FORMAT "X(256)":U 
     LABEL "Natureza Sa°da" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-depos AS CHARACTER FORMAT "X(256)":U 
     LABEL "Dep¢sito" 
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

DEFINE VARIABLE c-serie AS CHARACTER FORMAT "X(5)":U 
     LABEL "SÇrie" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

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
DEFINE QUERY br-it-selec FOR 
      tt-saldo SCROLLING.

DEFINE QUERY br-it-transf FOR 
      tt-transf SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
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
tt-saldo.qt-transf      column-label 'Transferir'
tt-saldo.qt-saldo       column-label 'Saldo'
tt-saldo.qt-aloc-ped
enable
tt-saldo.qt-transf
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 144.43 BY 12
         TITLE "Itens da Seleá∆o" FIT-LAST-COLUMN.

DEFINE BROWSE br-it-transf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-it-transf w-livre _FREEFORM
  QUERY br-it-transf DISPLAY
      tt-transf.cod-estabel
tt-transf.it-codigo
tt-transf.desc-item
tt-transf.cod-depos     format 'x(5)'
tt-transf.lote          
tt-transf.dt-vali-lote
tt-transf.qt-transf     column-label 'Qtd Transf'
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 119.43 BY 11.5
         TITLE "Itens a Tranferir" FIT-LAST-COLUMN.


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
     br-it-transf AT ROW 16.25 COL 1.57 WIDGET-ID 300
     c-est-orig AT ROW 18.38 COL 134.14 COLON-ALIGNED WIDGET-ID 14
     c-cfop-saida AT ROW 19.38 COL 133.43 COLON-ALIGNED WIDGET-ID 18
     c-est-dest AT ROW 20.38 COL 134.14 COLON-ALIGNED WIDGET-ID 16
     c-cfop-ent AT ROW 21.38 COL 133.43 COLON-ALIGNED WIDGET-ID 20
     c-serie AT ROW 22.38 COL 133.43 COLON-ALIGNED WIDGET-ID 22
     c-cod-depos AT ROW 23.38 COL 135.43 COLON-ALIGNED WIDGET-ID 24
     bt-carrega AT ROW 16.5 COL 130.57 WIDGET-ID 34
     bt-inf-comp AT ROW 24.5 COL 126.29 WIDGET-ID 38
     bt-refresh AT ROW 1.17 COL 7.72 WIDGET-ID 30
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
/* BROWSE-TAB br-it-selec bt-calc f-cad */
/* BROWSE-TAB br-it-transf d-peso-item f-cad */
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

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-it-selec
/* Query rebuild information for BROWSE br-it-selec
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-saldo.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-it-selec */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-it-transf
/* Query rebuild information for BROWSE br-it-transf
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-transf.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-it-transf */
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
DO:

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
                    natur-oper.nat-operacao = input frame {&frame-name} c-cfop-saida) then do:

        run utp/ut-msgs.p (input 'show',
                           input 17006,
                           input 'Natureza Saida Inv†lida!':U).
        apply 'entry' to c-cfop-saida in frame {&frame-name}.

        return no-apply.

    end.

    if not can-find(natur-oper where
                    natur-oper.nat-operacao = input frame {&frame-name} c-cfop-ent) then do:

        run utp/ut-msgs.p (input 'show',
                           input 17006,
                           input 'Natureza Entrada Inv†lida!':U).
        apply 'entry' to c-cfop-ent in frame {&frame-name}.

        return no-apply.

    end.

    if not can-find(deposito where
                    deposito.cod-depos = input frame {&frame-name} c-cod-depos) then do:

        run utp/ut-msgs.p (input 'show',
                           input 17006,
                           input 'Dep¢sito Inv†lido!':U).
        apply 'entry' to c-cod-depos in frame {&frame-name}.

        return no-apply.

    end.

    if not can-find(serie where
                    serie.serie = input frame {&frame-name} c-serie) then do:

        run utp/ut-msgs.p (input 'show',
                           input 17006,
                           input 'SÇrie Inv†lida!':U).
        apply 'entry' to c-serie in frame {&frame-name}.

        return no-apply.

    end.

    if not can-find(first tt-transf) then do:

        run utp/ut-msgs.p (input 'show',
                           input 17006,
                           input 'Nenhum Item selecionado!':U).

        return no-apply.

    end.

    if can-find(first tt-transf where
                      tt-transf.qt-transf = 0) then do:

        run utp/ut-msgs.p (input 'show',
                           input 17006,
                           input 'Quantidade Transferància deve ser maior que 0 (zero)!':U).

        return no-apply.

    end.    

    run pi-gera-transf.

    if can-find(first tt-erro) then do:
        for each tt-erro:
    
            run utp/ut-msgs.p (input 'show',
                               input tt-erro.cd-erro,
                               input tt-erro.mensagem).        
        end.
    end.
    else do:
        for first tt-notas-geradas:
            find nota-fiscal where 
                 rowid(nota-fiscal) = tt-notas-geradas.rw-nota-fiscal no-error.
    
            if avail nota-fiscal then
                run utp/ut-msgs.p (input 'show',
                                   input 15825,
                                   input 'Nota Fiscal Gerada:'                + '~~' +
                                         'Estab - ' + nota-fiscal.cod-estabel + chr(10) +
                                         'Serie - ' + nota-fiscal.serie       + chr(10) +
                                         'Nota  - ' + nota-fiscal.nr-nota-fis).
        end.
    
        if can-find(first tt-notas-geradas) then
            run pi-recebe-nft.

    end.

    apply 'choose' to bt-refresh in frame {&frame-name}.
  
END.

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

    run pi-carrega-transf.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-inf-comp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inf-comp w-livre
ON CHOOSE OF bt-inf-comp IN FRAME f-cad /* Inf. Compl. */
DO:

    assign current-window:sensitive = no.

    run esp/esft104b.w (output c-des-inf-compl).

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
    run pi-carrega-transf.

    assign c-est-orig:screen-value in frame {&frame-name}   = ''
           c-cfop-saida:screen-value in frame {&frame-name} = ''
           c-est-dest:screen-value in frame {&frame-name}   = ''
           c-cfop-ent:screen-value in frame {&frame-name}   = ''
           c-serie:screen-value in frame {&frame-name}      = ''
           c-cod-depos:screen-value in frame {&frame-name}  = ''
           c-des-inf-compl                                  = ''.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-selec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-selec w-livre
ON CHOOSE OF bt-selec IN FRAME f-cad /* Button 1 */
DO:

    def var p-ok as log no-undo.

    assign current-window:sensitive = no.

    run esp/esft104a.w (input-output p-cod-estabel,
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
        run pi-carrega-transf.

        assign c-est-orig:screen-value in frame {&frame-name}   = ''
               c-cfop-saida:screen-value in frame {&frame-name} = ''
               c-est-dest:screen-value in frame {&frame-name}   = ''
               c-cfop-ent:screen-value in frame {&frame-name}   = ''
               c-serie:screen-value in frame {&frame-name}      = ''
               c-cod-depos:screen-value in frame {&frame-name}  = ''
               c-des-inf-compl                                  = ''.
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


&Scoped-define SELF-NAME c-cfop-ent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cfop-ent w-livre
ON F5 OF c-cfop-ent IN FRAME f-cad /* Natureza Entrada */
DO:

    {include/zoomvar.i &prog-zoom=inzoom/z01in245.w
                       &campo=c-cfop-ent
                       &campozoom=nat-operacao
                       &frame=f-cad
                                              
    }
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cfop-ent w-livre
ON MOUSE-SELECT-DBLCLICK OF c-cfop-ent IN FRAME f-cad /* Natureza Entrada */
DO:

    apply 'f5' to self.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-cfop-saida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cfop-saida w-livre
ON F5 OF c-cfop-saida IN FRAME f-cad /* Natureza Sa°da */
do:

    {include/zoomvar.i &prog-zoom=inzoom/z01in245.w
                       &campo=c-cfop-saida
                       &campozoom=nat-operacao
                       &frame=f-cad
                                              
    }
  
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cfop-saida w-livre
ON MOUSE-SELECT-DBLCLICK OF c-cfop-saida IN FRAME f-cad /* Natureza Sa°da */
DO:

    apply 'f5' to self.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-cod-depos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cod-depos w-livre
ON F5 OF c-cod-depos IN FRAME f-cad /* Dep¢sito */
DO:

    {include/zoomvar.i &prog-zoom=inzoom/z01in084.w
                       &campo=c-cod-depos
                       &campozoom=cod-depos
                       &frame=f-cad}
                        
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cod-depos w-livre
ON MOUSE-SELECT-DBLCLICK OF c-cod-depos IN FRAME f-cad /* Dep¢sito */
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


&Scoped-define SELF-NAME c-serie
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-serie w-livre
ON F5 OF c-serie IN FRAME f-cad /* SÇrie */
DO:

    {include/zoomvar.i &prog-zoom=inzoom/z01in407.w
                       &campo=c-serie
                       &campozoom=serie
                       &frame=f-cad}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-serie w-livre
ON MOUSE-SELECT-DBLCLICK OF c-serie IN FRAME f-cad /* SÇrie */
DO:

    apply 'f5' to self.
  
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

on 'leave':U of tt-saldo.qt-transf in browse br-it-selec do:

    if avail tt-saldo then do:
        if input browse br-it-selec tt-saldo.qt-transf = 0 then do:            
    
            run utp/ut-msgs.p (input 'show',
                               input 17006,
                               input 'Quantidade Transferància n∆o pode ser igual a 0!').

            assign tt-saldo.qt-transf:screen-value in browse br-it-selec = string(tt-saldo.qt-saldo).

            return no-apply.
    
        end.

        if input browse br-it-selec tt-saldo.qt-transf > tt-saldo.qt-saldo then do:            
    
            run utp/ut-msgs.p (input 'show',
                               input 17006,
                               input 'Quantidade Transferància n∆o pode ser maior que o Saldo!').

            assign tt-saldo.qt-transf:screen-value in browse br-it-selec = string(tt-saldo.qt-saldo).

            return no-apply.
    
        end.
    end.
    
end.

c-est-orig:load-mouse-pointer("image/lupa.cur").
c-est-dest:load-mouse-pointer("image/lupa.cur").
c-cfop-saida:load-mouse-pointer("image/lupa.cur").
c-cfop-ent:load-mouse-pointer("image/lupa.cur").
c-serie:load-mouse-pointer("image/lupa.cur").
c-cod-depos:load-mouse-pointer("image/lupa.cur").

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
  DISPLAY c-est-orig c-cfop-saida c-est-dest c-cfop-ent c-serie c-cod-depos 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button RECT-1 bt-calc br-it-selec bt-marca bt-todos bt-nenhum 
         br-it-transf c-est-orig c-cfop-saida c-est-dest c-cfop-ent c-serie 
         c-cod-depos bt-carrega bt-inf-comp bt-refresh bt-selec 
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

  {utp/ut9000.i "ESFT104" "12.01.00.001"}

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
               tt-saldo.qt-transf = tt-saldo.qt-saldo.

    end.
end.

{&open-query-br-it-selec}


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-transf w-livre 
PROCEDURE pi-carrega-transf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

empty temp-table tt-transf.

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

        create tt-transf.
        buffer-copy saldo-estoq to tt-transf.
        assign tt-transf.desc-item = tt-saldo.desc-item
               tt-transf.qt-saldo  = tt-saldo.qt-saldo
               tt-transf.qt-transf = tt-saldo.qt-transf.

    end.
end.

{&open-query-br-it-transf}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-item-entr-st w-livre 
PROCEDURE pi-cria-item-entr-st :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    if docum-est.cod-observa = 1 then do:
    
        if item-doc-est.base-subs[1] = 0 then do:           
            run utp/ut-msgs.p ( input "show",
                                input 17006,
                                input "Valor Base ST Antec deve ser diferente de zero" + "~~" + "Valor Base ST Antec do item " + string(item-doc-est.it-codigo) + " deve ser diferente de zero"). 
            return "NOK". 
        end.
        
        IF item-doc-est.vl-subs[1] = 0 then do:
            run utp/ut-msgs.p ( input "show",
                                input 17006,
                                input "Valor ST Antec deve ser diferente de zero" + "~~" + "Valor ST Antec do item " + string(item-doc-est.it-codigo) + " deve ser diferente de zero"). 
    
            return "NOK".
        end.
    
        if natur-oper.tipo-compra = 1 then do:
            if not can-find (first item-entr-st where 
                                   item-entr-st.cod-estab        = docum-est.cod-estabel          and   
                                   item-entr-st.cod-serie        = docum-est.serie-docto          and   
                                   item-entr-st.cod-docto        = docum-est.nro-docto            and   
                                   item-entr-st.cod-natur-operac = docum-est.nat-operacao         and   
                                   item-entr-st.cod-emitente     = string(docum-est.cod-emitente) and   
                                   item-entr-st.cod-item         = item-doc-est.it-codigo         and   
                                   item-entr-st.num-seq          = item-doc-est.sequencia )       then do:
                create item-entr-st.
                assign item-entr-st.cod-estab           = docum-est.cod-estabel         
                       item-entr-st.cod-serie           = docum-est.serie-docto         
                       item-entr-st.cod-docto           = docum-est.nro-docto           
                       item-entr-st.cod-natur-operac    = docum-est.nat-operacao        
                       item-entr-st.cod-emitente        = string(docum-est.cod-emitente)
                       item-entr-st.cod-item            = item-doc-est.it-codigo        
                       item-entr-st.num-seq             = item-doc-est.sequencia
                       item-entr-st.qtd-sdo-final       = item-doc-est.quantidade
                       item-entr-st.dat-movto           = docum-est.dt-trans 
                       item-entr-st.val-livre-1         = item-doc-est.quantidade
                       item-entr-st.val-base-calc-impto = item-doc-est.base-subs[1]
                       item-entr-st.val-impto           = item-doc-est.vl-subs[1]
                       item-entr-st.log-finaliz         = yes.
    
                create tt-item-entr-st.
                buffer-copy item-entr-st to tt-item-entr-st.
            end.
        end.
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-transf w-livre 
PROCEDURE pi-gera-transf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    empty temp-table tt-notas-geradas.
    empty temp-table tt-erro.

    find estabelec no-lock where
         estabelec.cod-estabel = input frame {&frame-name} c-est-dest no-error.

    find emitente no-lock where
         emitente.cod-emitente = estabelec.cod-emitente no-error.
    if not avail emitente then do:
        create tt-erro.
        assign tt-erro.cd-erro  = 888
               tt-erro.mensagem = "Cliente NFT n∆o existe":U.
        return "NOK".
    end.

    assign c-cod-estabel  = input frame {&frame-name} c-est-orig
           c-serie-bo     = input frame {&frame-name} c-serie
           c-nome-abrev   = emitente.nome-abrev
           c-nat-operacao = input frame {&frame-name} c-cfop-saida.

    if not valid-handle(h-bodi317in) then do:
        run dibo/bodi317in.p persistent set h-bodi317in.
        run inicializaBOS in h-bodi317in(output h-bodi317pr,
                                         output h-bodi317sd,
                                         output h-bodi317im1bra,
                                         output h-bodi317va).
    end.

    run criaWtDocto in h-bodi317sd (input  c-seg-usuario,
                                    input  c-cod-estabel,
                                    input  c-serie-bo,
                                    input  "1",
                                    input  c-nome-abrev,
                                    input  ?,
                                    input  1, /* Sistema - nota-fiscal.ind-tip-nota */ 
                                    input  4003,
                                    input  today,
                                    input  0,
                                    input  c-nat-operacao,
                                    input  0,
                                    output i-seq-wt-docto,
                                    output l-proc-ok-aux).

    run devolveErrosbodi317sd in h-bodi317sd(output c-ultimo-metodo-exec,
                                             output table RowErrors).

    for each RowErrors
        where RowErrors.ErrorSubtype = "Error":
        create tt-erro.
        assign tt-erro.cd-erro     = RowErrors.ErrorNumber
               tt-erro.mensagem    = RowErrors.ErrorDescription.
    end.

    if not l-proc-ok-aux then
        return "NOK".

    /* Bloco a ser repetido para cada item da nota */
    /*bloco-cria-item:*/
    assign i-seq-nft = 0.

    for each tt-transf where
             tt-transf.qt-transf > 0:

        find first preco-item no-lock where
                   preco-item.nr-tabpre = emitente.nr-tabpre  and
                   preco-item.it-codigo = tt-transf.it-codigo no-error.

        if not avail preco-item then do:
            create tt-erro.
            assign tt-erro.cd-erro  = 888
                   tt-erro.mensagem = "Item " + string(tt-transf.it-codigo) + " sem tab preco".
            return "NOK".
        end.

        assign de-vl-preori-ped = preco-item.preco-venda.

        find item no-lock where
             item.it-codigo = tt-transf.it-codigo no-error.

        assign i-seq-nft = i-seq-nft + 10.

        /* Limpar a tabela de erros em todas as BOS */
        run emptyRowErrors in h-bodi317in.

        /* Disponibilizar o registro WT-DOCTO na bodi317sd */
        run localizaWtDocto in h-bodi317sd(input  i-seq-wt-docto,
                                           output l-proc-ok-aux).

        /* Cria item para nota fiscal. */
        run criaWtItDocto in h-bodi317sd  (input  ?,
                                           input  "",
                                           input  i-seq-nft,
                                           input  item.it-codigo,
                                           input  item.cod-refer,
                                           input  c-nat-operacao,
                                           output i-seq-wt-it-docto,
                                           output l-proc-ok-aux).

        /* Busca poss°veis erros que ocorreram nas validaá‰es */
        run devolveErrosbodi317sd in h-bodi317sd(output c-ultimo-metodo-exec,
                                                 output table RowErrors).

        /* Pesquisa algum erro ou advertància que tenha ocorrido */
        for each RowErrors where
                 RowErrors.ErrorSubtype = "Error":
            assign l-proc-ok-aux = no.
            create tt-erro.
            assign tt-erro.cd-erro     = RowErrors.ErrorNumber
                   tt-erro.mensagem    = RowErrors.ErrorDescription.
        end.

        /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
        if not l-proc-ok-aux then
            return "NOK".

        /*--- qdo nao tem preco medio, valoriza a 1 real ---*/
        if  de-vl-preori-ped <= 0 then
            assign de-vl-preori-ped = 1.

        /*************TESTE NOVO ***************************/
        &if defined (bf_dis_preco_un_med_mult) &then
            run emptyRowErrors in h-bodi317in.

            run WriteUomQuantity        in h-bodi317sd (input  i-seq-wt-docto,
                                                        input  i-seq-wt-it-docto,
                                                        input  tt-transf.qt-transf,
                                                        input  item.un,
                                                        output de-quantidade-aux,
                                                        output l-proc-ok-aux).

            run devolveErrosbodi317sd   in h-bodi317sd (output c-ultimo-metodo-exec,
                                                        output table RowErrors).

            /* Pesquisa algum erro ou advertància que tenha ocorrido */
            for each RowErrors where
                     RowErrors.ErrorSubtype = "Error":
                assign l-proc-ok-aux = no.
                create tt-erro.
                assign tt-erro.cd-erro     = RowErrors.ErrorNumber
                       tt-erro.mensagem    = RowErrors.ErrorDescription.
            end.

            /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
            if not l-proc-ok-aux then
                return "NOK".

            run gravaInfGeraisWtItDocto in h-bodi317sd (input i-seq-wt-docto,
                                                        input i-seq-wt-it-docto,
                                                        input tt-transf.qt-transf,
                                                        input de-vl-preori-ped,
                                                        input 0,
                                                        input 0).
        &else
            run gravaInfGeraisWtItDocto in h-bodi317sd (input i-seq-wt-docto,
                                                        input i-seq-wt-it-docto,
                                                        input tt-transf.qt-transf,
                                                        input de-vl-preori-ped,
                                                        input 0,
                                                        input 0).
        &endif
        /*************FIM TESTE NOVO************************/

        /* Grava informaá‰es gerais para o item da nota */
        run gravaInfGeraisWtItDocto IN h-bodi317sd (input i-seq-wt-docto,
                                                    input i-seq-wt-it-docto,
                                                    input tt-transf.qt-transf,
                                                    input de-vl-preori-ped,
                                                    input 0,
                                                    input 0).

        /* Limpar a tabela de erros em todas as BOS */
        run emptyRowErrors        IN h-bodi317in.

        /* Disp. registro WT-DOCTO, WT-IT-DOCTO e WT-IT-IMPOSTO na bodi317pr */
        run localizaWtDocto       IN h-bodi317pr(input  i-seq-wt-docto,
                                                 output l-proc-ok-aux).
        run localizaWtItDocto     IN h-bodi317pr(input  i-seq-wt-docto,
                                                 input  i-seq-wt-it-docto,
                                                 output l-proc-ok-aux).
        run localizaWtItImposto   IN h-bodi317pr(input  i-seq-wt-docto,
                                                 input  i-seq-wt-it-docto,
                                                 output l-proc-ok-aux).

        /* Atualiza dados c†lculados do item */
        run atualizaDadosItemNota IN h-bodi317pr(output l-proc-ok-aux).

        assign l-eliminou-lote = no.

        find wt-it-docto no-lock where
             wt-it-docto.seq-wt-it-docto = i-seq-wt-it-docto and
             wt-it-docto.seq-wt-docto    = i-seq-wt-docto    no-error.
        if avail wt-it-docto then do:
            for each wt-fat-ser-lote exclusive-lock where
                     wt-fat-ser-lote.seq-wt-it-docto = wt-it-docto.seq-wt-it-docto and
                     wt-fat-ser-lote.seq-wt-docto    = wt-it-docto.seq-wt-docto    and
                     wt-fat-ser-lote.it-codigo       = wt-it-docto.it-codigo:
                delete wt-fat-ser-lote.
                assign l-eliminou-lote = yes.
            end.

            if l-eliminou-lote then do:
                find saldo-estoq no-lock where
                     saldo-estoq.cod-estabel = c-cod-estabel         and
                     saldo-estoq.cod-depos   = tt-transf.cod-depos   and
                     saldo-estoq.cod-localiz = tt-transf.cod-localiz and
                     saldo-estoq.it-codigo   = tt-transf.it-codigo   and
                     saldo-estoq.cod-refer   = ""                    and
                     saldo-estoq.lote        = tt-transf.lote        no-error.
                if not avail saldo-estoq or saldo-estoq.qtidade-atu - (saldo-estoq.qt-alocada  +
                                                           saldo-estoq.qt-aloc-ped +
                                                           saldo-estoq.qt-aloc-prod) < tt-transf.qt-transf then do:
                    create tt-erro.
                    assign tt-erro.cd-erro  = 17006
                           tt-erro.mensagem = "Saldo insuficiente : Item " + tt-transf.it-codigo + " , lote: " + tt-transf.lote + ", Qt:" + string(tt-transf.qt-transf).
                    return "NOK".
                end.

                find wt-fat-ser-lote exclusive-lock where 
                     wt-fat-ser-lote.seq-wt-docto    = wt-it-docto.seq-wt-docto    and 
                     wt-fat-ser-lote.seq-wt-it-docto = wt-it-docto.seq-wt-it-docto and 
                     wt-fat-ser-lote.it-codigo       = wt-it-docto.it-codigo       and 
                     wt-fat-ser-lote.cod-depos       = tt-transf.cod-depos         and 
                     wt-fat-ser-lote.cod-localiz     = tt-transf.cod-localiz       and 
                     wt-fat-ser-lote.lote            = tt-transf.lote              and 
                     wt-fat-ser-lote.cod-refer       = "" no-error.
                if not avail wt-fat-ser-lote then do:
                    create wt-fat-ser-lote.
                    assign wt-fat-ser-lote.seq-wt-docto    = wt-it-docto.seq-wt-docto
                           wt-fat-ser-lote.seq-wt-it-docto = wt-it-docto.seq-wt-it-docto
                           wt-fat-ser-lote.it-codigo       = wt-it-docto.it-codigo
                           wt-fat-ser-lote.cod-depos       = tt-transf.cod-depos
                           wt-fat-ser-lote.cod-localiz     = tt-transf.cod-localiz
                           wt-fat-ser-lote.lote            = tt-transf.lote
                           wt-fat-ser-lote.quantidade[1]   = tt-transf.qt-transf
                           wt-fat-ser-lote.qtd-contada[1]  = tt-transf.qt-transf
                           wt-fat-ser-lote.dt-vali-lote    = saldo-estoq.dt-vali-lote 
                           wt-fat-ser-lote.cod-refer       = "".
                end.
                else
                    assign wt-fat-ser-lote.quantidade[1]   = wt-fat-ser-lote.quantidade[1] + tt-transf.qt-transf
                           wt-fat-ser-lote.qtd-contada[1]  = wt-fat-ser-lote.qtd-contada[1] + tt-transf.qt-transf.

            end.
        end.

        /* Busca poss°veis erros que ocorreram nas validaá‰es */
        run devolveErrosbodi317pr in h-bodi317pr(output c-ultimo-metodo-exec,
                                                 output table RowErrors).

        for each RowErrors where
                 RowErrors.ErrorSubtype = "Error":
            assign l-proc-ok-aux = no.
            create tt-erro.
            assign tt-erro.cd-erro  = RowErrors.ErrorNumber
                   tt-erro.mensagem = RowErrors.ErrorDescription.
        end.

        /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
        if not l-proc-ok-aux then
            return "NOK".

        /* Limpar a tabela de erros em todas as BOS */
        run emptyRowErrors in h-bodi317in.
    
        /* Valida informaá‰es do item */
        run validaItemDaNota in h-bodi317va(input  i-seq-wt-docto,
                                            input  i-seq-wt-it-docto,
                                            output l-proc-ok-aux).
        
        /* Busca poss°veis erros que ocorreram nas validaá‰es */
        run devolveErrosbodi317va IN h-bodi317va(output c-ultimo-metodo-exec,
                                                 output table RowErrors).
        
        for each RowErrors where 
                 RowErrors.ErrorSubtype = "Error":
            assign l-proc-ok-aux = no.
            create tt-erro.
            assign tt-erro.cd-erro     = RowErrors.ErrorNumber
                   tt-erro.mensagem    = RowErrors.ErrorDescription.
        end.
    
        /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
        if not l-proc-ok-aux then 
            return "NOK".
    end.

    /**** Gera Fatura Para representante ****/
    run geraWtFatRepre in h-bodi317sd(input i-seq-wt-docto,
                                      output l-proc-ok-aux).
             
    /* Finalizaá∆o das BOS utilizada no c†lculo */
    run finalizaBOS in h-bodi317in.
    
    /* Reinicializaá∆o das BOS para C†lculo */
    run dibo/bodi317in.p persistent set h-bodi317in.
    run inicializaBOS in h-bodi317in(output h-bodi317pr,
                                     output h-bodi317sd,     
                                     output h-bodi317im1bra,
                                     output h-bodi317va).
    
    /* Limpar a tabela de erros em todas as BOS */
    run emptyRowErrors        in h-bodi317in.
    
    /* Calcula o pedido, com acompanhamento */
    run inicializaAcompanhamento in h-bodi317pr.
    run confirmaCalculo          in h-bodi317pr(input  i-seq-wt-docto,
                                                output l-proc-ok-aux).
    run finalizaAcompanhamento   in h-bodi317pr.
    
    /* Busca poss°veis erros que ocorreram nas validaá‰es */
    run devolveErrosbodi317pr    in h-bodi317pr (output c-ultimo-metodo-exec,
                                                 output table RowErrors).
    
    for each RowErrors where 
             RowErrors.ErrorSubtype = "Error":
        assign l-proc-ok-aux = no.
        create tt-erro.
        assign tt-erro.cd-erro     = RowErrors.ErrorNumber
               tt-erro.mensagem    = RowErrors.ErrorDescription.
    end.
    
    /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
    if not l-proc-ok-aux then
        return "NOK".
        
    /* Efetiva os pedidos e cria a nota */
    run dibo/bodi317ef.p persistent set h-bodi317ef.
    run emptyRowErrors           in h-bodi317in.
    run inicializaAcompanhamento in h-bodi317ef.

    run setaHandlesBOS           in h-bodi317ef(h-bodi317pr,     
                                                h-bodi317sd, 
                                                h-bodi317im1bra, 
                                                h-bodi317va).
    run efetivaNota              in h-bodi317ef(input  i-seq-wt-docto,
                                                input  yes,
                                                output l-proc-ok-aux).
    run finalizaAcompanhamento   in h-bodi317ef.
    
    /* Busca poss°veis erros que ocorreram nas validaá‰es */
    run devolveErrosbodi317ef    in h-bodi317ef(output c-ultimo-metodo-exec,
                                                output table RowErrors).
    
    for each RowErrors where 
             RowErrors.ErrorSubtype = "Error":
        assign l-proc-ok-aux = no.
        create tt-erro.
        assign tt-erro.cd-erro     = RowErrors.ErrorNumber
               tt-erro.mensagem    = RowErrors.ErrorDescription.
    end.

    /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
    if not l-proc-ok-aux then do:
        delete procedure h-bodi317ef.
        return "NOK".
    end.

    /* Busca as notas fiscais geradas */
    run buscaTTNotasGeradas in h-bodi317ef(output l-proc-ok-aux,
                                           output table tt-notas-geradas).

    for each tt-notas-geradas:

        find nota-fiscal exclusive-lock where 
             rowid(nota-fiscal) = tt-notas-geradas.rw-nota-fiscal no-error.
        if avail nota-fiscal then
            assign nota-fiscal.observ-nota = nota-fiscal.observ-nota + "  " + c-des-inf-compl.

    end.
    
    /* Elimina o handle do programa bodi317ef */
    if valid-handle(h-bodi317ef) then
        delete procedure h-bodi317ef.
    
    /* Finalizaá∆o das BOS utilizada no c†lculo */
    run finalizaBOS in h-bodi317in.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-recebe-nft w-livre 
PROCEDURE pi-recebe-nft :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

empty temp-table tt-it-nota-fisc.
empty temp-table tt-item-entr-st.

BLOCO-A:
do transaction on error undo, retry:
    
    for each tt-notas-geradas:
    
        empty temp-table rowErrors.
        empty temp-table tt-docum-est.
        
        find nota-fiscal no-lock where 
             rowid(nota-fiscal) = tt-notas-geradas.rw-nota-fiscal no-error.
        if not avail nota-fiscal then do:
            run utp/ut-msgs.p(input "show",
                              input 17006,
                              input "Nota Fiscal n∆o encontrada!~~":U + 
                                    "nota Fiscal selecionada n∆o foi encontrada!":U).
            return "ADM-ERROR":U.                                                        
        end.
        
        for each it-nota-fisc of nota-fiscal no-lock:
            create tt-it-nota-fisc.
            buffer-copy it-nota-fisc to tt-it-nota-fisc.
        end.
    
        find estabelec no-lock where 
             estabelec.cod-estabel = input frame {&frame-name} c-est-dest no-error.
        
        find b-estabelec no-lock where 
             b-estabelec.cod-estabel = nota-fiscal.cod-estabel no-error.
        
        find natur-oper no-lock where 
             natur-oper.nat-operacao = nota-fiscal.nat-operacao no-error.
            
        find emitente no-lock where 
             emitente.cod-emitente = b-estabelec.cod-emitente no-error.   
        
        create tt-docum-est.
        assign tt-docum-est.cod-emitente = b-estabelec.cod-emitente
               tt-docum-est.serie-docto  = nota-fiscal.serie
               tt-docum-est.nat-operacao = input frame {&frame-name} c-cfop-ent
               tt-docum-est.nro-docto    = nota-fiscal.nr-nota-fis
               tt-docum-est.cod-observa  = 1 /* industria */
               tt-docum-est.cod-estabel  = estabelec.cod-estabel
               tt-docum-est.dt-emissao   = nota-fiscal.dt-emis-nota
               tt-docum-est.dt-trans     = today
               tt-docum-est.usuario      = c-seg-usuario
               tt-docum-est.base-iss     = 0 
               tt-docum-est.iss-deb-cre  = 0     
               tt-docum-est.base-subs    = 0    
               tt-docum-est.vl-subs      = 0    
               tt-docum-est.icm-complem  = 0    
               tt-docum-est.icm-fonte    = 0    
               tt-docum-est.tot-peso     = 0    
               tt-docum-est.tot-desconto = 0    
               tt-docum-est.despesa-nota = 0    
               tt-docum-est.valor-mercad = 0    
               tt-docum-est.base-ipi     = 0    
               tt-docum-est.ipi-deb-cre  = 0    
               tt-docum-est.base-icm     = 0    
               tt-docum-est.icm-deb-cre  = 0
               tt-docum-est.tot-valor    = 0
               tt-docum-est.rec-fisico   = no
               tt-docum-est.log-1        = no
               tt-docum-est.ce-atual     = no
               tt-docum-est.of-atual     = no
               tt-docum-est.ap-atual     = no
               tt-docum-est.cr-atual     = no
               tt-docum-est.cp-atual     = no
               /*tt-docum-est.cod-chave-aces-nf-eletro = nota-fiscal.cod-chave-aces-nf-eletro*/.
        
        run inbo/boin090.p persistent set h-boin090.
           
        run openQueryStatic in h-boin090("main").
            
        run setRecord in h-boin090(input TABLE tt-docum-est).
            
        run emptyRowErrors in h-boin090.
            
        run inbo/boin176.p persistent set h-boin176.
            
        run setConstraintOfDocumEst in h-boin176(input tt-docum-est.cod-emitente,
                                                 input tt-docum-est.serie-docto,
                                                 input tt-docum-est.nro-docto,
                                                 input tt-docum-est.nat-operacao).
            
        run openQueryStatic in h-boin176("ofDocumEst").
        
        run getHandleBOItemDoc in h-boin090(h-boin176).
            
        run createRecord in h-boin090.
        
        run getRowerrors in h-boin090(output table rowErrors).
        
        if can-find(first rowErrors where
                          RowErrors.ErrorSubType = "ERROR") then do:
            {method/showmessage.i1}
            {method/showmessage.i2}
            undo BLOCO-A,leave.
        end.
         
/*         find first docum-est exclusive-lock where                                                           */
/*                    docum-est.serie-docto  = tt-docum-est.serie-docto  and                                   */
/*                    docum-est.nro-docto    = tt-docum-est.nro-docto    and                                   */
/*                    docum-est.cod-emitente = tt-docum-est.cod-emitente and                                   */
/*                    docum-est.nat-operacao = tt-docum-est.nat-operacao no-error.                             */
/*         if avail docum-est then do:                                                                         */
/*                                                                                                             */
/*             find natur-oper no-lock where                                                                   */
/*                  natur-oper.nat-operacao = docum-est.nat-operacao no-error.                                 */
/*                                                                                                             */
/*             for each item-doc-est of docum-est exclusive-lock:                                              */
/*                 for each rat-lote no-lock where                                                             */
/*                          rat-lote.serie-docto  = item-doc-est.serie-docto  and                              */
/*                          rat-lote.nro-docto    = item-doc-est.nro-docto    and                              */
/*                          rat-lote.cod-emitente = item-doc-est.cod-emitente and                              */
/*                          rat-lote.nat-operacao = item-doc-est.nat-operacao and                              */
/*                          rat-lote.sequencia    = item-doc-est.sequencia:                                    */
/*                     create tt-rat-lote-esp.                                                                 */
/*                     buffer-copy rat-lote to tt-rat-lote-esp.                                                */
/*                 end.                                                                                        */
/*                                                                                                             */
/*                 if natur-oper.log-icms-substto-antecip and                                                  */
/*                    natur-oper.log-contrib-st-antec     then do:                                             */
/*                     find first it-nota-fisc no-lock where                                                   */
/*                                it-nota-fisc.cod-estabel = nota-fiscal.cod-estabel and                       */
/*                                it-nota-fisc.serie       = nota-fiscal.serie       and                       */
/*                                it-nota-fisc.nr-nota-fis = nota-fiscal.nr-nota-fis and                       */
/*                                it-nota-fisc.it-codigo   = item-doc-est.it-codigo  no-error.                 */
/*                     if avail it-nota-fisc then do:                                                          */
/*                         assign item-doc-est.base-subs[1] = it-nota-fisc.vl-bsubs-it                         */
/*                                item-doc-est.vl-subs[1]   = it-nota-fisc.vl-icmsub-it                        */
/*                                docum-est.tot-valor       = docum-est.tot-valor + it-nota-fisc.vl-icmsub-it. */
/*                                                                                                             */
/*                         run pi-cria-item-entr-st.                                                           */
/*                                                                                                             */
/*                         if return-value = "NOK" then                                                        */
/*                            undo BLOCO-A,leave.                                                              */
/*                     end.                                                                                    */
/*                 end.                                                                                        */
/*             end.                                                                                            */
/*         end.                                                                                                */
        
        run atualizaDocumento in h-boin090.
        
        if return-value = "NOK" then do:
            run utp/ut-msgs.p (input 'show',
                                   input 17006,
                                   input 'Erro Recebimento Documento!~~Verificar RE1001').
            undo BLOCO-A,leave. 
        end.
        else do:
            if can-find(docum-est where
                        docum-est.serie-docto  = tt-docum-est.serie-docto   and
                        docum-est.nro-docto    = tt-docum-est.nro-docto     and
                        docum-est.cod-emitente = tt-docum-est.cod-emitente  and
                        docum-est.nat-operacao = tt-docum-est.nat-operacao) then do:

                run utp/ut-msgs.p (input 'show',
                                   input 15825,
                                   input 'Documento Gerado no Recebimento:'         + '~~' +
                                         'Estab     - ' + tt-docum-est.cod-estabel  + chr(10) +
                                         'Serie     - ' + tt-docum-est.serie        + chr(10) +
                                         'Documento - ' + tt-docum-est.nro-docto    + chr(10) +
                                         'Natureza  - ' + tt-docum-est.nat-operacao).
                
            end.
            else do:
                run utp/ut-msgs.p (input 'show',
                                   input 17006,
                                   input 'Erro Recebimento Documento!~~Verificar RE1001').
            end.
        end.
    end.
end.

delete procedure h-boin176.
delete procedure h-boin090.


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
  {src/adm/template/snd-list.i "tt-transf"}
  {src/adm/template/snd-list.i "tt-saldo"}

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

