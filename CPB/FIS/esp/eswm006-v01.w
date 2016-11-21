&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgcpb            PROGRESS
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
{include/i-prgvrs.i eswm006-v01 12.06.00.001}

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
def var v-row-parent       as rowid  no-undo.
def var v_num_tip_cta      as int    no-undo.
def var v_num_sit_cta      as int    no-undo.
def var h_api_cta          as handle no-undo.
def var v_cod_format_cta   as char   no-undo.
def var v_cod_cta          as char   no-undo.
def var v_des_cta          as char   no-undo.
def var v_ind_finalid_cta  as char   no-undo.

def new global shared var v_cdn_empres_usuar   like mgcad.empresa.ep-codigo no-undo.


def temp-table tt_log_erro no-undo
    field ttv_num_cod_erro  as integer format ">>>>,>>9" label "Numero" column-label "Numero"
    field ttv_des_msg_ajuda as character format "x(40)" label "Mensagem Ajuda" column-label "Mensagem Ajuda"
    field ttv_des_msg_erro  as character format "x(60)" label "Mensagem Erro" column-label "Inconsist¼ncia".


find first param-global no-lock no-error.

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
&Scoped-define EXTERNAL-TABLES esp-conta-frete
&Scoped-define FIRST-EXTERNAL-TABLE esp-conta-frete


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR esp-conta-frete.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS esp-conta-frete.cod-canal-venda ~
esp-conta-frete.conta-contabil 
&Scoped-define ENABLED-TABLES esp-conta-frete
&Scoped-define FIRST-ENABLED-TABLE esp-conta-frete
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS esp-conta-frete.cod-canal-venda ~
esp-conta-frete.conta-contabil 
&Scoped-define DISPLAYED-TABLES esp-conta-frete
&Scoped-define FIRST-DISPLAYED-TABLE esp-conta-frete
&Scoped-Define DISPLAYED-OBJECTS c-desc-canal-venda c-des-conta-frete 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS esp-conta-frete.cod-canal-venda 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
cod-canal-venda|y|y|mgcpb.esp-conta-frete.cod-canal-venda
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "cod-canal-venda",
     Keys-Supplied = "cod-canal-venda"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE c-des-conta-frete AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 33.72 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-canal-venda AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 1.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 1.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     esp-conta-frete.cod-canal-venda AT ROW 1.17 COL 22 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 4.57 BY .88
     c-desc-canal-venda AT ROW 1.17 COL 26.72 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     esp-conta-frete.conta-contabil AT ROW 2.75 COL 22 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 18.14 BY .88
     c-des-conta-frete AT ROW 2.75 COL 40.29 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 2.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgcpb.esp-conta-frete
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
         HEIGHT             = 3.08
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

/* SETTINGS FOR FILL-IN c-des-conta-frete IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc-canal-venda IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN esp-conta-frete.cod-canal-venda IN FRAME f-main
   1                                                                    */
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

&Scoped-define SELF-NAME esp-conta-frete.cod-canal-venda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL esp-conta-frete.cod-canal-venda V-table-Win
ON F5 OF esp-conta-frete.cod-canal-venda IN FRAME f-main /* Canal Venda */
DO:
    assign l-implanta = yes.
   {include/zoomvar.i &prog-zoom="dizoom/z01di232.w"
                      &campo=esp-conta-frete.cod-canal-venda
                      &campozoom=cod-canal-venda
                      &campo2=c-desc-canal-venda
                      &campozoom2=descricao}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL esp-conta-frete.cod-canal-venda V-table-Win
ON LEAVE OF esp-conta-frete.cod-canal-venda IN FRAME f-main /* Canal Venda */
DO:
    {include/leave.i &tabela=canal-venda
                   &atributo-ref=descricao
                   &variavel-ref=c-desc-canal-venda
                   &where="canal-venda.cod-canal-venda = input frame {&frame-name} esp-conta-frete.cod-canal-venda"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL esp-conta-frete.cod-canal-venda V-table-Win
ON MOUSE-SELECT-DBLCLICK OF esp-conta-frete.cod-canal-venda IN FRAME f-main /* Canal Venda */
DO:

    apply 'f5' to self.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME esp-conta-frete.conta-contabil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL esp-conta-frete.conta-contabil V-table-Win
ON F5 OF esp-conta-frete.conta-contabil IN FRAME f-main /* Conta */
DO:

    def var v_cod_format_cta   as char   no-undo.
    def var v_cod_cta          as char   no-undo.
    def var v_des_cta          as char   no-undo.
    def var v_ind_finalid_cta  as char   no-undo.
        
    empty temp-table tt_log_erro.
    
    if not valid-handle(h_api_cta) then
        run prgint\utb\utb743za.py persistent set h_api_cta.

    run pi_retorna_formato_cta_ctbl in h_api_cta (input param-global.empresa-prin,              /* EMPRESA EMS2 */
                                                  input  "",                    /* PLANO CONTAS */
                                                  input  today,                 /* DATA DE TRANSACAO */
                                                  output v_cod_format_cta,      /* FORMATO cta */
                                                  output table tt_log_erro).    /* ERROS */

    run pi_zoom_cta_ctbl_integr in h_api_cta (input  param-global.empresa-prin,          /* EMPRESA EMS2 */
                                              input  "FTP",              /* M…DULO */         
                                              input  "",                 /* PLANO DE CONTAS */
                                              input  "(nenhum)",         /* FINALIDADES */    
                                              input  today,              /* DATA TRANSACAO */
                                              output v_cod_cta,          /* CODIGO CONTA */   
                                              output v_des_cta,          /* DESCRICAO CONTA */
                                              output v_ind_finalid_cta,  /* FINALIDADE DA CONTA */
                                              output table tt_log_erro). /* ERROS */ 
   if can-find(tt_log_erro) or 
      return-value = "NOK"  then do:
       for first tt_log_erro:
           run utp/ut-msgs.p (input 'show',
                              17006, 
                              string(tt_log_erro.ttv_des_msg_erro) + ' ('  + string(tt_log_erro.ttv_num_cod_erro) + ')' + "~~" + tt_log_erro.ttv_des_msg_ajuda).
           return no-apply.
       end.
   end.
   else do:
       
       assign esp-conta-frete.conta-contabil:screen-value in frame {&frame-name} = v_cod_cta
              c-des-conta-frete:screen-value in frame {&frame-name}              = v_des_cta.
              
       if v_cod_format_cta <> '' then
          assign esp-conta-frete.conta-contabil:format in frame {&frame-name} = v_cod_format_cta.
      
   end.

   delete procedure h_api_cta.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL esp-conta-frete.conta-contabil V-table-Win
ON LEAVE OF esp-conta-frete.conta-contabil IN FRAME f-main /* Conta */
DO:

    empty temp-table tt_log_erro.

    if not valid-handle(h_api_cta) then
        run prgint\utb\utb743za.py persistent set h_api_cta.
        
    run pi_retorna_formato_cta_ctbl in h_api_cta (input param-global.empresa-prin,              /* EMPRESA EMS2 */
                                                  input  "",                    /* PLANO CONTAS */
                                                  input  today,                 /* DATA DE TRANSACAO */
                                                  output v_cod_format_cta,      /* FORMATO cta */
                                                  output table tt_log_erro).    /* ERROS */    

    assign v_cod_cta = input frame {&frame-name} esp-conta-frete.conta-contabil.

    if v_cod_cta = "" then return.

    empty temp-table tt_log_erro.
    
    run pi_busca_dados_cta_ctbl in h_api_cta (input        param-global.empresa-prin, /* EMPRESA EMS2 */
                                              input        "",                /* PLANO DE CONTAS */
                                              input-output v_cod_cta,         /* CONTA */
                                              input        today,             /* DATA TRANSACAO */   
                                              output       v_des_cta,         /* DESCRICAO CONTA */
                                              output       v_num_tip_cta,     /* TIPO DA CONTA */
                                              output       v_num_sit_cta,     /* SITUA°€O DA CONTA */
                                              output       v_ind_finalid_cta, /* FINALIDADES DA CONTA */
                                              output table tt_log_erro). 
                                              
    assign esp-conta-frete.conta-contabil:screen-value in frame {&frame-name} = v_cod_cta           
           c-des-conta-frete:screen-value              in frame {&frame-name} = v_des_cta.
           
    if v_cod_format_cta <> '' then
        assign esp-conta-frete.conta-contabil:format in frame {&frame-name} = v_cod_format_cta.

    delete procedure h_api_cta.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL esp-conta-frete.conta-contabil V-table-Win
ON MOUSE-SELECT-DBLCLICK OF esp-conta-frete.conta-contabil IN FRAME f-main /* Conta */
DO:

    apply 'f5' to self.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

esp-conta-frete.cod-canal-venda:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
esp-conta-frete.conta-contabil:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.

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
    WHEN 'cod-canal-venda':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = esp-conta-frete
           &WHERE = "WHERE esp-conta-frete.cod-canal-venda eq INTEGER(key-value)"
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
  {src/adm/template/row-list.i "esp-conta-frete"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "esp-conta-frete"}

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
    /*{include/i-valid.i}*/
    if  not frame {&frame-name}:validate() then
        return 'ADM-ERROR':U.

    if adm-new-record then do:
        find canal-venda no-lock where 
             canal-venda.cod-canal-venda = input frame {&frame-name} esp-conta-frete.cod-canal-venda no-error.
        if not avail canal-venda then do:
            run utp/ut-msgs.p (input "show":U, 
                               input 2, 
                               input "Canal Venda").
            apply "entry" to esp-conta-frete.cod-canal-venda in frame {&frame-name}.
            undo, return "adm-error". 
        end.

        if esp-conta-frete.cod-canal-venda:screen-value = "" then do:
            run utp/ut-msgs.p(input 'show':u,
                              input 164,
                              input "Canal Venda").
            return 'ADM-ERROR':U.
        end.

        find esp-conta-frete no-lock where 
             esp-conta-frete.cod-canal-venda = input frame {&frame-name} esp-conta-frete.cod-canal-venda no-error.
        if avail esp-conta-frete then do:
           run utp/ut-msgs.p (input "show":U, 
                              input 1, 
                              input "Canal Venda").
           apply "entry" to esp-conta-frete.cod-canal-venda in frame {&frame-name}.
           undo, return "adm-error". 
        end.
    end.

    if input frame {&frame-name} esp-conta-frete.conta-contabil <> "" then do:
        if not valid-handle(h_api_cta) then
            run prgint\utb\utb743za.py persistent set h_api_cta.
    
        assign v_cod_cta = input frame {&frame-name} esp-conta-frete.conta-contabil.
        
        empty temp-table tt_log_erro.

        run pi_valida_cta_ctbl_integr in h_api_cta (input  param-global.empresa-prin, /* EMPRESA EMS2 */
                                                   input  "FTP",               /* MODULO */
                                                   input  "",                  /* PLANO CONTAS */ 
                                                   input  v_cod_cta,           /* CONTA */
                                                   input  "(nenhum)",          /* FINALIDADES */
                                                   input  today,               /* DATA DE TRANSACAO */ 
                                                   output table tt_log_erro).  /* ERROS */
       /* validacao da conta */
       if can-find(tt_log_erro) or return-value = "NOK" then do:
           for first tt_log_erro:
               run utp/ut-msgs.p (input 'show',
                                  input 17006, 
                                  input string(tt_log_erro.ttv_des_msg_erro) + ' ('  + string(tt_log_erro.ttv_num_cod_erro) + ')' + "~~" + tt_log_erro.ttv_des_msg_ajuda).
               return "adm-error".
           end.
       end.
       
       delete procedure h_api_cta.
    end.
    
    /*:T Ponha na pi-validate todas as valida‡äes */
    /*:T NÆo gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    /* Dispatch standard ADM method.                             */
    run dispatch in this-procedure ( input 'assign-record':U ) .

    if return-value = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
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

    find canal-venda no-lock where 
         canal-venda.cod-canal-venda = input frame {&frame-name} esp-conta-frete.cod-canal-venda no-error.
    if avail canal-venda then
        assign c-desc-canal-venda:screen-value in frame {&frame-name} = canal-venda.descricao.
    else
        assign c-desc-canal-venda:screen-value in frame {&frame-name} = "".

    if not valid-handle(h_api_cta) then
        run prgint\utb\utb743za.py persistent set h_api_cta.

    assign v_cod_cta = input frame {&frame-name} esp-conta-frete.conta-contabil.

    if v_cod_cta <> "" then do:
        run pi_busca_dados_cta_ctbl in h_api_cta (input        param-global.empresa-prin, /* EMPRESA EMS2 */
                                                  input        "",                /* PLANO DE CONTAS */
                                                  input-output v_cod_cta,         /* CONTA */
                                                  input        today,             /* DATA TRANSACAO */   
                                                  output       v_des_cta,         /* DESCRICAO CONTA */
                                                  output       v_num_tip_cta,     /* TIPO DA CONTA */
                                                  output       v_num_sit_cta,     /* SITUA°€O DA CONTA */
                                                  output       v_ind_finalid_cta, /* FINALIDADES DA CONTA */
                                                  output table tt_log_erro). 
                                                  
        run pi_retorna_formato_cta_ctbl in h_api_cta (input param-global.empresa-prin,              /* EMPRESA EMS2 */
                                                      input  "",                    /* PLANO CONTAS */
                                                      input  today,                 /* DATA DE TRANSACAO */
                                                      output v_cod_format_cta,      /* FORMATO cta */
                                                      output table tt_log_erro).    /* ERROS */
                                          
    
        assign c-des-conta-frete:screen-value in frame {&frame-name} = v_des_cta.
        
        if v_cod_format_cta <> '' then
            assign esp-conta-frete.conta-contabil:format in frame {&frame-name} = v_cod_format_cta.
            
    end.

    delete procedure h_api_cta.    

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
  {src/adm/template/sndkycas.i "cod-canal-venda" "esp-conta-frete" "cod-canal-venda"}

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
  {src/adm/template/snd-list.i "esp-conta-frete"}

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

