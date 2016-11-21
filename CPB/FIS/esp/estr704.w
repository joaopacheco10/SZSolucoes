&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-cadpaifilho-filho
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-cadpaifilho-filho 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

{include/i-prgvrs.i ESTR704 12.01.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEF NEW GLOBAL SHARED VAR v_cod_empres_usuar AS CHAR FORMAT "x(3)":U LABEL "Empresa" COLUMN-LABEL "Empresa" NO-UNDO.
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def var p-table as rowid.

/* Temp-Table's para gerar dados no ems506 */
def temp-table tt_integr_apb_lote_fatura no-undo
    field tta_cod_estab                    as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_refer                    as character format "x(10)" label "Referància" column-label "Referància"
    field tta_cod_espec_docto              as character format "x(3)" label "EspÇcie Documento" column-label "EspÇcie"
    field tta_dat_transacao                as date      format "99/99/9999" initial today label "Data Transaá∆o" column-label "Dat Transac"
    field tta_ind_origin_tit_ap            as character format "X(03)" initial "APB" label "Origem" column-label "Origem"
    field tta_cod_estab_ext                as character format "x(8)" label "Estabelecimento Exte" column-label "Estabelecimento Ext"
    field tta_val_tot_lote_impl_tit_ap     as decimal   format ">>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Total  Movimento" column-label "Total Movto"
    field tta_cod_empresa                  as character format "x(3)" label "Empresa" column-label "Empresa"
    field ttv_cod_empresa_ext              as character format "x(3)" label "C¢digo Empresa Ext" column-label "C¢d Emp Ext"
    field tta_cod_finalid_econ_ext         as character format "x(8)" label "Finalid Econ Externa" column-label "Finalidade Externa"
    field tta_cod_indic_econ               as character format "x(8)" label "Moeda" column-label "Moeda"
    field ttv_log_atualiza_refer_apb       as logical   format "Sim/N∆o" initial yes label "Atualiza Referància" column-label "Atualiza Referància"
    field ttv_log_elimina_lote             as logical   format "Sim/N∆o" initial no
    field tta_cdn_fornecedor               as Integer   format ">>>,>>>,>>9" initial 0 label "Fornecedor" column-label "Fornecedor"
    field tta_num_fatur_ap                 as integer   format ">>>>,>>>,>>9" initial 0 label "N£mero  Fatura" column-label "Num Fatura"
    field tta_qtd_parcela                  as decimal   format "->9" decimals 0 initial 0 label "Qtd Parcelas" column-label "Qtd Parc"
    field tta_cod_histor_padr              as character format "x(8)" label "Hist¢rico Padr∆o" column-label "Hist¢rico Padr∆o"
    field tta_cod_histor_padr_dupl         as character format "x(8)" label "Hist¢rico Padr∆o" column-label "Hist¢rico Padr∆o"
    field ttv_ind_matriz_fornec            as character format "X(08)"
    field ttv_rec_integr_apb_lote_impl     as recid     format ">>>>>>9"
    field ttv_log_vinc_impto_auto          as logical   format "Sim/N∆o" initial no label "PIS/COFINS/CSLL Auto"
    index tt_lote_impl_tit_ap_integr_id    is primary unique
          tta_cod_estab                    ascending
          tta_cod_refer                    ascending
          tta_cod_estab_ext                ascending.

def temp-table tt_integr_apb_item_lote_fatura no-undo
    field ttv_rec_integr_apb_lote_impl     as recid     format ">>>>>>9"
    field tta_num_seq_refer                as integer   format ">>>9" initial 0 label "Sequància" column-label "Seq"
    field tta_cdn_fornecedor               as Integer   format ">>>,>>>,>>9" initial 0 label "Fornecedor" column-label "Fornecedor"
    field tta_cod_ser_docto                as character format "x(3)" label "SÇrie Documento" column-label "SÇrie"
    field tta_cod_tit_ap                   as character format "x(10)" label "T°tulo" column-label "T°tulo"
    field tta_cod_parcela                  as character format "x(02)" label "Parcela" column-label "Parc"
    field tta_dat_emis_docto               as date      format "99/99/9999" initial today label "Data  Emiss∆o" column-label "Dt Emiss∆o"
    field tta_dat_vencto_tit_ap            as date      format "99/99/9999" initial today label "Data Vencimento" column-label "Dt Vencto"
    field tta_dat_prev_pagto               as date      format "99/99/9999" initial today label "Data Prevista Pgto" column-label "Dt Prev Pagto"
    field tta_dat_desconto                 as date      format "99/99/9999" initial ? label "Data Desconto" column-label "Dt Descto"
    field tta_cod_indic_econ               as character format "x(8)" label "Moeda" column-label "Moeda"
    field tta_val_tit_ap                   as decimal   format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor T°tulo" column-label "Valor T°tulo"
    field tta_val_desconto                 as decimal   format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Desconto" column-label "Valor Desconto"
    field tta_val_perc_desc                as decimal   format ">9.9999" decimals 4 initial 0 label "Percentual Desconto" column-label "Perc Descto"
    field tta_num_dias_atraso              as integer   format ">9" initial 0 label "Dias Atraso" column-label "Dias Atr"
    field tta_val_juros_dia_atraso         as decimal   format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Juro" column-label "Vl Juro"
    field tta_val_perc_juros_dia_atraso    as decimal   format ">9.999999" decimals 6 initial 00.00 label "Perc Jur Dia Atraso" column-label "Perc Dia"
    field tta_val_perc_multa_atraso        as decimal   format ">9.99" decimals 2 initial 00.00 label "Perc Multa Atraso" column-label "Multa Atr"
    field tta_cod_portador                 as character format "x(5)" label "Portador" column-label "Portador"
    field tta_cod_apol_seguro              as character format "x(12)" label "Ap¢lice Seguro" column-label "Apolice Seguro"
    field tta_cod_seguradora               as character format "x(8)" label "Seguradora" column-label "Seguradora"
    field tta_cod_arrendador               as character format "x(6)" label "Arrendador" column-label "Arrendador"
    field tta_cod_contrat_leas             as character format "x(12)" label "Contrato Leasing" column-label "Contr Leas"
    field tta_des_text_histor              as character format "x(2000)" label "Hist¢rico" column-label "Hist¢rico"
    field tta_num_id_tit_ap                as integer   format "9999999999" initial 0 label "Token Tit AP" column-label "Token Tit AP"
    field tta_num_id_movto_tit_ap          as integer   format "9999999999" initial 0 label "Token Movto Tit AP" column-label "Id Tit AP"
    field tta_num_id_movto_cta_corren      as integer   format "9999999999" initial 0 label "ID Movto Conta" column-label "ID Movto Conta"
    field ttv_qtd_parc_tit_ap              as decimal   format ">>9" initial 1 label "Quantidade Parcelas" column-label "Quantidade Parcelas"
    field ttv_num_dias                     as integer   format ">>>>,>>9" label "N£mero de Dias" column-label "N£mero de Dias"
    field ttv_ind_vencto_previs            as character format "X(4)" initial "Màs" label "C†lculo Vencimento" column-label "C†lculo Vencimento"
    field ttv_log_gerad                    as logical   format "Sim/N∆o" initial no
    field tta_cod_finalid_econ_ext         as character format "x(8)" label "Finalid Econ Externa" column-label "Finalidade Externa"
    field tta_cod_portad_ext               as character format "x(8)" label "Portador Externo" column-label "Portador Externo"
    field tta_cod_modalid_ext              as character format "x(8)" label "Modalidade Externa" column-label "Modalidade Externa"
    field tta_cod_cart_bcia                as character format "x(3)" label "Carteira" column-label "Carteira"
    field tta_cod_forma_pagto              as character format "x(3)" label "Forma Pagamento" column-label "F Pagto"
    field tta_val_cotac_indic_econ         as decimal   format ">>>>,>>9.9999999999" decimals 10 initial 0 label "Cotaá∆o" column-label "Cotaá∆o"
    field ttv_num_ord_invest               as integer   format ">>>>>,>>9" initial 0 label "Ordem Investimento" column-label "Ordem Invest"
    field tta_cod_livre_1                  as character format "x(100)" label "Livre 1" column-label "Livre 1"
    field tta_cod_livre_2                  as character format "x(100)" label "Livre 2" column-label "Livre 2"
    field tta_dat_livre_1                  as date      format "99/99/9999" initial ? label "Livre 1" column-label "Livre 1"
    field tta_dat_livre_2                  as date      format "99/99/9999" initial ? label "Livre 2" column-label "Livre 2"
    field tta_log_livre_1                  as logical   format "Sim/N∆o" initial no label "Livre 1" column-label "Livre 1"
    field tta_log_livre_2                  as logical   format "Sim/N∆o" initial no label "Livre 2" column-label "Livre 2"
    field tta_num_livre_1                  as integer   format ">>>>>9" initial 0 label "Livre 1" column-label "Livre 1"
    field tta_num_livre_2                  as integer   format ">>>>>9" initial 0 label "Livre 2" column-label "Livre 2"
    field tta_val_livre_1                  as decimal   format ">>>,>>>,>>9.9999" decimals 4 initial 0 label "Livre 1" column-label "Livre 1"
    field tta_val_livre_2                  as decimal   format ">>>,>>>,>>9.9999" decimals 4 initial 0 label "Livre 2" column-label "Livre 2"
    field ttv_rec_integr_apb_item_lote     as recid     format ">>>>>>9"
    field ttv_val_1099                     as decimal   format "->>,>>>,>>>,>>9.99" decimals 2
    field tta_cod_tax_ident_number         as character format "x(15)" label "Tax Id Number" column-label "Tax Id Number"
    field tta_ind_tip_trans_1099           as character format "X(50)" initial "Rents" label "Tipo Transacao 1099" column-label "Tipo Transacao 1099"
    index tt_integr_apb_fatura_nf          is unique
          ttv_rec_integr_apb_lote_impl     ascending
          tta_cdn_fornecedor               ascending
          tta_cod_ser_docto                ascending
          tta_cod_tit_ap                   ascending
          tta_cod_parcela                  ascending
    index tt_item_lote_impl_ap_integr_id   is primary unique
          ttv_rec_integr_apb_lote_impl     ascending
          tta_num_seq_refer                ascending.

def temp-table tt_integr_apb_relacto_fatura no-undo
    field ttv_rec_integr_apb_lote_impl     as recid     format ">>>>>>9"
    field tta_cod_estab_tit_ap_pai         as character format "x(3)" label "Estab Tit Pai" column-label "Estab Tit Pai"
    field tta_cdn_fornec_pai               as Integer   format ">>>,>>>,>>9" initial 0 label "Fornecedor Pai" column-label "Fornecedor Pai"
    field tta_cod_espec_docto_nf           as character format "x(8)" label "Especie Nota Fiscal" column-label "Especie Nota Fiscal"
    field tta_cod_ser_docto_nf             as character format "x(8)" label "Serie Nota Fiscal" column-label "Serie Nota Fiscal"
    field tta_cod_tit_ap                   as character format "x(10)" label "T°tulo" column-label "T°tulo"
    field tta_cod_parc_nf                  as character format "x(8)" label "Parcela Nota fiscal" column-label "Parcela Nota fiscal"
    field tta_ind_motiv_acerto_val         as character format "X(12)" initial "Alteraá∆o" label "Motivo Acerto Valor" column-label "Motivo Acerto Valor"
    field ttv_log_bxo_estab_tit            as logical   format "Sim/N∆o" initial no
    index tt_integr_apb_relacto_fatura     is primary unique
          ttv_rec_integr_apb_lote_impl     ascending
          tta_cod_estab_tit_ap_pai         ascending
          tta_cdn_fornec_pai               ascending
          tta_cod_espec_docto_nf           ascending
          tta_cod_ser_docto_nf             ascending
          tta_cod_tit_ap                   ascending
          tta_cod_parc_nf                  ascending.

def temp-table tt_integr_apb_impto_impl_pend1 no-undo
    field ttv_rec_integr_apb_item_lote     as recid     format ">>>>>>9"
    field ttv_rec_antecip_pef_pend         as recid     format ">>>>>>9"
    field tta_cod_pais                     as character format "x(3)" label "Pa°s" column-label "Pa°s"
    field tta_cod_unid_federac             as character format "x(3)" label "Unidade Federaá∆o" column-label "UF"
    field tta_cod_imposto                  as character format "x(5)" label "Imposto" column-label "Imposto"
    field tta_cod_classif_impto            as character format "x(05)" initial "00000" label "Class Imposto" column-label "Class Imposto"
    field tta_ind_clas_impto               as character format "X(14)" initial "Retido" label "Classe Imposto" column-label "Classe Imposto"
    field tta_cod_plano_cta_ctbl           as character format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field tta_cod_cta_ctbl                 as character format "x(20)" label "Conta Cont†bil" column-label "Conta Cont†bil"
    field tta_cod_espec_docto              as character format "x(3)" label "EspÇcie Documento" column-label "EspÇcie"
    field tta_cod_ser_docto                as character format "x(3)" label "SÇrie Documento" column-label "SÇrie"
    field tta_cod_tit_ap                   as character format "x(10)" label "T°tulo" column-label "T°tulo"
    field tta_cod_parcela                  as character format "x(02)" label "Parcela" column-label "Parc"
    field tta_val_rendto_tribut            as decimal   format ">,>>>,>>>,>>9.99" decimals 2 initial 0 label "Rendto Tribut†vel" column-label "Vl Rendto Tribut"
    field tta_val_deduc_inss               as decimal   format ">,>>>,>>>,>>9.99" decimals 2 initial 0 label "Deduá∆o Inss" column-label "Deduá∆o Inss"
    field tta_val_deduc_depend             as decimal   format ">,>>>,>>>,>>9.99" decimals 2 initial 0 label "Deduá∆o Dependentes" column-label "Deduá∆o Dependentes"
    field tta_val_deduc_pensao             as decimal   format ">,>>>,>>>,>>9.99" decimals 2 initial 0 label "Deducao Pens∆o" column-label "Deducao Pens∆o"
    field tta_val_outras_deduc_impto       as decimal   format ">,>>>,>>>,>>9.99" decimals 2 initial 0 label "Outras Deduá‰es" column-label "Outras Deduá‰es"
    field tta_val_base_liq_impto           as decimal   format ">,>>>,>>>,>>9.99" decimals 2 initial 0 label "Base L°quida Imposto" column-label "Base L°quida Imposto"
    field tta_val_aliq_impto               as decimal   format ">9.99" decimals 2 initial 0.00 label "Al°quota" column-label "Aliq"
    field tta_val_impto_ja_recolhid        as decimal   format ">,>>>,>>>,>>9.99" decimals 2 initial 0 label "Imposto J† Recolhido" column-label "Imposto J† Recolhido"
    field tta_val_imposto                  as decimal   format ">,>>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Imposto" column-label "Vl Imposto"
    field tta_dat_vencto_tit_ap            as date      format "99/99/9999" initial today label "Data Vencimento" column-label "Dt Vencto"
    field tta_cod_indic_econ               as character format "x(8)" label "Moeda" column-label "Moeda"
    field tta_val_impto_indic_econ_impto   as decimal   format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Val Finalid Impto" column-label "Val Finalid Impto"
    field tta_des_text_histor              as character format "x(2000)" label "Hist¢rico" column-label "Hist¢rico"
    field tta_cdn_fornec_favorec           as Integer   format ">>>,>>>,>>9" initial 0 label "Fornec Favorecido" column-label "Fornec Favorecido"
    field tta_val_deduc_faixa_impto        as decimal   format ">,>>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Deducao" column-label "Valor Deduá∆o"
    field tta_num_id_tit_ap                as integer   format "9999999999" initial 0 label "Token Tit AP" column-label "Token Tit AP"
    field tta_num_id_movto_tit_ap          as integer   format "9999999999" initial 0 label "Token Movto Tit AP" column-label "Id Tit AP"
    field tta_num_id_movto_cta_corren      as integer   format "9999999999" initial 0 label "ID Movto Conta" column-label "ID Movto Conta"
    field tta_cod_pais_ext                 as character format "x(20)" label "Pa°s Externo" column-label "Pa°s Externo"
    field tta_cod_cta_ctbl_ext             as character format "x(20)" label "Conta Contab Extern" column-label "Conta Contab Extern"
    field tta_cod_sub_cta_ctbl_ext         as character format "x(15)" label "Sub Conta Externa" column-label "Sub Conta Externa"
    field ttv_cod_tip_fluxo_financ_ext     as character format "x(12)" label "Tipo Fluxo Financ" column-label "Tipo Fluxo Financ"
    index tt_impto_impl_pend_ap_integr     is primary unique
          ttv_rec_integr_apb_item_lote     ascending
          tta_cod_pais                     ascending
          tta_cod_unid_federac             ascending
          tta_cod_imposto                  ascending
          tta_cod_classif_impto            ascending
    index tt_impto_impl_pend_ap_integr_ant is unique
          ttv_rec_antecip_pef_pend         ascending
          tta_cod_pais                     ascending
          tta_cod_unid_federac             ascending
          tta_cod_imposto                  ascending
          tta_cod_classif_impto            ascending.

def NEW SHARED temp-table tt_log_erros_atualiz no-undo
    field tta_cod_estab                    as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_refer                    as character format "x(10)" label "Referància" column-label "Referància"
    field tta_num_seq_refer                as integer   format ">>>9" initial 0 label "Sequància" column-label "Seq"
    field ttv_num_mensagem                 as integer   format ">>>>,>>9" label "N£mero" column-label "N£mero Mensagem"
    field ttv_des_msg_erro                 as character format "x(60)" label "Mensagem Erro" column-label "Inconsistància"
    field ttv_des_msg_ajuda                as character format "x(40)" label "Mensagem Ajuda" column-label "Mensagem Ajuda"
    field ttv_ind_tip_relacto              as character format "X(15)" label "Tipo Relacionamento" column-label "Tipo Relac"
    field ttv_num_relacto                  as integer format ">>>>,>>9" label "Relacionamento" column-label "Relacionamento".

DEF TEMP-TABLE tt-erro2 NO-UNDO
    FIELD i-sequen AS integer
    FIELD cd-erro  AS integer
    FIELD mensagem AS character.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-paifil
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button bt-vencto bt-filtro 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 bt-aprov-1 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-cadpaifilho-filho AS WIDGET-HANDLE NO-UNDO.

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
DEFINE VARIABLE h_estr704-b01 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_estr704-q01 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_estr704-v01 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navega AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-aprov-1 
     IMAGE-UP FILE "image/im-chk1.bmp":U NO-FOCUS
     LABEL "1" 
     SIZE 4 BY 1.27 TOOLTIP "Atualizar".

DEFINE BUTTON bt-filtro 
     IMAGE-UP FILE "image/toolbar/im-fil.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Filtro" 
     SIZE 4 BY 1.27 TOOLTIP "Filtrar/Classificar".

DEFINE BUTTON bt-vencto 
     IMAGE-UP FILE "image/toolbar/calendario.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-calen.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Vencimento" 
     SIZE 4 BY 1.27 TOOLTIP "Alterar Vencimento".

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 109 BY 1.47
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-aprov-1 AT ROW 1.13 COL 75.33 WIDGET-ID 10
     bt-vencto AT ROW 1.13 COL 58 WIDGET-ID 44
     bt-filtro AT ROW 1.13 COL 44.89 WIDGET-ID 4
     rt-button AT ROW 1 COL 1.33
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 110.72 BY 21.5 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-paifil
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-cadpaifilho-filho ASSIGN
         HIDDEN             = YES
         TITLE              = "Manutená∆o <Insira o complemento>"
         HEIGHT             = 21.5
         WIDTH              = 110.67
         MAX-HEIGHT         = 21.63
         MAX-WIDTH          = 114.33
         VIRTUAL-HEIGHT     = 21.63
         VIRTUAL-WIDTH      = 114.33
         RESIZE             = yes
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-cadpaifilho-filho 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-paifil.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-cadpaifilho-filho
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
/* SETTINGS FOR BUTTON bt-aprov-1 IN FRAME f-cad
   NO-ENABLE 1                                                          */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadpaifilho-filho)
THEN w-cadpaifilho-filho:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-cadpaifilho-filho
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cadpaifilho-filho w-cadpaifilho-filho
ON END-ERROR OF w-cadpaifilho-filho /* Manutená∆o <Insira o complemento> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cadpaifilho-filho w-cadpaifilho-filho
ON WINDOW-CLOSE OF w-cadpaifilho-filho /* Manutená∆o <Insira o complemento> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-aprov-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-aprov-1 w-cadpaifilho-filho
ON CHOOSE OF bt-aprov-1 IN FRAME f-cad /* 1 */
DO:
    do trans:
        find current esp-fatura-frete exclusive-lock no-error.
        if avail esp-fatura-frete then do:
            assign esp-fatura-frete.ind-sit-fatura = 2
                   esp-fatura-frete.usr-aprov = c-seg-usuario.
                   
            run utp/ut-msgs.p ("show",
                               27100,
                               "Atualizar Fatura?~~A Fatura ser† atualizada no contas a pagar.").
            if return-value = "yes" then
                run pi-atualiza.

            if return-value <> "OK" then
                undo, return no-apply.                   
    
            run dispatch in h_estr704-v01 ("display-fields").
    
            run pi-botoes (rowid(esp-fatura-frete)).
        end.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-filtro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-filtro w-cadpaifilho-filho
ON CHOOSE OF bt-filtro IN FRAME f-cad /* Filtro */
DO:
    run esp/estr704-d01.w (h_estr704-q01).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vencto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vencto w-cadpaifilho-filho
ON CHOOSE OF bt-vencto IN FRAME f-cad /* Vencimento */
DO:
    do trans:
        if avail esp-fatura-frete then do:
            run esp/estr704-d02.w (rowid(esp-fatura-frete)).
            
            run dispatch in h_estr704-v01 ("display-fields").
    
            run pi-botoes (rowid(esp-fatura-frete)).
        end.
    end.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-anterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-anterior w-cadpaifilho-filho
ON CHOOSE OF MENU-ITEM mi-anterior /* Anterior */
DO:
  RUN pi-anterior IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-arquivo w-cadpaifilho-filho
ON MENU-DROP OF MENU mi-arquivo /* Arquivo */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-cadpaifilho-filho
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-cadpaifilho-filho
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-cadpaifilho-filho
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-pesquisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-pesquisa w-cadpaifilho-filho
ON CHOOSE OF MENU-ITEM mi-pesquisa /* Pesquisa */
DO:
  RUN pi-pesquisa IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-primeiro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-primeiro w-cadpaifilho-filho
ON CHOOSE OF MENU-ITEM mi-primeiro /* Primeiro */
DO:
  RUN pi-primeiro IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-proximo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-proximo w-cadpaifilho-filho
ON CHOOSE OF MENU-ITEM mi-proximo /* Pr¢ximo */
DO:
  RUN pi-proximo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-cadpaifilho-filho
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-cadpaifilho-filho
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-ultimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-ultimo w-cadpaifilho-filho
ON CHOOSE OF MENU-ITEM mi-ultimo /* Èltimo */
DO:
  RUN pi-ultimo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-va-para
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-va-para w-cadpaifilho-filho
ON CHOOSE OF MENU-ITEM mi-va-para /* V† para */
DO:
  RUN pi-vapara IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-cadpaifilho-filho 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */

{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-cadpaifilho-filho  _ADM-CREATE-OBJECTS
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
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.13 , 93.45 ) NO-ERROR.
       /* Size in UIB:  ( 1.27 , 16.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-navega.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navega ).
       RUN set-position IN h_p-navega ( 1.17 , 1.56 ) NO-ERROR.
       /* Size in UIB:  ( 1.27 , 24.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esp/estr704-v01.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_estr704-v01 ).
       RUN set-position IN h_estr704-v01 ( 2.53 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 6.27 , 109.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Conhecimentos' + ',
                     FOLDER-TAB-TYPE = 1':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 9.00 , 1.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 13.50 , 109.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Links to SmartViewer h_estr704-v01. */
       RUN add-link IN adm-broker-hdl ( h_estr704-q01 , 'Record':U , h_estr704-v01 ).

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navega ,
             h_p-exihel , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_estr704-v01 ,
             h_p-navega , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
             h_estr704-v01 , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esp/estr704-b01.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_estr704-b01 ).
       RUN set-position IN h_estr704-b01 ( 11.13 , 5.00 ) NO-ERROR.
       /* Size in UIB:  ( 10.07 , 99.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esp/estr704-q01.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'ProgPesquisa = esp/estr704-z01.w,
                     ProgVaPara = esp/estr704-g01.w,
                     ProgIncMod = ,
                     Implantar = no':U ,
             OUTPUT h_estr704-q01 ).
       RUN set-position IN h_estr704-q01 ( 1.00 , 33.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.63 , 7.67 ) */

       /* Links to SmartBrowser h_estr704-b01. */
       RUN add-link IN adm-broker-hdl ( h_estr704-v01 , 'Record':U , h_estr704-b01 ).

       /* Links to SmartQuery h_estr704-q01. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , h_estr704-q01 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'Navigation':U , h_estr704-q01 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'State':U , h_estr704-q01 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_estr704-b01 ,
             h_folder , 'AFTER':U ).
    END. /* Page 1 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-cadpaifilho-filho  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-cadpaifilho-filho  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadpaifilho-filho)
  THEN DELETE WIDGET w-cadpaifilho-filho.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-cadpaifilho-filho  _DEFAULT-ENABLE
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
  ENABLE rt-button bt-vencto bt-filtro 
      WITH FRAME f-cad IN WINDOW w-cadpaifilho-filho.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-cadpaifilho-filho.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-cadpaifilho-filho 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-cadpaifilho-filho 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-cadpaifilho-filho 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}

  run pi-before-initialize.

  {utp/ut9000.i "ESTR704" "12.01.00.001"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza w-cadpaifilho-filho 
PROCEDURE pi-atualiza :
    
    DEF VAR h-acomp                AS HANDLE                               NO-UNDO.
    DEF VAR v_hdl_apb925za         AS HANDLE                               NO-UNDO.
    def var i                      as integer                              no-undo.

    empty temp-table tt_integr_apb_lote_fatura. 
    empty temp-table tt_integr_apb_item_lote_fatura.
    empty temp-table tt_integr_apb_relacto_fatura.
    empty temp-table tt_integr_apb_impto_impl_pend1.
    empty temp-table tt_log_erros_atualiz. 
    empty temp-table tt-erro2.
    
    FIND FIRST emscad.empresa NO-LOCK NO-ERROR.

    find first param-estoq no-lock no-error.
    
    find first transporte no-lock where
               transporte.nome-abrev = esp-fatura-frete.nome-transp no-error.
    
    find first emitente no-lock where 
               emitente.cgc = transporte.cgc no-error.
    
    /*criar um titulo no contas a pagar no ems506 utitlizando substituicao de nota por duplicata*/
    CREATE tt_integr_apb_lote_fatura.
    ASSIGN tt_integr_apb_lote_fatura.tta_cod_estab                = param-estoq.estabel-pad
           tt_integr_apb_lote_fatura.tta_cod_refer                = STRING(TIME + 1)
           tt_integr_apb_lote_fatura.tta_cdn_fornecedor           = emitente.cod-emitente
           tt_integr_apb_lote_fatura.tta_dat_transacao            = TODAY
           tt_integr_apb_lote_fatura.tta_cod_espec_docto          = "FO"
           tt_integr_apb_lote_fatura.tta_num_fatur_ap             = INT(esp-fatura-frete.nr-fatura)
           tt_integr_apb_lote_fatura.tta_qtd_parcela              = 1
           tt_integr_apb_lote_fatura.tta_cod_indic_econ           = "REAL"
           tt_integr_apb_lote_fatura.tta_val_tot_lote_impl_tit_ap = 0
           tt_integr_apb_lote_fatura.tta_ind_origin_tit_ap        = "APB"
           tt_integr_apb_lote_fatura.tta_cod_empresa              = empresa.cod_empresa
           tt_integr_apb_lote_fatura.ttv_ind_matriz_fornec        = "Todos"
           tt_integr_apb_lote_fatura.ttv_rec_integr_apb_lote_impl = RECID(tt_integr_apb_lote_fatura)
           tt_integr_apb_lote_fatura.tta_val_tot_lote_impl_tit_ap = esp-fatura-frete.vl-fatura + esp-fatura-frete.val-desconto.
    
    CREATE tt_integr_apb_item_lote_fatura.
    ASSIGN tt_integr_apb_item_lote_fatura.ttv_rec_integr_apb_lote_impl = RECID(tt_integr_apb_lote_fatura)
           tt_integr_apb_item_lote_fatura.tta_num_seq_refer            = 10
           tt_integr_apb_item_lote_fatura.tta_cdn_fornecedor           = emitente.cod-emitente
           tt_integr_apb_item_lote_fatura.tta_cod_ser_docto            = if esp-fatura-frete.ser-docto = ? then "1" else esp-fatura-frete.ser-docto
           tt_integr_apb_item_lote_fatura.tta_cod_tit_ap               = esp-fatura-frete.nr-fatura
           tt_integr_apb_item_lote_fatura.tta_cod_parcela              = "1"
           tt_integr_apb_item_lote_fatura.tta_dat_emis_docto           = esp-fatura-frete.dt-emissao
           tt_integr_apb_item_lote_fatura.tta_dat_vencto_tit_ap        = esp-fatura-frete.dt-vencto
           tt_integr_apb_item_lote_fatura.tta_dat_prev_pagto           = esp-fatura-frete.dt-vencto
           tt_integr_apb_item_lote_fatura.tta_dat_desconto             = esp-fatura-frete.dt-vencto when esp-fatura-frete.val-desconto > 0
           tt_integr_apb_item_lote_fatura.tta_val_desconto             = esp-fatura-frete.val-desconto
           tt_integr_apb_item_lote_fatura.tta_cod_indic_econ           = "REAL"
           tt_integr_apb_item_lote_fatura.tta_cod_portador             = STRING(emitente.portador-ap)
           tt_integr_apb_item_lote_fatura.tta_des_text_histor          = "Fatura Importada do FIS"
           tt_integr_apb_item_lote_fatura.tta_val_tit_ap               = esp-fatura-frete.vl-fatura + esp-fatura-frete.val-desconto.
 
    for first transporte no-lock
        where transporte.nome-abrev = esp-fatura-frete.nome-transp,
        first emitente no-lock where emitente.cgc = transporte.cgc:
    end.
    
    for each esp-cte-fatura OF esp-fatura-frete no-lock:    
         
        find last docum-est no-lock where
             docum-est.cod-emitente = emitente.cod-emitente and 
             docum-est.serie-docto  = esp-cte-fatura.ser-cte and
             docum-est.nro-docto    = string(int(esp-cte-fatura.nr-cte),"9999999") no-error.
        if not avail docum-est then do:
            run utp/ut-msgs.p ("show",
                               17006,
                               substitute("Conhecimento &1 n∆o encontrado no recebimento",string(int(esp-cte-fatura.nr-cte),"9999999"))).
            
            return "NOK".
        end.
        
        

        for each dupli-apagar of docum-est no-lock:
            FOR EACH tit_ap NO-LOCK
               WHERE tit_ap.cod_estab       = dupli-apagar.cod-estabel
                 AND tit_ap.cdn_fornecedor  = dupli-apagar.cod-emitente
                 AND tit_ap.cod_espec_docto = dupli-apagar.cod-esp
                 AND tit_ap.cod_ser_docto   = dupli-apagar.serie-docto
                 AND tit_ap.cod_tit_ap      = dupli-apagar.nr-duplic
                 AND tit_ap.cod_parcela     = dupli-apagar.parcela:
                
                CREATE tt_integr_apb_relacto_fatura.
                ASSIGN tt_integr_apb_relacto_fatura.ttv_rec_integr_apb_lote_impl = RECID(tt_integr_apb_lote_fatura)
                       tt_integr_apb_relacto_fatura.tta_cod_estab_tit_ap_pai     = param-estoq.estabel-pad
                       tt_integr_apb_relacto_fatura.tta_cdn_fornec_pai           = tit_ap.cdn_fornecedor
                       tt_integr_apb_relacto_fatura.tta_cod_espec_docto_nf       = tit_ap.cod_espec_docto
                       tt_integr_apb_relacto_fatura.tta_cod_ser_docto_nf         = tit_ap.cod_ser_docto
                       tt_integr_apb_relacto_fatura.tta_cod_tit_ap               = tit_ap.cod_tit_ap
                       tt_integr_apb_relacto_fatura.tta_cod_parc_nf              = tit_ap.cod_parcela.                      
                
            end.
        end.
    end.

    /*Instancia a API*/
    RUN prgfin/apb/apb925za.py PERSISTENT SET v_hdl_apb925za.

    ASSIGN v_cod_empres_usuar = empresa.cod_empresa.

    /*Executa API*/
    RUN pi_main_code_apb925za_01 IN v_hdl_apb925za (INPUT 1,
                                                    INPUT TABLE tt_integr_apb_lote_fatura,
                                                    INPUT TABLE tt_integr_apb_item_lote_fatura,
                                                    INPUT TABLE tt_integr_apb_relacto_fatura,
                                                    INPUT TABLE tt_integr_apb_impto_impl_pend1,
                                                    INPUT-OUTPUT TABLE tt_log_erros_atualiz).
    /*Elimina Instanciamento*/
    DELETE PROCEDURE v_hdl_apb925za NO-ERROR.

    FIND FIRST tt_log_erros_atualiz NO-ERROR.
    IF AVAIL tt_log_erros_atualiz THEN DO:

        assign i = 0.
        FOR EACH tt_log_erros_atualiz:
            assign i = i + 1.
            create tt-erro2.
            assign tt-erro2.i-sequen = i
                   tt-erro2.cd-erro  = tt_log_erros_atualiz.ttv_num_mensagem
                   tt-erro2.mensagem = tt_log_erros_atualiz.ttv_des_msg_erro.
        END.
        run cdp/cd0666.w (input table tt-erro2).

        return "NOK".
    END.

    assign esp-fatura-frete.ind-sit-fatura = 3.
    return "OK".

    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-botoes w-cadpaifilho-filho 
PROCEDURE pi-botoes :
define input param r-fatura as rowid no-undo.

assign bt-aprov-1:sensitive in frame {&frame-name} = no
       bt-vencto:sensitive in frame {&frame-name}  = no.

find esp-fatura-frete no-lock where
     rowid(esp-fatura-frete) = r-fatura no-error.
if avail esp-fatura-frete then do:
    
    if esp-fatura-frete.ind-sit-fatura <> 3 then
        assign bt-vencto:sensitive in frame {&frame-name}  = yes.


    if esp-fatura-frete.ind-sit-fatura = 1 then
        assign bt-aprov-1:sensitive in frame {&frame-name} = yes.
        
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-cadpaifilho-filho  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-paifil, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-cadpaifilho-filho 
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

