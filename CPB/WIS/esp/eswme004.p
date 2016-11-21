/**************************************************************************
**   Programa: esp/eswme004.p
**   Data    : Maio de 2015
**   Autor   : Eder
**   Objetivo: Integra‡Æo WMS Ajustes de Estoque
**   Versao..: 
**************************************************************************/
DEF BUFFER prog_dtsul   FOR emsfnd.prog_dtsul.
DEF BUFFER usuar_mestre FOR emsfnd.usuar_mestre.

{include/i-prgvrs.i eswms004 2.11.00.001}

DEF VAR h-api      AS HANDLE  NO-UNDO.
DEF VAR l-saida    AS LOGICAL NO-UNDO.

DEF SHARED STREAM s-log.

DEF TEMP-TABLE INT_S_AJUSTE_ESTOQUE NO-UNDO
    FIELDS CD_EMPRESA          AS INTEGER FORMAT ">>>9"
    FIELDS CD_DEPOSITO         AS CHAR FORMAT "x(3)"
    FIELDS CD_PRODUTO          AS CHAR FORMAT "x(40)"
    FIELDS QT_MOVIMENTACAO     AS DECIMAL DECIMALS 3
    FIELDS DT_MOVIMENTO        AS DATE
    FIELDS DS_AREA_ERP_ORIGEM  AS CHAR FORMAT "x(10)"
    FIELDS DS_AREA_ERP_DESTINO AS CHAR FORMAT "x(10)"
    FIELDS CD_REGISTRO         AS INTEGER
    FIELDS TP_MOVIMENTO        AS CHAR FORMAT "x"
    FIELDS NU_LOTE             AS CHAR
/*    FIELDS NU_NOTA             AS CHAR*/
    FIELDS DT_ADDROW           AS DATETIME
    FIELDS ID_PROCESSADO       AS CHAR FORMAT "x(1)"
    FIELDS DT_PROCESSADO       AS DATE
    fields QT_AJUSTADA         AS DECIMAL format "->>>,>>>,>>9.999".

{utp/ut-glob.i}
{cep\ceapi001k.i}    /* include ref. api */ 

DEF TEMP-TABLE tt-erro NO-UNDO
    FIELD i-sequen AS INT             
    FIELD cd-erro  AS INT
    FIELD mensagem AS CHAR FORMAT "x(255)".

RUN esp/eswmapi999.p PERSISTENT SET h-api.

PUT STREAM s-log UNFORMATTED
            "- " STRING(TIME,"hh:mm:ss") 
            " - Processando interface --> INT_S_AJUSTE_ESTOQUE" SKIP.

RUN pi_select IN h-api (TEMP-TABLE INT_S_AJUSTE_ESTOQUE:DEFAULT-BUFFER-HANDLE).

FIND FIRST param-estoq NO-LOCK NO-ERROR.

/*
FIND conta-contab NO-LOCK WHERE
     conta-contab.ep-codigo      = i-ep-codigo-usuario AND 
     conta-contab.conta-contabil = param-estoq.conta-trans NO-ERROR.*/     
     
FOR EACH INT_S_AJUSTE_ESTOQUE where
         int_s_ajuste_estoque.DT_MOVIMENTO > today - 15
    BY INT_S_AJUSTE_ESTOQUE.CD_REGISTRO:
    
        
    IF CAN-FIND(FIRST movto-estoq 
                WHERE movto-estoq.serie-docto = "wis"
                  AND movto-estoq.nro-docto   = STRING(INT_S_AJUSTE_ESTOQUE.CD_REGISTRO)) then do:
        PUT STREAM s-log UNFORMATTED
            "# " STRING(TIME,"hh:mm:ss") 
            " - NU_MV -> " INT_S_AJUSTE_ESTOQUE.CD_REGISTRO
            " - Item --> " INT_S_AJUSTE_ESTOQUE.CD_PRODUTO " J  Processado. " skip.
        RUN pi_update_status IN h-api (TEMP-TABLE INT_S_AJUSTE_ESTOQUE:DEFAULT-BUFFER-HANDLE).            

        NEXT.
    END.
    
        
    FIND ITEM NO-LOCK WHERE
         ITEM.it-codigo = INT_S_AJUSTE_ESTOQUE.CD_PRODUTO NO-ERROR.

    IF NOT AVAIL ITEM THEN DO:
        PUT STREAM s-log UNFORMATTED
            "# " STRING(TIME,"hh:mm:ss") 
            " - NU_MV -> " INT_S_AJUSTE_ESTOQUE.CD_REGISTRO
            " - Item --> " INT_S_AJUSTE_ESTOQUE.CD_PRODUTO "NÆo Cadastrado!" SKIP.
        NEXT.
    END.
    
    FIND item-uni-estab NO-LOCK WHERE
         item-uni-estab.it-codigo = ITEM.it-codigo AND
         item-uni-estab.cod-estabel = STRING(INT(INT_S_AJUSTE_ESTOQUE.CD_DEPOSITO)) NO-ERROR.
    IF NOT AVAIL item-uni-estab THEN DO:
        PUT STREAM s-log UNFORMATTED
            "# " STRING(TIME,"hh:mm:ss") 
            " - NU_MV -> " INT_S_AJUSTE_ESTOQUE.CD_REGISTRO            
            " - Item --> " INT_S_AJUSTE_ESTOQUE.CD_PRODUTO "NÆo relacionado ao estabelecimento " INT_S_AJUSTE_ESTOQUE.CD_DEPOSITO SKIP.
        NEXT.
    END.

    EMPTY TEMP-TABLE tt-movto.
    EMPTY TEMP-TABLE tt-erro.   

    IF INT_S_AJUSTE_ESTOQUE.TP_MOVIMENTO = "T" THEN DO:
    
        CREATE tt-movto.
        ASSIGN tt-movto.cod-versao-integracao  = 1
               tt-movto.cod-prog-orig          = "ESWME004"
               tt-movto.tipo-trans             = 2
               tt-movto.esp-docto              = 33
               tt-movto.ct-codigo              = param-estoq.ct-tr-trans
               tt-movto.sc-codigo              = ""
               tt-movto.dt-trans               = INT_S_AJUSTE_ESTOQUE.DT_MOVIMENTO
               tt-movto.serie-docto            = "wis"
               tt-movto.nro-docto              = STRING(INT_S_AJUSTE_ESTOQUE.CD_REGISTRO)
               tt-movto.cod-depos              = INT_S_AJUSTE_ESTOQUE.DS_AREA_ERP_ORIGEM
               tt-movto.cod-estabel            = item-uni-estab.cod-estabel
               tt-movto.it-codigo              = ITEM.it-codigo
               tt-movto.cod-refer              = ""
               tt-movto.cod-localiz            = item-uni-estab.cod-localiz
               tt-movto.lote                   = INT_S_AJUSTE_ESTOQUE.NU_LOTE WHEN INT_S_AJUSTE_ESTOQUE.NU_LOTE <> ?
               tt-movto.dt-vali-lote           = if tt-movto.lote <> "" then date("31/12/9999") else ? /*12/31/9999 when tt-movto.lote <> ""*/
               tt-movto.quantidade             = INT_S_AJUSTE_ESTOQUE.QT_MOVIMENTACAO
               tt-movto.un                     = ITEM.un
               tt-movto.usuario                = c-seg-usuario
               tt-movto.descricao-db           = /*STRING(INT_S_AJUSTE_ESTOQUE.NU_NOTA) +*/ " | " + SUBST('Efetuado em &1 as &2.',STRING(TODAY,"99/99/9999"),STRING(TIME,"HH:MM")).

         PUT STREAM s-log UNFORMATTED
                "# " STRING(TIME,"hh:mm:ss")
            " - Item/depos/lote -> "  tt-movto.it-codigo tt-movto.cod-depos tt-movto.lote SKIP.

        /**************************entrada***************************/
            
        CREATE tt-movto.
        ASSIGN tt-movto.cod-versao-integracao = 1
               tt-movto.cod-prog-orig         = "ESWME004"
               tt-movto.ct-codigo             = param-estoq.ct-tr-trans
               tt-movto.sc-codigo             = ""
               tt-movto.dt-trans              = INT_S_AJUSTE_ESTOQUE.DT_MOVIMENTO
               tt-movto.serie-docto           = "wis"
               tt-movto.nro-docto             = STRING(INT_S_AJUSTE_ESTOQUE.CD_REGISTRO)
               tt-movto.cod-depos             = INT_S_AJUSTE_ESTOQUE.DS_AREA_ERP_DESTINO
               tt-movto.cod-estabel           = item-uni-estab.cod-estabel
               tt-movto.it-codigo             = ITEM.it-codigo
               tt-movto.cod-localiz           = item-uni-estab.cod-localiz
               tt-movto.lote                  = INT_S_AJUSTE_ESTOQUE.NU_LOTE WHEN INT_S_AJUSTE_ESTOQUE.NU_LOTE <> ?
               tt-movto.dt-vali-lote           = if tt-movto.lote <> "" then date("31/12/9999") else ? /*when tt-movto.lote <> ""*/
               tt-movto.quantidade            = INT_S_AJUSTE_ESTOQUE.QT_MOVIMENTACAO
               tt-movto.un                    = ITEM.un
               tt-movto.tipo-trans            = 1
               tt-movto.esp-docto             = 33
               tt-movto.usuario               = c-seg-usuario
               tt-movto.descricao-db          = /*STRING(INT_S_AJUSTE_ESTOQUE.NU_NOTA) +*/ " | " + SUBST('Efetuado em &1 as &2.',STRING(TODAY,"99/99/9999"),STRING(TIME,"HH:MM")).
               
               /*message "eswme004" skip
                       STRING(INT_S_AJUSTE_ESTOQUE.CD_REGISTRO) skip
                       "ITEM.it-codigo - " ITEM.it-codigo skip
                       "item-uni-estab.cod-estabel - " item-uni-estab.cod-estabel skip
                       "INT_S_AJUSTE_ESTOQUE.DS_AREA_ERP_ORIGEM - " INT_S_AJUSTE_ESTOQUE.DS_AREA_ERP_ORIGEM skip
                       "item-uni-estab.cod-localiz - " item-uni-estab.cod-localiz skip
                       "INT_S_AJUSTE_ESTOQUE.NU_LOTE - " INT_S_AJUSTE_ESTOQUE.NU_LOTE skip
                       "INT_S_AJUSTE_ESTOQUE.QT_MOVIMENTACAO - " INT_S_AJUSTE_ESTOQUE.QT_MOVIMENTACAO skip
                       "item-uni-estab.cod-localiz - " item-uni-estab.cod-localiz view-as alert-box.                
                      */
    END.
    ELSE DO:
        FOR FIRST estab-mat FIELDS(cod-estabel conta-inven-ent conta-inven-sai cod-cta-invent-e-unif cod-cta-invent-saida-unif) 
            WHERE estab-mat.cod-estabel = item-uni-estab.cod-estabel NO-LOCK: 
        END.
        
        PUT STREAM s-log UNFORMATTED
            'INT_S_AJUSTE_ESTOQUE.QT_AJUSTADA - ' INT_S_AJUSTE_ESTOQUE.QT_AJUSTADA skip.

        ASSIGN l-saida = INT_S_AJUSTE_ESTOQUE.QT_AJUSTADA < 0.

        /*
        IF NOT l-saida THEN
            FIND FIRST conta-contab 
                 WHERE conta-contab.ep-codigo      = i-ep-codigo-usuario
                   AND conta-contab.conta-contabil = estab-mat.cod-cta-invent-e-unif NO-LOCK NO-ERROR.
        ELSE
            FIND FIRST conta-contab 
                 WHERE conta-contab.ep-codigo      = i-ep-codigo-usuario
                   AND conta-contab.conta-contabil = estab-mat.cod-cta-invent-saida-unif NO-LOCK NO-ERROR.
                   */

        CREATE tt-movto.
        ASSIGN tt-movto.cod-versao-integ = 1
               tt-movto.cod-estabel      = item-uni-estab.cod-estabel
               tt-movto.cod-depos        = IF l-saida THEN INT_S_AJUSTE_ESTOQUE.DS_AREA_ERP_ORIGEM 
                                                      ELSE INT_S_AJUSTE_ESTOQUE.DS_AREA_ERP_DESTINO 
               tt-movto.cod-localiz      = item-uni-estab.cod-localiz
               tt-movto.lote             = INT_S_AJUSTE_ESTOQUE.NU_LOTE WHEN INT_S_AJUSTE_ESTOQUE.NU_LOTE <> ?
               tt-movto.dt-vali-lote     = ?
               tt-movto.cod-refer        = ""
               tt-movto.it-codigo        = item-uni-estab.it-codigo
               tt-movto.un               = ITEM.un
               tt-movto.serie-docto      = "wis"
               tt-movto.nro-docto        = STRING(INT_S_AJUSTE_ESTOQUE.CD_REGISTRO)
               tt-movto.esp-docto        = 15
               tt-movto.cod-prog-orig    = "ESWME004"
               tt-movto.dt-trans         = TODAY
               tt-movto.tipo-trans       = IF l-saida then 2 else 1
               tt-movto.quantidade       = ABSOLUTE(INT_S_AJUSTE_ESTOQUE.QT_MOVIMENTACAO)
               
               tt-movto.ct-codigo        = if not l-saida then estab-mat.cod-cta-invent-e-unif else estab-mat.cod-cta-invent-saida-unif
               tt-movto.sc-codigo        = ""
               tt-movto.usuario          = c-seg-usuario
               tt-movto.descricao-db     = /*STRING(INT_S_AJUSTE_ESTOQUE.NU_NOTA) +*/ " | " + SUBST('Efetuado em &1 as &2.',STRING(TODAY,"99/99/9999"),STRING(TIME,"HH:MM")).
        
             
    END.
    
    IF CAN-FIND(FIRST tt-movto) THEN DO:
        DEF VAR h-ceapi001K AS HANDLE NO-UNDO.
        RUN cep/ceapi001K.p PERSISTENT SET h-ceapi001K.

        RUN pi-execute IN h-ceapi001K (INPUT-OUTPUT TABLE tt-movto,
                                       INPUT-OUTPUT TABLE tt-erro,
                                       INPUT YES).
        
        FIND FIRST tt-erro NO-LOCK NO-ERROR.
        
        IF NOT AVAIL tt-erro THEN DO:
            RUN pi_update_status IN h-api (TEMP-TABLE INT_S_AJUSTE_ESTOQUE:DEFAULT-BUFFER-HANDLE).
            PUT STREAM s-log UNFORMATTED
                "# " STRING(TIME,"hh:mm:ss")
            " - NU_MV -> " INT_S_AJUSTE_ESTOQUE.CD_REGISTRO                 
                " - Item --> " INT_S_AJUSTE_ESTOQUE.CD_PRODUTO " Processado com Sucesso! " SKIP.
        END.
        ELSE do:
            
            PUT STREAM s-log UNFORMATTED
                "# " STRING(TIME,"hh:mm:ss") 
                " - NU_MV -> " INT_S_AJUSTE_ESTOQUE.CD_REGISTRO                
                " - Item --> " INT_S_AJUSTE_ESTOQUE.CD_PRODUTO " Erro CEAPI001 " tt-erro.mensagem SKIP.
        end.        
        DELETE PROCEDURE h-ceapi001K.
    END.
END.

RUN pi_finalizar IN h-api.
DELETE PROCEDURE h-api.

