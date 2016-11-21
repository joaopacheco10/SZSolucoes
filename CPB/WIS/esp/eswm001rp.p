/******************************************************************************
**
**       Programa: esp/eswm001rp .p
**
**       Data....: Abril/2010.
**
**       Autor...: SZ - Eder
**
**       Objetivo: Integraá∆o WMS Invent†rio.
**
*******************************************************************************/
DEF BUFFER ped_exec                 FOR emsfnd.ped_exec.                                                                   
DEF BUFFER servid_exec              FOR emsfnd.servid_exec.
DEF BUFFER layout_impres_padr       FOR emsfnd.layout_impres_padr.
DEF BUFFER imprsor_usuar            FOR emsfnd.imprsor_usuar.
DEF BUFFER layout_impres            FOR emsfnd.layout_impres.
DEF BUFFER configur_layout_impres   FOR emsfnd.configur_layout_impres.
DEF BUFFER configur_tip_imprsor     FOR emsfnd.configur_tip_imprsor.
DEF BUFFER impressora               FOR emsfnd.impressora.
DEF BUFFER tip_imprsor              FOR emsfnd.tip_imprsor.
DEF BUFFER servid_exec_imprsor      FOR emsfnd.servid_exec_imprsor.
DEF BUFFER prog_dtsul               FOR emsfnd.prog_dtsul.
DEF BUFFER empresa                  FOR mgcad.empresa.
DEF BUFFER usuar_mestre             FOR emsfnd.usuar_mestre.
DEF BUFFER procedimento             FOR emsfnd.procedimento.
DEF BUFFER modul_dtsul              FOR emsfnd.modul_dtsul.

{include/i-prgvrs.i eswm001rp 2.04.00.000} 

DO FOR ped_exec, servid_exec:
    {include/i-rpvar.i}
END.

DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD destino          AS INTEGER
    FIELD arquivo          AS CHAR FORMAT "x(35)"
    FIELD usuario          AS CHAR FORMAT "x(12)"
    FIELD data-exec        AS DATE
    FIELD hora-exec        AS INTEGER
    FIELD classifica       AS INTEGER
    FIELD desc-classifica  AS CHAR FORMAT "x(40)"
    FIELD modelo-rtf       AS char format "x(35)"
    FIELD l-habilitaRtf    AS LOG
    FIELD cod-estabel      AS char.

DEF VAR l-todos AS LOG NO-UNDO.

DEFINE TEMP-TABLE tt-digita
    FIELD cod-depos   LIKE deposito.cod-depos
    FIELD descricao   LIKE deposito.nome 
    FIELD selec       AS LOGICAL FORMAT "*/ " LABEL ""
    INDEX id IS PRIMARY cod-depos.

DEF TEMP-TABLE tt-saldo-estoq NO-UNDO
    FIELDS cod-estabel LIKE saldo-estoq.cod-estabel
    FIELDS cod-depos   LIKE saldo-estoq.cod-depos
    FIELDS it-codigo   LIKE ITEM.it-codigo
    FIELDS lote        LIKE saldo-estoq.lote
    FIELDS quantidade  LIKE movto-estoq.quantidade.

DEF TEMP-TABLE tt-raw-digita NO-UNDO
    FIELD raw-digita       AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

DEF NEW GLOBAL SHARED VAR i-ep-codigo-usuario  LIKE mgcad.empresa.ep-codigo NO-UNDO.

DEF TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER
    FIELD ErrorParameters  AS CHARACTER
    FIELD ErrorType        AS CHARACTER
    FIELD ErrorHelp        AS CHARACTER
    FIELD ErrorSubType     AS CHARACTER.

DEF TEMP-TABLE INT_S_INVENTARIO_ESTATICO NO-UNDO
    FIELDS CD_EMPRESA       AS INTEGER FORMAT ">>>9"
    FIELDS CD_DEPOSITO      AS CHAR FORMAT "x(3)"
    FIELDS CD_PRODUTO       AS CHAR FORMAT "x(40)"
    FIELDS QT_PRODUTO       AS DEC
    FIELDS DS_AREA_ERP      AS CHAR
    FIELDS NU_INVENTARIO    AS INTEGER
    FIELDS NU_LOTE          AS CHAR
    FIELDS NU_INTERFACE     AS INTEGER. 

DEF TEMP-TABLE tt-inventario NO-UNDO LIKE inventario
     FIELD r-Rowid   AS ROWID.

DEF VAR h-boin157q01 AS HANDLE  NO-UNDO.
DEF VAR i-nr-ficha   AS INTEGER NO-UNDO.
DEF VAR r-rowid-aux  AS ROWID   NO-UNDO.
DEFINE VARIABLE h-api AS HANDLE      NO-UNDO.

def var d-qtd-saldoestoq like saldo-estoq.qtidade-atu no-undo.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

FOR EACH tt-raw-digita:
    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
END.

FIND mgcad.empresa NO-LOCK WHERE
     empresa.ep-codigo = "1" /*i-ep-codigo-usuario*/ NO-ERROR.

ASSIGN c-titulo-relat = "Integraá∆o WMS Invent†rio"
       c-sistema      = "Datasul"
       c-empresa      = empresa.razao-social.

FORM tt-saldo-estoq.cod-estabel
     tt-saldo-estoq.cod-depos
     tt-saldo-estoq.it-codigo
     tt-saldo-estoq.lote
     tt-saldo-estoq.quantidade
     RowErrors.ErrorDesc FORMAT "x(60)" LABEL "Mensagem"
     WITH STREAM-IO WIDTH 200 DOWN FRAME f-estoq.

DEF VAR h-acomp AS HANDLE NO-UNDO.
RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

{include/i-rpcab.i}
{include/i-rpout.i}
    
RUN pi-inicializar IN h-acomp (INPUT "Aguarde...").

RUN esp/eswmapi999.p PERSISTENT SET h-api.

RUN pi_select IN h-api (TEMP-TABLE INT_S_INVENTARIO_ESTATICO:DEFAULT-BUFFER-HANDLE).


FOR EACH tt-digita
   WHERE tt-digita.selec:
    
    EMPTY TEMP-TABLE tt-saldo-estoq.

    RUN pi-acompanhar IN h-acomp ("Processando dep¢sito: " + tt-digita.cod-depos).
    

    FOR EACH INT_S_INVENTARIO_ESTATICO
       WHERE INT_S_INVENTARIO_ESTATICO.CD_DEPOSITO = STRING(INT(tt-param.cod-estabel),"999")
         AND INT_S_INVENTARIO_ESTATICO.DS_AREA_ERP = tt-digita.cod-depos:

        FIND tt-saldo-estoq NO-LOCK WHERE
             tt-saldo-estoq.cod-estabel = tt-param.cod-estabel AND
             tt-saldo-estoq.cod-depos   = tt-digita.cod-depos AND
             tt-saldo-estoq.lote        = INT_S_INVENTARIO_ESTATICO.NU_LOTE AND
             tt-saldo-estoq.it-codigo   = INT_S_INVENTARIO_ESTATICO.CD_PRODUTO NO-ERROR.
        IF NOT AVAIL tt-saldo-estoq THEN DO:
            CREATE tt-saldo-estoq.
            ASSIGN tt-saldo-estoq.cod-estabel = tt-param.cod-estabel
                   tt-saldo-estoq.cod-depos   = tt-digita.cod-depos
                   tt-saldo-estoq.lote        = INT_S_INVENTARIO_ESTATICO.NU_LOTE
                   tt-saldo-estoq.it-codigo   = INT_S_INVENTARIO_ESTATICO.CD_PRODUTO.
        END.

        ASSIGN tt-saldo-estoq.quantidade = tt-saldo-estoq.quantidade + INT_S_INVENTARIO_ESTATICO.QT_PRODUTO.


    END.
    
    RUN inbo/boin157q01.p PERSISTENT SET h-boin157q01.
    run setConstraintDataSaldo in h-boin157q01 (input today).
    RUN openQueryStatic IN h-boin157q01 (INPUT "Inclui":U).

    FOR EACH tt-saldo-estoq:
        EMPTY TEMP-TABLE tt-inventario.
    
        FIND ITEM NO-LOCK WHERE
             ITEM.it-codigo = tt-saldo-estoq.it-codigo NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
            DISP tt-saldo-estoq.cod-estabel
                 tt-saldo-estoq.cod-depos
                 tt-saldo-estoq.it-codigo
                 tt-saldo-estoq.lote
                 tt-saldo-estoq.quantidade
                 "Item n∆o cadastrado" @ RowErrors.ErrorDesc 
                 WITH STREAM-IO WIDTH 200 DOWN FRAME f-estoq.
            DOWN WITH FRAME f-estoq.
            NEXT.
        END.
    
        FIND item-uni-estab NO-LOCK WHERE
             item-uni-estab.it-codigo   = ITEM.it-codigo AND
             item-uni-estab.cod-estabel = tt-saldo-estoq.cod-estabel NO-ERROR.
        IF NOT AVAIL item-uni-estab THEN DO:
            DISP tt-saldo-estoq.cod-estabel
                 tt-saldo-estoq.cod-depos
                 tt-saldo-estoq.it-codigo
                 tt-saldo-estoq.lote
                 tt-saldo-estoq.quantidade
                 "Item n∆o cadastrado no estabelecimento" @ RowErrors.ErrorDesc 
                 WITH STREAM-IO WIDTH 200 DOWN FRAME f-estoq.
            DOWN WITH FRAME f-estoq.
            NEXT.
        END.
        
        /*descontar saldo do deposito SAA*/
        assign d-qtd-saldoestoq = 0.
        
        for each saldo-estoq use-index item-lote no-lock where 
                 saldo-estoq.it-codigo   = item.it-codigo             and
                 saldo-estoq.cod-estabel = tt-saldo-estoq.cod-estabel and 
                 /*saldo-estoq.lote        = tt-saldo-estoq.lote        and*/
                 saldo-estoq.cod-depos   = "SAA":
    
            assign d-qtd-saldoestoq = d-qtd-saldoestoq + saldo-estoq.qtidade-atu - (saldo-estoq.qt-alocada  +                                    
                                                                     saldo-estoq.qt-aloc-ped +                                    
                                                                     saldo-estoq.qt-aloc-prod).
        end.
        
        assign tt-saldo-estoq.quantidade = tt-saldo-estoq.quantidade - d-qtd-saldoestoq.
        
        CREATE tt-inventario.
        ASSIGN tt-inventario.situacao = 4 . /* situaá∆o 'ok p/ invent†rio' */
    
        EMPTY TEMP-TABLE RowErrors NO-ERROR.
        
        RUN emptyRowErrors      IN h-boin157q01.
        RUN emptyRowObject      IN h-boin157q01.
        RUN setRecord           IN h-boin157q01 (INPUT TABLE tt-inventario).
        RUN setConstraintInclui IN h-boin157q01 (1). /* 1¶ contagem */
    
        FIND LAST inventario USE-INDEX nr-ficha NO-LOCK
            WHERE inventario.dt-saldo = TODAY NO-ERROR.
        IF AVAIL inventario THEN
            ASSIGN i-nr-ficha = inventario.nr-ficha + 1.
        ELSE 
            ASSIGN i-nr-ficha = 1.
    
        ASSIGN tt-inventario.dt-saldo       = TODAY
               tt-inventario.nr-ficha       = i-nr-ficha
               tt-inventario.it-codigo      = ITEM.it-codigo
               tt-inventario.cod-estabel    = tt-saldo-estoq.cod-estabel
               tt-inventario.cod-depos      = tt-saldo-estoq.cod-depos
               tt-inventario.cod-localiz    = item-uni-estab.cod-localiz
               tt-inventario.lote           = tt-saldo-estoq.lote WHEN tt-saldo-estoq.lote <> ?
               tt-inventario.dt-ult-entra   = 12/31/9999 WHEN tt-saldo-estoq.lote <> ?
               tt-inventario.cod-refer      = ""
               tt-inventario.val-apurado[1] = tt-saldo-estoq.quantidade.
                                              
        RUN setRecord          IN h-boin157q01 (INPUT TABLE tt-inventario).
    
        INVENTARIO:
        DO TRANSACTION:
            
            RUN piInicializa       IN h-boin157q01  (OUTPUT TABLE RowErrors).
           
            EMPTY TEMP-TABLE RowErrors NO-ERROR.
           
            RUN emptyRowErrors     IN h-boin157q01.
           
            RUN createRecord       IN h-boin157q01.
            RUN getRowErrors       IN h-boin157q01 (OUTPUT TABLE RowErrors).
            
            IF NOT CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorSubType = "ERROR":U) THEN DO:
                /* Atualiza saldo */          
                RUN getRowid       IN h-boin157q01 (OUTPUT r-rowid-aux).
                RUN piCalculaSaldo IN h-boin157q01 (INPUT "CREATE":U, INPUT r-rowid-aux).
                RUN getRowErrors   IN h-boin157q01 (OUTPUT TABLE RowErrors).
            END.
    
            FIND FIRST rowErrors WHERE RowErrors.ErrorSubType = "ERROR" NO-ERROR.
            IF AVAIL rowErrors THEN DO:
                DISP tt-saldo-estoq.cod-estabel
                      tt-saldo-estoq.cod-depos
                      tt-saldo-estoq.it-codigo
                      tt-saldo-estoq.lote
                      tt-saldo-estoq.quantidade
                      string(RowErrors.ErrorNumber) + "-" + RowErrors.ErrorDesc @ RowErrors.ErrorDesc FORMAT "x(60)" LABEL "Mensagem"
                      WITH STREAM-IO WIDTH 200 DOWN FRAME f-estoq.
                 DOWN WITH FRAME f-estoq.
                 UNDO INVENTARIO, next.
            END.
            else do:
        
                DISP tt-saldo-estoq.cod-estabel
                     tt-saldo-estoq.cod-depos
                     tt-saldo-estoq.it-codigo
                     tt-saldo-estoq.lote
                     tt-saldo-estoq.quantidade
                     "Registro importado com sucesso" @ RowErrors.ErrorDesc FORMAT "x(60)" LABEL "Mensagem"
                     WITH STREAM-IO WIDTH 200 DOWN FRAME f-estoq.
                DOWN WITH FRAME f-estoq.

               FOR EACH INT_S_INVENTARIO_ESTATICO
                  WHERE INT_S_INVENTARIO_ESTATICO.CD_DEPOSITO = STRING(INT(tt-saldo-estoq.cod-estabel),"999")
                    AND INT_S_INVENTARIO_ESTATICO.DS_AREA_ERP = tt-saldo-estoq.cod-depos
                    AND INT_S_INVENTARIO_ESTATICO.CD_PRODUTO  =  tt-saldo-estoq.it-codigo
                    AND INT_S_INVENTARIO_ESTATICO.nu_lote     = tt-saldo-estoq.lote:
         
                   RUN pi_update_status_interface IN h-api (TEMP-TABLE INT_S_INVENTARIO_ESTATICO:DEFAULT-BUFFER-HANDLE).
               END.
           end.
        end.
    END.
END.

DELETE PROCEDURE h-api.
RUN pi-finalizar IN h-acomp.

{include/i-rpclo.i}

RETURN "ok".

