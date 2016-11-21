/**************************************************************************
**   Programa: esp/eswm015.p
**   Data    : Dezembro de 2009
**   Autor   : Eder
**   Objetivo: Integra‡Æo WMS Itens / Sa¡da
**   Versao..: 
**************************************************************************/
DEF BUFFER prog_dtsul   FOR emsfnd.prog_dtsul.
DEF BUFFER usuar_mestre FOR emsfnd.usuar_mestre.
      
{include/i-prgvrs.i eswms015.p 2.06.00.000}
{utp/ut-glob.i}

DEFINE PARAM BUFFER ITEM FOR ITEM. 
DEF NEW GLOBAL SHARED VAR g-integrator AS LOG NO-UNDO.

/*FIND FIRST tt-grup-estoque-param OF ITEM
     WHERE CAN-DO("1,5",STRING(tt-grup-estoque-param.rs-tipo-grupo)) NO-ERROR.
IF NOT AVAIL tt-grup-estoque-param THEN 
    RETURN.*/

/*IF NOT CAN-FIND(item-uni-estab WHERE item-uni-estab.it-codigo = ITEM.it-codigo AND item-uni-estab.cod-estabel = "6") THEN
    RETURN.*/

IF NOT g-integrator THEN DO:
    RUN tools/QueueManager.p ("CodigoBarras",ITEM.it-codigo).        
    RETURN "OK".
END.

DEFINE TEMP-TABLE INT_E_CODIGO_BARRAS NO-UNDO
    FIELD CD_EMPRESA          AS INTEGER
    FIELD CD_PRODUTO          AS CHAR
    FIELD CD_BARRAS           AS CHAR
    FIELD CD_SITUACAO         AS INT 
    FIELD TP_CODIGO_BARRAS    AS INT
    FIELD QT_EMBALAGEM        AS DEC
    FIELD ID_CODIGO_PRINCIPAL AS CHAR
    FIELD NU_PRIORIDADE_USO   AS INT
    FIELD DT_ADDROW           AS DATE.

DEF VAR c-sql AS CHAR NO-UNDO.
DEF VAR h-api AS HANDLE NO-UNDO.

RUN esp/eswmapi999.p PERSISTENT SET h-api.

find first embalagem no-lock where
           embalagem.it-codigo = item.it-codigo no-error.
if avail embalagem then do:

    create int_e_codigo_barras.
    assign int_e_codigo_barras.cd_empresa          = if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1
           int_e_codigo_barras.cd_produto          = embalagem.it-codigo
           int_e_codigo_barras.cd_barras           = embalagem.it-codigo + string(int(embalagem.qt-item)) + "0000"
           int_e_codigo_barras.cd_situacao         = 1
           int_e_codigo_barras.tp_codigo_barras    = 14
           int_e_codigo_barras.qt_embalagem        = if embalagem.qt-item = 0 then 1 else embalagem.qt-item
           int_e_codigo_barras.id_codigo_principal = "N"
           int_e_codigo_barras.nu_prioridade_uso   = 0
           int_e_codigo_barras.dt_addrow           = today.
           
    RUN pi_insert IN h-api (TEMP-TABLE INT_E_CODIGO_BARRAS:DEFAULT-BUFFER-HANDLE).
    
    IF RETURN-VALUE <> "OK" THEN DO:
        FOR EACH esp-fila-wms EXCLUSIVE-LOCK
           WHERE esp-fila-wms.cod-trans = "CodigoBarras"
             AND esp-fila-wms.chave     = ITEM.it-codigo:
             
             CREATE esp-erro-wms.
             ASSIGN esp-erro-wms.id             = esp-fila-wms.id
                    esp-erro-wms.data-hora-erro = NOW
                    esp-erro-wms.mensagem       = RETURN-VALUE.
             
        END.
    END.
    ELSE
        FOR EACH esp-fila-wms EXCLUSIVE-LOCK
           WHERE esp-fila-wms.cod-trans = "CodigoBarras"
             AND esp-fila-wms.chave     = ITEM.it-codigo:
             IF esp-fila-wms.data-hora-integracao = ? THEN
                 ASSIGN esp-fila-wms.data-hora-integracao = NOW.
        END.       

end.

FIND item-mat NO-LOCK WHERE
     item-mat.it-codigo = ITEM.it-codigo NO-ERROR.
IF AVAIL item-mat THEN DO:
    CREATE INT_E_CODIGO_BARRAS.
    ASSIGN INT_E_CODIGO_BARRAS.CD_EMPRESA          = if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1
           INT_E_CODIGO_BARRAS.CD_PRODUTO          = item.it-codigo
           INT_E_CODIGO_BARRAS.CD_BARRAS           = if trim(item-mat.cod-ean) <> "" then item-mat.cod-ean else item.it-codigo
           INT_E_CODIGO_BARRAS.CD_SITUACAO         = 1
           INT_E_CODIGO_BARRAS.TP_CODIGO_BARRAS    = 13
           INT_E_CODIGO_BARRAS.QT_EMBALAGEM        = 1
           INT_E_CODIGO_BARRAS.ID_CODIGO_PRINCIPAL = "N"
           INT_E_CODIGO_BARRAS.NU_PRIORIDADE_USO   = 0
           INT_E_CODIGO_BARRAS.DT_ADDROW           = today.

    RUN pi_insert IN h-api (TEMP-TABLE INT_E_CODIGO_BARRAS:DEFAULT-BUFFER-HANDLE).
    IF RETURN-VALUE <> "OK" THEN DO:
        FOR EACH esp-fila-wms EXCLUSIVE-LOCK
           WHERE esp-fila-wms.cod-trans = "CodigoBarras"
             AND esp-fila-wms.chave     = ITEM.it-codigo:
             
             CREATE esp-erro-wms.
             ASSIGN esp-erro-wms.id             = esp-fila-wms.id
                    esp-erro-wms.data-hora-erro = NOW
                    esp-erro-wms.mensagem       = RETURN-VALUE.
             
        END.
    END.
    ELSE
        FOR EACH esp-fila-wms EXCLUSIVE-LOCK
           WHERE esp-fila-wms.cod-trans = "CodigoBarras"
             AND esp-fila-wms.chave     = ITEM.it-codigo:
             IF esp-fila-wms.data-hora-integracao = ? THEN
                 ASSIGN esp-fila-wms.data-hora-integracao = NOW.
        END.
END.

/*DEFINE VARIABLE i-seq AS INTEGER     NO-UNDO.
FOR EACH item-dun NO-LOCK
   WHERE item-dun.it-codigo = ITEM.it-codigo:

    EMPTY TEMP-TABLE  INT_E_CODIGO_BARRAS.

    i-seq = i-seq + 1.

    CREATE INT_E_CODIGO_BARRAS.
    ASSIGN INT_E_CODIGO_BARRAS.CD_aA          = i-ep-codigo-usuario
           INT_E_CODIGO_BARRAS.CD_PRODUTO          = ITEM.it-codigo
           INT_E_CODIGO_BARRAS.CD_BARRAS           = item-dun.cod-dun
           INT_E_CODIGO_BARRAS.CD_SITUACAO         = 1
           INT_E_CODIGO_BARRAS.TP_CODIGO_BARRAS    = 14
           INT_E_CODIGO_BARRAS.QT_EMBALAGEM        = 1
           INT_E_CODIGO_BARRAS.ID_CODIGO_PRINCIPAL = "N"
           INT_E_CODIGO_BARRAS.NU_PRIORIDADE_USO   = i-seq
           INT_E_CODIGO_BARRAS.DT_ADDROW           = NOW
           INT_E_CODIGO_BARRAS.ID_PROCESSADO       = "N"
           INT_E_CODIGO_BARRAS.DT_PROCESSADO       = ?.
           
    RUN pi_insert IN h-api (TEMP-TABLE INT_E_CODIGO_BARRAS:DEFAULT-BUFFER-HANDLE).
    IF RETURN-VALUE <> "OK" THEN DO:
        FOR EACH esp-fila-wms EXCLUSIVE-LOCK
           WHERE esp-fila-wms.cod-trans = "CodigoBarras"
             AND esp-fila-wms.chave     = ITEM.it-codigo:
             
             CREATE esp-erro-wms.
             ASSIGN esp-erro-wms.id             = esp-fila-wms.id
                    esp-erro-wms.data-hora-erro = NOW
                    esp-erro-wms.mensagem       = RETURN-VALUE.
        END.
    END.
    ELSE
        FOR EACH esp-fila-wms EXCLUSIVE-LOCK
           WHERE esp-fila-wms.cod-trans = "CodigoBarras"
             AND esp-fila-wms.chave     = ITEM.it-codigo:
             IF esp-fila-wms.data-hora-integracao = ? THEN
                 ASSIGN esp-fila-wms.data-hora-integracao = NOW.
        END.

END.*/
RUN pi_finalizar IN h-api.

DELETE PROCEDURE h-api.


