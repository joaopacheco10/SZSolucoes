/**************************************************************************
**   Programa: esp/eswme005.p
**   Data    : Maio de 2015
**   Autor   : Eder
**   Objetivo: Integra‡Æo WMS Recebimento Invent rio
**   Versao..: 
**************************************************************************/
DEF BUFFER prog_dtsul   FOR emsfnd.prog_dtsul.
DEF BUFFER usuar_mestre FOR emsfnd.usuar_mestre.

{include/i-prgvrs.i eswme005 2.11.00.001}

DEF VAR h-api        AS HANDLE  NO-UNDO.

DEF TEMP-TABLE INT_S_INVENTARIO_ESTATICO NO-UNDO
    FIELDS CD_EMPRESA       AS INTEGER FORMAT ">>>9"
    FIELDS CD_DEPOSITO      AS CHAR FORMAT "x(3)"
    FIELDS NU_INVENTARIO    AS INTEGER
    FIELDS CD_PRODUTO       AS CHAR FORMAT "x(40)"
    FIELDS QT_PRODUTO       AS DEC
    FIELDS DS_AREA_ERP      AS CHAR
    FIELDS NU_LOTE          AS CHAR
    FIELDS DT_ADDROW        AS DATETIME
    FIELDS ID_PROCESSADO    AS CHAR FORMAT "x(1)"
    FIELDS DT_PROCESSADO    AS DATE.

DEF TEMP-TABLE tt-saldos-wms NO-UNDO
    FIELDS it-codigo  LIKE ITEM.it-codigo
    FIELDS lote       LIKE saldo-estoq.lote
    FIELDS quantidade LIKE movto-estoq.quantidade.

DEF  INPUT PARAM p-cod-estabel AS CHAR NO-UNDO.
DEF  INPUT PARAM p-cod-depos   AS CHAR NO-UNDO.
DEF  OUTPUT PARAM TABLE FOR tt-saldos-wms.

RUN esp/eswmapi999.p PERSISTENT SET h-api.

RUN pi_select IN h-api (TEMP-TABLE INT_S_INVENTARIO_ESTATICO:DEFAULT-BUFFER-HANDLE).

FOR EACH INT_S_INVENTARIO_ESTATICO
   WHERE INT_S_INVENTARIO_ESTATICO.CD_DEPOSITO = p-cod-estabel
     AND INT_S_INVENTARIO_ESTATICO.DS_AREA_ERP = p-cod-depos:
    CREATE tt-saldos-wms.
    ASSIGN tt-saldos-wms.it-codigo  = INT_S_INVENTARIO_ESTATICO.CD_PRODUTO
           tt-saldos-wms.lote       = INT_S_INVENTARIO_ESTATICO.NU_LOTE
           tt-saldos-wms.quantidade = INT_S_INVENTARIO_ESTATICO.QT_PRODUTO.
END.

RUN pi_finalizar IN h-api.

DELETE PROCEDURE h-api.
