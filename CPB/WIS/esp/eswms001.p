/**************************************************************************
**   Programa: esp/eswm001.p
**   Data    : Dezembro de 2009
**   Autor   : Eder
**   Objetivo: Integra‡Æo WMS Itens / Sa¡da
**   Versao..: 
**************************************************************************/
DEF BUFFER prog_dtsul   FOR emsfnd.prog_dtsul.
DEF BUFFER usuar_mestre FOR emsfnd.usuar_mestre.
      
{include/i-prgvrs.i eswms001.p 2.11.00.000}
{utp/ut-glob.i}

DEFINE PARAM BUFFER ITEM FOR ITEM. 
DEF NEW GLOBAL SHARED VAR g-integrator AS LOG NO-UNDO.
    
    put unformatted "item: " program-name(2).
IF NOT g-integrator THEN DO:
    RUN tools/QueueManager.p ("Item",ITEM.it-codigo).        
    RETURN "OK".
END.

DEF TEMP-TABLE INT_E_PRODUTO NO-UNDO
    FIELDS CD_EMPRESA             AS INTEGER FORMAT ">>>9"
    FIELDS CD_DEPOSITO            AS CHAR FORMAT "xxx" /*001*/
    FIELDS CD_PRODUTO             AS CHAR FORMAT "x(40)"
    FIELDS DS_PRODUTO             AS CHAR FORMAT "x(100)"
    FIELDS DS_REDUZIDA	          AS CHAR FORMAT "x(40)"
    FIELDS CD_UNIDADE_MEDIDA      AS CHAR FORMAT "x(4)"
    FIELDS DS_UNIDADE_MEDIDA      AS CHAR FORMAT "x(30)"
    FIELDS ID_ACEITA_DECIMAL      AS CHAR FORMAT "x(1)" 
    FIELDS CD_EMBALAGEM           AS CHAR FORMAT "x(3)"
    FIELDS DS_EMBALAGEM           AS CHAR FORMAT "x(40)"
    FIELDS QT_UNIDADE_EMBALAGEM   AS decimal
    FIELDS CD_PRODUTO_MASTER      AS CHAR FORMAT "x(40)"
    FIELDS QT_ITENS               AS decimal
    FIELDS CD_FAMILIA             AS CHAR INITIAL "."
    FIELDS DS_FAMILIA             AS CHAR INITIAL "."
    FIELDS VL_ALTURA	          AS DECIMAL DECIMALS 4
    FIELDS VL_LARGURA	          AS DECIMAL DECIMALS 4
    FIELDS VL_PROFUNDIDADE        AS DECIMAL DECIMALS 4
    FIELDS PS_LIQUIDO	          AS DECIMAL
    FIELDS PS_BRUTO	              AS DECIMAL
    FIELDS CD_SITUACAO	          AS INTEGER
    FIELDS CD_ROTATIVIDADE        AS CHAR FORMAT "x(2)"
    FIELDS CD_CLASSE              AS CHAR FORMAT "x(5)"
    FIELDS DS_CLASSE              AS CHAR FORMAT "x(35)"
    FIELDS QT_DIAS_VALIDADE       AS INTEGER
    FIELDS QT_DIAS_REMONTE        AS INTEGER
    FIELDS ID_CONTROLE_LOTE       AS CHAR FORMAT "x(1)" 
    FIELDS ID_CONTROLE_SERIE      AS CHAR FORMAT "x(1)"
    FIELDS ID_CONTROLE_VALIDADE   AS CHAR FORMAT "x(1)"
    FIELDS QT_CAIXA_FECHADA       AS decimal
    FIELDS CD_FORNECEDOR          AS CHAR FORMAT "x(10)"
    FIELDS CD_CNPJ_FORNECEDOR     AS INTEGER
    FIELDS CD_PRODUTO_FORNECEDOR  AS CHAR FORMAT "x(40)"
    FIELDS TP_ARMAZENAGEM_PRODUTO AS CHAR FORMAT "x(1)"
    FIELDS DT_ADDROW              AS DATE.

DEF VAR h-api AS HANDLE NO-UNDO.

FIND tab-unidade NO-LOCK
    WHERE tab-unidade.un = ITEM.un NO-ERROR.

FIND fam-comerc NO-LOCK WHERE
     fam-comerc.fm-cod-com = ITEM.fm-cod-com NO-ERROR.   

CREATE INT_E_PRODUTO.
ASSIGN INT_E_PRODUTO.CD_EMPRESA             = if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1
       INT_E_PRODUTO.CD_DEPOSITO            = item.cod-estab
       INT_E_PRODUTO.CD_PRODUTO             = ITEM.it-codigo
       INT_E_PRODUTO.DS_PRODUTO             = SUBSTRING(ITEM.desc-item,1,100)
       INT_E_PRODUTO.DS_REDUZIDA            = SUBSTRING(ITEM.desc-item,1,40)
       INT_E_PRODUTO.CD_UNIDADE_MEDIDA      = IF AVAIL tab-unidade THEN tab-unidade.un ELSE "NC"
       INT_E_PRODUTO.DS_UNIDADE_MEDIDA      = IF AVAIL tab-unidade THEN tab-unidade.descricao ELSE "NÇO CADASTRADO"
       INT_E_PRODUTO.ID_ACEITA_DECIMAL      = "S"
       INT_E_PRODUTO.CD_EMBALAGEM           = "NC"
       INT_E_PRODUTO.DS_EMBALAGEM           = "NÇO CADASTRADO"
       INT_E_PRODUTO.QT_UNIDADE_EMBALAGEM   = 1
       INT_E_PRODUTO.CD_PRODUTO_MASTER      = ITEM.it-codigo
       INT_E_PRODUTO.QT_ITENS               = 1
       INT_E_PRODUTO.CD_FAMILIA             = IF AVAIL fam-comerc and trim(fam-comerc.fm-cod-com) <> "" THEN fam-comerc.fm-cod-com ELSE "NC"
       INT_E_PRODUTO.DS_FAMILIA             = IF AVAIL fam-comerc THEN fam-comerc.descricao ELSE "NÇO CADASTRADO"
       INT_E_PRODUTO.VL_ALTURA              = if ITEM.altura = 0 then 1 else ITEM.altura
       INT_E_PRODUTO.VL_LARGURA             = if ITEM.largura = 0 then 1 else ITEM.largura
       INT_E_PRODUTO.VL_PROFUNDIDADE        = if ITEM.comprim = 0 then 1 else ITEM.comprim
       INT_E_PRODUTO.PS_LIQUIDO             = IF item.peso-liquido = ? or item.peso-liquido = 0 THEN 1 ELSE ITEM.peso-liquido
       INT_E_PRODUTO.PS_BRUTO               = IF item.peso-bruto = ? or item.peso-bruto = 0 THEN 1 ELSE ITEM.peso-bruto
       INT_E_PRODUTO.CD_SITUACAO            = IF ITEM.cod-obsoleto = 1 THEN 15 ELSE 16
       INT_E_PRODUTO.CD_ROTATIVIDADE        = "NC" /* Verificar */
       INT_E_PRODUTO.CD_CLASSE              = "NC" /* Verificar */
       INT_E_PRODUTO.DS_CLASSE              = "NÇO CADASTRADO" /* Verificar */
       INT_E_PRODUTO.QT_DIAS_VALIDADE       = 0 
       INT_E_PRODUTO.QT_DIAS_REMONTE        = 0
       INT_E_PRODUTO.ID_CONTROLE_LOTE       = if item.tipo-con-est = 3 then "S" else "N"
       INT_E_PRODUTO.ID_CONTROLE_SERIE      = "N"
       INT_E_PRODUTO.ID_CONTROLE_VALIDADE   = "N"
       INT_E_PRODUTO.QT_CAIXA_FECHADA       = 1
       INT_E_PRODUTO.CD_FORNECEDOR          = ""
       INT_E_PRODUTO.CD_CNPJ_FORNECEDOR     = 0
       INT_E_PRODUTO.CD_PRODUTO_FORNECEDOR  = ""
       INT_E_PRODUTO.TP_ARMAZENAGEM_PRODUTO = ""
       INT_E_PRODUTO.DT_ADDROW              = today.

run esp/eswmapi999.p persistent set h-api.

run pi_insert in h-api (temp-table INT_E_PRODUTO:default-buffer-handle).
if return-value <> "OK" then do:
    for each esp-fila-wms exclusive-lock
       where esp-fila-wms.cod-trans = "Item"
         and esp-fila-wms.chave     = item.it-codigo:
         
         create esp-erro-wms.
         assign esp-erro-wms.id             = esp-fila-wms.id
                esp-erro-wms.data-hora-erro = now
                esp-erro-wms.mensagem       = return-value.
         
    end.
end.
else
    for each esp-fila-wms exclusive-lock
       where esp-fila-wms.cod-trans = "Item"
         and esp-fila-wms.chave     = item.it-codigo:
         if esp-fila-wms.data-hora-integracao = ? then
             assign esp-fila-wms.data-hora-integracao = now.    
    end.


run pi_finalizar in h-api.

delete procedure h-api.
