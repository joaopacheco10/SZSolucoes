/******************************************************************************
**  Programa.....: estg/tw-emitente.p
**  Elaboracao...: Dezembro/2009.
**  Autor........: Eder
**  Cadastrar TRG: emitente.
**  Objetivo.....: Exportar dados de clientes/fornecedores para WMS.
**  Versoes......: 001 - 20/12/2009 - Desenvolvimento Programa
******************************************************************************/

DEF PARAM BUFFER emitente     FOR emitente.
DEF PARAM BUFFER old-emitente FOR emitente.

/***************************************************************************
 ** Se for diferente de fornecedor ‚ cliente
 ** Se for diferente de cliente ‚ fornecedor
 ** a l¢gica se repete para chamar o programa duas vezes quando for ambos 
 **************************************************************************/


IF emitente.identific <> 2 THEN 
    RUN esp/eswms002.p (BUFFER emitente, 1).

IF emitente.identific <> 1 THEN
    RUN esp/eswms002.p (BUFFER emitente, 2).

RETURN "OK".  





