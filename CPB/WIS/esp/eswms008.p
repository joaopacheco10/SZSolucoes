/**************************************************************************
**   Programa: esp/eswms008.p
**   Data    : Janeiro de 2010
**   Autor   : Eder
**   Objetivo: Integra‡Æo WMS Nota Fiscais (Embarque)
**   Versao..: 
**************************************************************************/
      
{include/i-prgvrs.i eswms008.p 2.04.00.000}
{utp/ut-glob.i}

define param buffer embarque for embarque. 

def temp-table INT_E_CAB_NOTA_FISCAL no-undo
    fields CD_EMPRESA        as integer
    fields CD_DEPOSITO       as char format "x(3)"
    fields NU_NOTA_FISCAL    as char format "x(12)" 
    fields NU_SERIE_NF       as char format "x(3)"
    fields DT_EMISSAO        as date
    fields CD_FORNECEDOR	 as integer
    fields NU_CGC_FORNECEDOR as char format "x(20)"
    fields CD_SITUACAO       as integer
    fields TP_NOTA           as char format "x(3)".

def temp-table INT_E_DET_NOTA_FISCAL no-undo
    fields CD_EMPRESA        as integer
    fields CD_DEPOSITO       as char format "x(3)"
    fields NU_NOTA_FISCAL    as char format "x(12)" 
    fields NU_SERIE_NF       as char format "x(3)"
    fields CD_FORNECEDOR	 as integer
    fields NU_CGC_FORNEC     as char format "x(20)"
    fields NU_ITEM	         as integer
    fields CD_PRODUTO        as char format "x(40)"
    fields QT_PRODUTO        as decimal decimals 3
    fields NU_DOC_ERP        as char format "x(30)".

def var h-api     as handle no-undo.

run esp/eswmapi999.p persistent set h-api.

for each nota-fiscal no-lock
   where nota-fiscal.nr-embarque = embarque.nr-embarque:
    
    for first natur-oper fields (tipo-compra transf) no-lock
        where natur-oper.nat-operacao = embarque.nat-operacao:
    end.

    for first estabelec fields (cod-emitente) no-lock 
        where estabelec.cod-estabel = nota-fiscal.cod-estabel:
    end.

    for first emitente fields() 
        where emitente.cod-emitente = estabelec.cod-emitente:
    end.

    create INT_E_CAB_NOTA_FISCAL.
    assign INT_E_CAB_NOTA_FISCAL.CD_EMPRESA        = if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1
           INT_E_CAB_NOTA_FISCAL.CD_DEPOSITO       = nota-fiscal.cod-estabel
           INT_E_CAB_NOTA_FISCAL.NU_NOTA_FISCAL    = nota-fiscal.nr-nota-fis
           INT_E_CAB_NOTA_FISCAL.NU_SERIE_NF       = nota-fiscal.serie
           INT_E_CAB_NOTA_FISCAL.DT_EMISSAO        = nota-fiscal.dt-emis-nota
           INT_E_CAB_NOTA_FISCAL.CD_FORNECEDOR     = emitente.cod-emitente 
           INT_E_CAB_NOTA_FISCAL.NU_CGC_FORNECEDOR = emitente.cgc
           INT_E_CAB_NOTA_FISCAL.CD_SITUACAO       = 4
           INT_E_CAB_NOTA_FISCAL.TP_NOTA           = "DEV".

    run pi_insert in h-api (temp-table INT_E_CAB_NOTA_FISCAL:default-buffer-handle).

    for each it-nota-fisc no-lock of nota-fiscal:
        create INT_E_DET_NOTA_FISCAL.
        assign INT_E_DET_NOTA_FISCAL.CD_EMPRESA        = if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1
               INT_E_DET_NOTA_FISCAL.CD_DEPOSITO       = nota-fiscal.cod-estabel
               INT_E_DET_NOTA_FISCAL.NU_NOTA_FISCAL    = nota-fiscal.nr-nota-fis 
               INT_E_DET_NOTA_FISCAL.NU_SERIE_NF       = nota-fiscal.serie       
               INT_E_DET_NOTA_FISCAL.CD_FORNECEDOR     = emitente.cod-emitente
               INT_E_DET_NOTA_FISCAL.NU_CGC_FORNEC     = emitente.cgc   
               INT_E_DET_NOTA_FISCAL.NU_ITEM           = it-nota-fisc.nr-seq-fat
               INT_E_DET_NOTA_FISCAL.CD_PRODUTO        = it-nota-fisc.it-codigo
               INT_E_DET_NOTA_FISCAL.QT_PRODUTO        = it-nota-fisc.qt-faturada[1]
               INT_E_DET_NOTA_FISCAL.NU_DOC_ERP        = string(i-ep-codigo-usuario) + nota-fiscal.cod-estabel + nota-fiscal.nr-nota-fis + nota-fiscal.serie.
    
        run pi_insert in h-api (temp-table INT_E_DET_NOTA_FISCAL:default-buffer-handle).
    end.
end.

run pi_finalizar in h-api.

delete procedure h-api.
