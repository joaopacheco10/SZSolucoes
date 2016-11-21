/**************************************************************************
**   Programa: esp/eswme007.p
**   Data    : Maio de 2015
**   Autor   : Eder
**   Objetivo: Integra‡Æo WMS Recebimento Confirma‡Æo de embarque
**   Versao..: 
**************************************************************************/
def buffer prog_dtsul   for emsfnd.prog_dtsul.
def buffer usuar_mestre for emsfnd.usuar_mestre.

{include/i-prgvrs.i eswme007 2.11.00.001}

def var h-api          as handle  no-undo.
def var h-api-embarque as handle  no-undo.
def var de-qt-difer    as dec     no-undo.
def var c-nr-series    as char    no-undo.
DEF VAR c-carga        AS CHAR    NO-UNDO.
DEF VAR i-embarque     AS INT     NO-UNDO.
DEF VAR i-resumo       AS INT     NO-UNDO.

def shared stream s-log.

def buffer b-it-pre-fat for it-pre-fat.
def buffer b-res-cli for res-cli.

DEF temp-table INT_S_DET_PEDIDO_SAIDA no-undo
    fields NU_INTERFACE      AS INT64
    fields CD_CLIENTE        as integer
    fields CD_EMPRESA        as intege
    fields CD_DEPOSITO       as char format "x(3)"
    fields CD_PRODUTO        as char format "x(40)"
    fields NU_ITEM_CORP      as integer
    fields QT_EXPEDIDA       as decimal decimals 3
    fields NU_PEDIDO_ORIGEM  as char FORMAT "x(20)"
    fields NU_LOTE           as char.

def temp-table INT_S_CAB_PEDIDO_SAIDA no-undo

    fields NU_INTERFACE      AS INT64

    fields NU_PEDIDO_ORIGEM  as CHAR FORMAT "x(20)"

    fields CD_CARGA	     as integer

    fields CD_TRANSPORTADORA as integer

    fields CD_PORTA          as integer
    field  PS_PEDIDO         as dec
    field  QT_VOLUME         as dec
    field CD_SITUACAO        as int
    field DT_ENTREGA         as date.

    

{cdp/cdcfgdis.i}


def temp-table tt-it-pre-fat no-undo
    field cdd-embarq   as dec
    field nr-resumo    as int
    field nome-abrev   as char
    field nr-pedcli    as char
    field nr-sequencia as int
    field it-codigo    as char
    field qt-a-alocar  as deci
    field i-sequen     as int
    field nr-entrega   as int
    index ch-it-pre-fat is primary
        cdd-embarq
        nr-resumo
        nome-abrev
        nr-pedcli
        nr-sequencia
        it-codigo
        nr-entrega.
        
put stream s-log unformatted
            "- " string(time,"hh:mm:ss") 
            " - Processando interface --> INT_S_DET_PEDIDO_SAIDA" skip.
            
run esp/eswmapi999.p persistent set h-api.
run pi_select in h-api (temp-table INT_S_DET_PEDIDO_SAIDA:default-buffer-handle).
run pi_finalizar in h-api.

delete procedure h-api.

put stream s-log unformatted
            "- " string(time,"hh:mm:ss") 
            " - Processando interface --> INT_S_CAB_PEDIDO_SAIDA" skip.

run esp/eswmapi999.p persistent set h-api.
run pi_select in h-api (temp-table INT_S_CAB_PEDIDO_SAIDA:default-buffer-handle).

FOR EACH INT_S_DET_PEDIDO_SAIDA,
    FIRST INT_S_CAB_PEDIDO_SAIDA WHERE 
    INT_S_CAB_PEDIDO_SAIDA.NU_PEDIDO_ORIGEM = INT_S_DET_PEDIDO_SAIDA.NU_PEDIDO_ORIGEM and
    INT_S_CAB_PEDIDO_SAIDA.CD_SITUACAO      = 57                                      and
    INT_S_CAB_PEDIDO_SAIDA.DT_ENTREGA       >= TODAY - 15
    BREAK BY INT_S_DET_PEDIDO_SAIDA.NU_PEDIDO_ORIGEM:

    ASSIGN c-carga = string(INT_S_CAB_PEDIDO_SAIDA.CD_CARGA).
    
    if length(c-carga) >= 8 then
        assign i-embarque = INT(SUBSTRING(c-carga,1,length(c-carga) - 3))
               i-resumo   = INT(SUBSTRING(c-carga,length(c-carga) - 2,3)).
    else
        assign i-embarque = INT(SUBSTRING(c-carga,1,5))
               i-resumo   = INT(SUBSTRING(c-carga,6,2)).           
               
    if not can-find(first pre-fatur where
                          pre-fatur.cdd-embarq = i-embarque and
                          pre-fatur.nr-resumo  = i-resumo)  then
        next.                    
           
    put stream s-log unformatted
        "INICIO - " string(time,"hh:mm:ss") SKIP
        'CARGA - ' c-carga skip
        'EMBARQUE - ' i-embarque skip
        'RESUMO - ' i-resumo skip
        'DATA - ' INT_S_CAB_PEDIDO_SAIDA.DT_ENTREGA skip.        

    FOR FIRST res-cli WHERE
        res-cli.cdd-embarq = i-embarque AND
        res-cli.nr-resumo  = i-resumo NO-LOCK,
        FIRST ped-venda WHERE
        ped-venda.cod-emitente = INT_S_DET_PEDIDO_SAIDA.CD_CLIENTE AND
        ped-venda.nr-pedcli    = trim(INT_S_DET_PEDIDO_SAIDA.NU_PEDIDO_ORIGEM) NO-LOCK,
        FIRST it-pre-fat exclusive-lock WHERE
        it-pre-fat.cdd-embarq   = res-cli.cdd-embarq                  AND 
        it-pre-fat.nr-resumo    = res-cli.nr-resumo                   AND
        it-pre-fat.nome-abrev   = ped-venda.nome-abrev                AND
        it-pre-fat.nr-pedcli    = ped-venda.nr-pedcli                 AND
        it-pre-fat.nr-sequencia = INT_S_DET_PEDIDO_SAIDA.NU_ITEM_CORP AND 
        it-pre-fat.it-codigo    = INT_S_DET_PEDIDO_SAIDA.CD_PRODUTO:
    
        put stream s-log unformatted
                "- " string(time,"hh:mm:ss") 
                " - Processando Pedido --> " ped-venda.nr-pedcli " - Item: " INT_S_DET_PEDIDO_SAIDA.CD_PRODUTO skip.              
    
        FIND FIRST res-emb WHERE 
            res-emb.cdd-embarq  = it-pre-fat.cdd-embarq  AND 
            res-emb.nr-resumo   = it-pre-fat.nr-resumo   AND 
            res-emb.sigla-emb   = "CX" EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAIL res-emb THEN DO:
            CREATE res-emb.
            ASSIGN res-emb.cdd-embarq  = it-pre-fat.cdd-embarq
                   res-emb.nr-resumo   = it-pre-fat.nr-resumo
                   res-emb.sigla-emb   = "CX".
    
            FIND FIRST embalag WHERE 
                embalag.sigla-emb = "CX" NO-LOCK NO-ERROR.
            IF AVAIL embalag THEN
                ASSIGN res-emb.desc-vol  = embalag.descricao.
            ELSE
                ASSIGN res-emb.desc-vol  = "ERRO.: Corrigir embalagem WIS".
        END.
        
        put stream s-log unformatted
            'item 2 - ' INT_S_DET_PEDIDO_SAIDA.CD_PRODUTO skip.
    
        ASSIGN res-emb.qt-volumes = if INT_S_CAB_PEDIDO_SAIDA.QT_VOLUME <> ? then INT_S_CAB_PEDIDO_SAIDA.QT_VOLUME else 0.
        
        
        put stream s-log unformatted
            'item 3 - ' INT_S_DET_PEDIDO_SAIDA.CD_PRODUTO skip.
        
        find current res-emb no-lock no-error.
            
        /*
          usa o mesmo campo de status usado pelo WMS Datasul para indicar o status do item no WMS
          9 - Retornado
         */
        assign it-pre-fat.int-2 = 9.
        
        put stream s-log unformatted
            'item 4 - ' INT_S_DET_PEDIDO_SAIDA.CD_PRODUTO skip.
        
        
        assign de-qt-difer = INT_S_DET_PEDIDO_SAIDA.QT_EXPEDIDA - it-pre-fat.qt-alocada.
        
        IF de-qt-difer <> 0 then do:
            put unformatted "ANTES api..." skip.
    
            run eqp/eqapi300.p persistent set h-api-embarque.
            put unformatted "DEPOIS api..." skip.
    
    
            create tt-it-pre-fat.
            buffer-copy it-pre-fat to tt-it-pre-fat.
            assign tt-it-pre-fat.cdd-embarq  = it-pre-fat.cdd-embarq
                   tt-it-pre-fat.qt-a-alocar = de-qt-difer
                   tt-it-pre-fat.i-sequen    = 1.
            
            run pi-recebe-tt-it-pre-fat in h-api-embarque (input table tt-it-pre-fat).
            run pi-trata-tt-it-pre-fat  in h-api-embarque (input no).
    
            delete tt-it-pre-fat.
            
            delete procedure h-api-embarque.
        end.
        
        put stream s-log unformatted
            'item 5 - ' INT_S_DET_PEDIDO_SAIDA.CD_PRODUTO skip.
        
        find b-res-cli exclusive-lock where
             b-res-cli.cdd-embarq  = res-cli.cdd-embarq  and
             b-res-cli.nr-resumo   = res-cli.nr-resumo   and
             b-res-cli.nr-embarque = res-cli.nr-embarque no-error.
        if avail b-res-cli then do:
        
            assign res-cli.peso-liq-tot = INT_S_CAB_PEDIDO_SAIDA.PS_PEDIDO
                   res-cli.peso-bru-tot = INT_S_CAB_PEDIDO_SAIDA.PS_PEDIDO
                   res-cli.volume       = INT_S_CAB_PEDIDO_SAIDA.QT_VOLUME.
        end.        

        find current b-res-cli no-lock no-error.
        
        if not can-find(first b-it-pre-fat WHERE
                              b-it-pre-fat.cdd-embarq = it-pre-fat.cdd-embarq AND
                              b-it-pre-fat.nr-resumo  = it-pre-fat.nr-resumo  AND
                              b-it-pre-fat.nome-abrev = it-pre-fat.nome-abrev and
                              b-it-pre-fat.nr-pedcli  = it-pre-fat.nr-pedcli  and
                              b-it-pre-fat.int-2     <> 9) then do:
            FIND FIRST esp-resumo-wms WHERE 
                esp-resumo-wms.nr-embarque = INT(it-pre-fat.cdd-embarq) AND
                esp-resumo-wms.nr-resumo   = it-pre-fat.nr-resumo EXCLUSIVE-LOCK NO-ERROR.
            if avail esp-resumo-wms then
                assign esp-resumo-wms.confirmado = yes.
    
        end.
        
        run pi_update_status_interface in h-api (temp-table INT_S_DET_PEDIDO_SAIDA:default-buffer-handle).
    
        put stream s-log unformatted
            "- " string(time,"hh:mm:ss") 
            " @ Processado com sucesso --> " ped-venda.nr-pedcli " - Item: " INT_S_DET_PEDIDO_SAIDA.CD_PRODUTO skip.
        
    
        find pre-fatur exclusive-lock where
            pre-fatur.cdd-embarq = i-embarque and
            pre-fatur.nr-resumo  = i-resumo   AND
             pre-fatur.nome-abrev = ped-venda.nome-abrev and
             pre-fatur.nr-pedcli = ped-venda.nr-pedcli no-error.
        if avail pre-fatur then do:
            find transporte no-lock where transporte.cod-transp = INT_S_CAB_PEDIDO_SAIDA.CD_TRANSPORTADORA  no-error.
            if avail transporte then
                assign pre-fatur.nome-transp = transporte.nome-abrev
                       pre-fatur.num-livre-1 = INT_S_CAB_PEDIDO_SAIDA.CD_PORTA.
        end.

        IF LAST-OF(INT_S_DET_PEDIDO_SAIDA.NU_PEDIDO_ORIGEM) THEN DO:
            
            run pi_update_status_interface in h-api (temp-table INT_S_CAB_PEDIDO_SAIDA:default-buffer-handle).
        
            put stream s-log unformatted
                "- " string(time,"hh:mm:ss") 
                " @ Processado com sucesso --> " ped-venda.nr-pedcli skip.
        END.
    END.
end.

run pi_finalizar in h-api.

delete procedure h-api.
