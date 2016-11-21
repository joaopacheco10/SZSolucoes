/**************************************************************************
**   Programa: esp/eswme009.p
**   Data    : Junho/2016
**   Autor   : Joao Pacheco
**   Objetivo: Integra‡Æo FIS - Recebimento CTE
**   Versao..: 
**************************************************************************/

{include/i-prgvrs.i eswme009.p 12.01.00.001}

define new global shared var c-seg-usuario as char no-undo.

def shared stream s-log.

def var h-api        as handle  no-undo.

def temp-table tt-docum-est no-undo like docum-est
    field r-rowid as rowid.

def temp-table tt-rat-docum no-undo like rat-docum
    field r-rowid as rowid.

def temp-table tt-item-doc-est no-undo like item-doc-est
    field r-rowid as rowid.

def temp-table tt-dupli-apagar no-undo like dupli-apagar
    field r-rowid as rowid.

def temp-table tt-dupli-imp no-undo like dupli-imp
    field r-rowid as rowid.

def temp-table tt-erro no-undo
    field identif-segment as char
    field cd-erro         as integer
    field desc-erro       as char format "x(80)".

def temp-table tt-erro2 no-undo
    field i-sequen as integer
    field cd-erro  as integer
    field mensagem as character.

def temp-table RowErrors no-undo
    field ErrorSequence    as integer
    field ErrorNumber      as integer
    field ErrorDescription as character
    field ErrorParameters  as character
    field ErrorType        as character
    field ErrorHelp        as character
    field ErrorSubType     as character.


def temp-table ti_s_faturamento no-undo
    field cd_empresa                as int
    field cd_cnpj_transp            as char
    field tp_entrada                as char
    field nu_cte                    as int
    field nu_cte_serie              as char
    field dt_emissao_cte            as date
    field cte_chave_acesso          as char
    field cte_protocolo_autorizacao as char
    field vlr_frete                 as dec
    field icms_vlr_base             as dec
    field icms_aliquota             as dec
    field icms_vlr                  as dec
    field iss_vlr_base              as dec
    field iss_aliquota              as dec
    field iss_vlr                   as dec
    field dt_vencimento             as date
    field cd_registro               as char
    field cfop                      as char
    field nu_faturamento            as int
    field dt_addrow                 as date
    field id_processado             as char.

def temp-table ti_s_det_faturamento no-undo
    field cd_empresa                as int
    field cd_cnpj_transp            as char
    field nu_cte                    as int
    field nu_cte_serie              as char
    field dt_emissao_cte            as date
    field nu_nf                     as int
    field nu_serie_nf               as char
    field dt_addrow                 as date
    field id_processado             as char.

if not valid-handle(h-api) then
    run esp/eswmapi998.p persistent set h-api.

put stream s-log unformatted
            "- " string(time,"hh:mm:ss") 
            " - Processando interface --> ti_s_faturamento" skip.

run pi_select in h-api (temp-table ti_s_faturamento:default-buffer-handle).

run pi_finalizar in h-api.

delete procedure h-api.

if not valid-handle(h-api) then
    run esp/eswmapi998.p persistent set h-api.

run pi_select in h-api (temp-table ti_s_det_faturamento:default-buffer-handle).

run pi_finalizar in h-api.

delete procedure h-api.

for each ti_s_faturamento,
   first ti_s_det_faturamento:

     put stream s-log unformatted
        "# " string(time,"hh:mm:ss") 
        " - Processando CTE/FATURAS --> ti_s_faturamento - CTE: " string(ti_s_faturamento.nu_cte) " ,CNPJ " ti_s_faturamento.cd_cnpj_transp skip.

    find transporte no-lock where
         transporte.cgc = ti_s_faturamento.cd_cnpj_transp no-error.
    if not avail transporte then do:
         put stream s-log unformatted
            "- " string(time,"hh:mm:ss") 
             "# Erro: Transportadora nÆo cadastrada" skip.
         next.
    end.

    do trans:

        if not valid-handle(h-api) then
            run esp/eswmapi998.p persistent set h-api.

        run pi-atualiza.

        if return-value = "OK" then do:
            run pi_update_status in h-api (temp-table ti_s_faturamento:default-buffer-handle).
        
            put stream s-log unformatted
                "- " string(time,"hh:mm:ss") 
                " @ Processado com sucesso --> cte: " ti_s_faturamento.nu_cte skip.
        end.
        else do:
            put stream s-log unformatted
                "- " string(time,"hh:mm:ss") 
                "# Erro: " + return-value skip.
                
            undo, next.                
        end.
    end.
end.


if valid-handle(h-api) then do:
    
    run pi_finalizar in h-api.
    
    delete procedure h-api.

end.

procedure pi-atualiza :

    def var c-nat-operacao like natur-oper.nat-operacao no-undo.
    def var dt-vencto      as date                      no-undo.
    def var i              as integer                   no-undo.
    def var c-nr-docto     as char                      no-undo.
    def var h-boin092      as handle                    no-undo.
    def var de-vl-docto    as dec                       no-undo.
    def var i-canal-venda  as integer                   no-undo.

    def buffer bdocum-est for docum-est.
    def buffer bestabel for estabelec.

    run inbo/boin092.p persistent set h-boin092.

    empty temp-table tt-docum-est. 
    empty temp-table tt-rat-docum.
    empty temp-table tt-item-doc-est.
    empty temp-table tt-dupli-apagar.
    empty temp-table tt-dupli-imp.
    empty temp-table tt-erro.
    empty temp-table tt-erro2.
    
    find first esp-param-cte-fis no-lock no-error.

    find first param-estoq no-lock no-error.

    find first estabelec no-lock where 
               estabelec.cod-estabel = param-estoq.estabel-pad no-error.

    find first emitente no-lock where 
               emitente.cgc = ti_s_faturamento.cd_cnpj_transp no-error.
    if not avail emitente then do:
        run utp/ut-msgs.p ("msg",
                           17006,
                           substitute("Emitente nÆo encontrado com CNPJ &1",ti_s_faturamento.cd_cnpj_transp)).
        
        return return-value.
    end.
    
    assign c-nat-operacao = ""
           i-canal-venda  = ?.

    find nota-fiscal no-lock where 
         nota-fiscal.cod-estabel = "1"                                          and
         nota-fiscal.serie       = trim(ti_s_det_faturamento.nu_serie_nf)       and
         nota-fiscal.nr-nota-fis = string(ti_s_det_faturamento.nu_nf,"9999999") no-error.
    if avail nota-fiscal then
        assign c-nat-operacao = nota-fiscal.nat-operacao
               i-canal-venda  = nota-fiscal.cod-canal-venda.
           
    find esp-natur-frete no-lock where
         esp-natur-frete.nat-operacao = c-nat-operacao no-error.
    if not avail esp-natur-frete then do:
        run utp/ut-msgs.p ("msg",
                          17006,
                          "Natureza de Opera‡Æo Frete nÆo cadastrada para natureza de sa¡da [" + c-nat-operacao + "].").
        return return-value.    
    end.
    
    find esp-conta-frete no-lock where
         esp-conta-frete.cod-canal-venda = i-canal-venda no-error.
    if not avail esp-conta-frete then do:
        run utp/ut-msgs.p ("msg",
                          17006,
                          "Conta Cont bil nÆo cadastrada para o canal de venda [" + string(i-canal-venda) + "].").
        return return-value.    
    end.
    
    if emitente.estado = estabelec.estado then
        assign c-nat-operacao = esp-natur-frete.nat-estadual.
    else
        assign c-nat-operacao = esp-natur-frete.nat-interestadual.

    if not available transporte then
        find first transporte no-lock where
                   transporte.cgc = emitente.cgc no-error.

    find first natur-oper no-lock where 
               natur-oper.nat-operacao = c-nat-operacao no-error.
    if not avail natur-oper then do: 
        run utp/ut-msgs.p ("msg",
                          17006,
                          "Natureza de Opera‡Æo [" + c-nat-operacao + "] nÆo Cadastrada.").
        return return-value.
    end.

    assign c-nr-docto = string(int(ti_s_faturamento.nu_cte),"9999999").

    assign de-vl-docto = ti_s_faturamento.vlr_frete.

    if can-find (docum-est where
                 docum-est.serie-docto  = ti_s_faturamento.nu_cte_serie and
                 docum-est.nro-docto    = c-nr-docto                    and
                 docum-est.cod-emitente = emitente.cod-emitente         and
                 docum-est.nat-operacao = natur-oper.nat-operacao ) then do:

        run utp/ut-msgs.p ("msg",
                          17006,
                          "Documento j  cadastrado.").
        return return-value.
    end.

    create tt-docum-est.
    assign tt-docum-est.serie-docto      = ti_s_faturamento.nu_cte_serie
           tt-docum-est.nro-docto        = c-nr-docto
           tt-docum-est.cod-emitente     = emitente.cod-emitente
           tt-docum-est.nat-operacao     = natur-oper.nat-operacao
           tt-docum-est.dt-emissao       = ti_s_faturamento.dt_emissao_cte
           tt-docum-est.tot-valor        = de-vl-docto
           tt-docum-est.cod-estabel      = param-estoq.estabel-pad
           tt-docum-est.estab-fisc       = param-estoq.estabel-pad
           tt-docum-est.dt-trans         = today
           tt-docum-est.observacao       = ""
           tt-docum-est.uf               = emitente.estado
           tt-docum-est.usuario          = c-seg-usuario
           tt-docum-est.conta-transit    = param-estoq.conta-fornec
           tt-docum-est.esp-docto        = 21
           tt-docum-est.valor-mercad     = de-vl-docto
           tt-docum-est.mod-frete        = 3 /*outros*/
           overlay(tt-docum-est.char-1,93,60)  = ti_s_faturamento.cte_chave_acesso
           overlay(tt-docum-est.char-2,143,8)  = "0".

    find first item no-lock where 
               item.it-codigo = esp-param-cte-fis.it-codigo no-error.

    find first item-uni-estab no-lock where 
               item-uni-estab.it-codigo = item.it-codigo no-error.

    create tt-item-doc-est.
    assign tt-item-doc-est.serie-docto    = tt-docum-est.serie-docto
           tt-item-doc-est.nro-docto      = tt-docum-est.nro-docto
           tt-item-doc-est.cod-emitente   = tt-docum-est.cod-emitente
           tt-item-doc-est.nat-operacao   = tt-docum-est.nat-operacao
           tt-item-doc-est.sequencia      = 10
           tt-item-doc-est.it-codigo      = item.it-codigo
           tt-item-doc-est.quantidade     = 1
           tt-item-doc-est.preco-total[1] = tt-docum-est.tot-valor
           tt-item-doc-est.cod-depos      = item.deposito-pad
           tt-item-doc-est.class-fiscal   = item.class-fiscal
           tt-item-doc-est.cd-trib-iss    = natur-oper.cd-trib-iss
           tt-item-doc-est.conta-contabil = replace(esp-conta-frete.conta-contabil,".","")
           tt-item-doc-est.aliquota-icm   = natur-oper.aliquota-icm
           tt-item-doc-est.base-iss[1]    = tt-item-doc-est.preco-total[1]
           tt-item-doc-est.cd-trib-ipi    = natur-oper.cd-trib-ipi
           tt-item-doc-est.cd-trib-icm    = natur-oper.cd-trib-icm
           tt-item-doc-est.cd-trib-iss    = natur-oper.cd-trib-iss
           tt-item-doc-est.qt-do-forn     = tt-item-doc-est.quantidade.

    if item.tipo-con-est = 3 then do:
        assign tt-item-doc-est.lote = "1"
               tt-item-doc-est.dt-vali-lote = 12/31/9999.
    end.

    assign dt-vencto = ti_s_faturamento.dt_vencimento.

    /* chamando a api do recebimento */
    run rep/reapi316b.p (input  "ADD",  /*cTranAction*/
                         input  table tt-docum-est,
                         input  table tt-rat-docum,
                         input  table tt-item-doc-est,
                         input  table tt-dupli-apagar,
                         input  table tt-dupli-imp,
                         output table tt-erro).

    find first tt-erro no-error.
    if avail tt-erro then do:
        return "Erro ao incluir documento: " + string(tt-erro.cd-erro) + "-" + tt-erro.desc-erro.
    end.
    else do:
        find bdocum-est no-lock where
             bdocum-est.serie-docto   =  tt-docum-est.serie-docto   and
             bdocum-est.nro-docto     =  tt-docum-est.nro-docto     and
             bdocum-est.cod-emitente  =  tt-docum-est.cod-emitente  and
             bdocum-est.nat-operacao  =  tt-docum-est.nat-operacao  no-error.
             
        create tt-dupli-apagar.
        assign tt-dupli-apagar.cod-estabel   = tt-docum-est.cod-estabel
               tt-dupli-apagar.serie-docto   = tt-docum-est.serie-docto
               tt-dupli-apagar.nro-docto     = tt-docum-est.nro-docto
               tt-dupli-apagar.nr-duplic     = tt-docum-est.nro-docto
               tt-dupli-apagar.cod-emitente  = tt-docum-est.cod-emitente
               tt-dupli-apagar.nat-operacao  = tt-docum-est.nat-operacao
               tt-dupli-apagar.cod-esp       = natur-oper.cod-esp
               tt-dupli-apagar.parcela       = "1"
               tt-dupli-apagar.dt-emissao    = tt-docum-est.dt-emissao
               tt-dupli-apagar.dt-vencim     = dt-vencto
               tt-dupli-apagar.vl-desconto   = 0
               tt-dupli-apagar.vl-a-pagar    = bdocum-est.tot-valor.

        run openQueryStatic in h-boin092 ( "Main":U ).

        run findDocumEst in h-boin092 (input tt-docum-est.cod-emitente,
                                       input tt-docum-est.serie-docto,
                                       input tt-docum-est.nro-docto,
                                       input tt-docum-est.nat-operacao).

        run emptyRowObject in h-boin092.

        /* Transfere TT-DUPLI-APAGAR para BO */
        run setRecord in h-boin092 (input table tt-dupli-apagar ).
    
        /* Seta Default Tipo Despesa */
        run setDefaultTpDespesa in h-boin092.
    
        /* Cria DUPLI-APAGAR */    
        run createRecord in h-boin092.

        empty temp-table rowErrors.
        empty temp-table tt-erro2.

        run getRowErrors in h-boin092 ( output table RowErrors ).
        
        find first RowErrors no-error.

        if avail RowErrors then do:

            run destroy in h-boin092.
            assign h-boin092 = ?.

            return "Erro ao incluir duplicata: " + string(RowErrors.ErrorNumber) + "-" + RowErrors.ErrorDescription.
        end.

        create nota-fisc-adc.
        assign nota-fisc-adc.cod-estab                  = bdocum-est.cod-estabel 
               nota-fisc-adc.cod-serie                  = bdocum-est.serie-docto
               nota-fisc-adc.cod-nota-fisc              = bdocum-est.nro-docto
               nota-fisc-adc.cdn-emitente               = bdocum-est.cod-emitente
               nota-fisc-adc.cod-natur-operac           = bdocum-est.nat-operacao
               nota-fisc-adc.idi-tip-dado               = 13.
               
        if lookup(bdocum-est.nat-operacao,"135309,135310,235308,235309") > 0 then
            overlay(nota-fisc-adc.cod-livre-2,9,2)   = "4".
        else
            overlay(nota-fisc-adc.cod-livre-2,9,2)   = "0".
               
        run destroy in h-boin092.
        assign h-boin092 = ?.
        
    end.

    if can-find(esp-fatura-frete where 
                esp-fatura-frete.nome-transp = transporte.nome-abrev             and
                esp-fatura-frete.ser-docto   = ti_s_det_faturamento.nu_serie_nf  and
                esp-fatura-frete.nr-fatura   = string(ti_s_det_faturamento.nu_nf,"9999999"))      then do:
        put stream s-log unformatted
            "- " string(time,"hh:mm:ss") 
             " - Erro: Fatura j  cadastrada: " + transporte.nome-abrev + "," + string(ti_s_det_faturamento.nu_nf,"9999999") skip.
         next.
    end.

    create esp-fatura-frete.
    assign esp-fatura-frete.nome-transp = transporte.nome-abrev
           esp-fatura-frete.ser-docto   = ti_s_det_faturamento.nu_serie_nf
           esp-fatura-frete.nr-fatura   = string(ti_s_det_faturamento.nu_nf,"9999999")
           esp-fatura-frete.dt-emissao  = ti_s_faturamento.dt_emissao_cte
           esp-fatura-frete.dt-vencto   = ti_s_faturamento.dt_vencimento
           esp-fatura-frete.vl-fatura   = ti_s_faturamento.vlr_frete
           esp-fatura-frete.ind-sit-fatura = 1.

    find last docum-est no-lock where
              docum-est.cod-emitente = emitente.cod-emitente and
              docum-est.serie-docto  = trim(ti_s_faturamento.nu_cte_serie) and
              docum-est.nro-docto    = string(int(ti_s_faturamento.nu_cte),"9999999") no-error.
    if not avail docum-est then do:
        put stream s-log unformatted
            "- " string(time,"hh:mm:ss") 
            " - Erro: Conhecimento nÆo encontrado no recebimento: " + string(ti_s_faturamento.nu_cte) skip .
        next.
    end.

    create esp-cte-fatura.
    assign esp-cte-fatura.nome-transp = transporte.nome-abrev
           esp-cte-fatura.ser-docto   = ti_s_det_faturamento.nu_serie_nf
           esp-cte-fatura.nr-fatura   = string(ti_s_det_faturamento.nu_nf,"9999999")
           esp-cte-fatura.ser-cte     = docum-est.serie-docto
           esp-cte-fatura.nr-cte      = docum-est.nro-docto  
           esp-cte-fatura.vlr-frete   = ti_s_faturamento.vlr_frete.
        
    put stream s-log unformatted
        "- " string(time,"hh:mm:ss") 
        " @ Processado com sucesso --> CTE: " string(ti_s_det_faturamento.nu_nf,"9999999") skip.

    return "OK".

end procedure.



