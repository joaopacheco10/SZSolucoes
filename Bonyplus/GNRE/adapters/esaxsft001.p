/*
Adapter Gera‡Æo XML Envio Lote
*/
{utp/ut-glob.i}

define variable hgenxml             as handle       no-undo.
define variable hmessagehandler     as handle       no-undo.
define variable creturnvalue        as character    no-undo.
define variable hdocxml             as handle       no-undo.
define variable c-path              as character    no-undo.

define variable iid_0               as integer      no-undo.
define variable iid_1               as integer      no-undo.
define variable iid_2               as integer      no-undo.
define variable iid_3               as integer      no-undo.
define variable iid_4               as integer      no-undo.
define variable iid_5               as integer      no-undo.
define variable iid_6               as integer      no-undo.
define variable iIDSon              as integer      no-undo.

/* verifica se messagehandler.p est  na mem¢ria */
{xmlinc/xmlloadmessagehandler.i &messagehandler="hmessagehandler" &mhreturnvalue="creturnvalue"}

/* verifica se apixml esta na memoria */
{xmlinc/xmlloadgenxml.i &genxml="hgenxml" &gxreturnvalue="creturnvalue"}

/* verifica se o ut-genxml foi executado corretamente */
if creturnvalue <> "ok" then
    return creturnvalue.

procedure pi-gera-xml:

    def input param p-cod-lote like es-lote-gnre.cod-lote no-undo.
    def output param p-xml-envio as longchar no-undo.

    create x-document hdocxml.
    
    run reset in hgenxml.
    run setencoding in hgenxml ("utf-8").
    
    
    find es-lote-gnre no-lock where
         es-lote-gnre.cod-lote = p-cod-lote no-error.
    if avail es-lote-gnre then do:
            
        run pi-cabecalho.
    
        for each es-nota-fiscal-gnre no-lock where
                 es-nota-fiscal-gnre.cod-lote = es-lote-gnre.cod-lote:
                 
                 
            find nota-fiscal no-lock where
                 nota-fiscal.cod-estabel = es-nota-fiscal-gnre.cod-estabel and
                 nota-fiscal.serie       = es-nota-fiscal-gnre.serie       and
                 nota-fiscal.nr-nota-fis = es-nota-fiscal-gnre.nr-nota-fis no-error.
            if avail nota-fiscal then
                run pi-gera-xml-corpo.
        end.
    end.
    
    run generatexml in hgenxml (output hdocxml).
    
    hdocxml:save ("longchar", p-xml-envio).
    
    
    output to 'c:\temp\xml_001.txt'.
    
    export p-xml-envio.
    
    output close.
    
    delete object hdocxml   no-error.

end.

procedure pi-cabecalho:

    /* n¢ principal*/
    run addnode in hgenxml (0
                           ,"TLote_GNRE"
                           ,""
                           ,output iid_1).

    run setattribute in hgenxml (input iid_1
                                ,input "xmlns"
                                ,input "http://www.gnre.pe.gov.br").

    /* adiciona n¢ loja c¢digo em "infpedidos" */
    run addnode in hgenxml (iid_1
                           ,"guias"
                           ,""
                           ,output iid_2).

end.


procedure pi-gera-xml-corpo:

    find es-conf-uf-gnre no-lock where
         es-conf-uf-gnre.cod-uf      = nota-fiscal.estado              and
         es-conf-uf-gnre.cod-receita = es-nota-fiscal-gnre.cod-receita no-error.
    if avail es-conf-uf-gnre then do:
    
        run addnode in hgenxml (iid_2
                                ,"TDadosGNRE"
                                ,""
                                ,output iid_3). 
    
        run addnode in hgenxml (iid_3
                               ,"c01_UfFavorecida"
                               ,nota-fiscal.estado
                               ,output iid_4).
    
        run addnode in hgenxml (iid_3
                               ,"c02_receita"
                               ,es-nota-fiscal-gnre.cod-receita
                               ,output iid_4).
    
        if es-conf-uf-gnre.det-receita then
            run addnode in hgenxml (iid_3
                                   ,"c25_detalhamentoReceita"
                                   ,string(es-conf-uf-gnre.cod-det-receita, '999999')
                                   ,output iid_4).

        if es-conf-uf-gnre.produto then
            run addnode in hgenxml (iid_3
                                   ,"c26_produto"
                                   ,string(es-conf-uf-gnre.cod-produto, '9999')
                                   ,output iid_4).
        
        if es-conf-uf-gnre.cont-emit then do:
            run addnode in hgenxml (iid_3
                                   ,"c27_tipoIdentificacaoEmitente"
                                   ,1
                                   ,output iid_4).
        
            run addnode in hgenxml (iid_3
                                   ,"c03_idContribuinteEmitente"
                                   ,""
                                   ,output iid_4).
                
            find estabelec no-lock where
                 estabelec.cod-estabel = nota-fiscal.cod-estabel no-error.
            if avail estabelec then
                run addnode in hgenxml (iid_4
                                       ,"CNPJ"
                                       ,estabelec.cgc
                                       ,output iid_5).
        end.

        if es-conf-uf-gnre.doc-orig then do:
            run addnode in hgenxml (iid_3
                                   ,"c28_tipoDocOrigem"
                                   ,string(es-conf-uf-gnre.cod-tp-docto, '99')
                                   ,output iid_4).
        
            run addnode in hgenxml (iid_3
                                   ,"c04_docOrigem"
                                   ,nota-fiscal.nr-nota-fis
                                   ,output iid_4).
        end.

        if es-conf-uf-gnre.tp-valor = 1 then
            run addnode in hgenxml (iid_3
                                   ,"c06_valorPrincipal"
                                   ,replace(trim(string(dec(es-nota-fiscal-gnre.vl-receita), '>>>>>>>>>9.99')),',','.')
                                   ,output iid_4).
        else if es-conf-uf-gnre.tp-valor = 2 or
                es-conf-uf-gnre.tp-valor = 3 then
            run addnode in hgenxml (iid_3
                                   ,"c10_valorTotal"
                                   ,replace(trim(string(dec(es-nota-fiscal-gnre.vl-receita), '>>>>>>>>>9.99')),',','.')
                                   ,output iid_4).
    
        if es-conf-uf-gnre.dat-vencto then
            run addnode in hgenxml (iid_3
                                   ,"c14_dataVencimento"
                                   ,string(year(es-lote-gnre.dt-vencto)) + '-' + string(month(es-lote-gnre.dt-vencto), '99') + '-' + string(day(es-lote-gnre.dt-vencto), '99')
                                   ,output iid_4).
    
        if es-conf-uf-gnre.convenio = 3 then
            run addnode in hgenxml (iid_3
                                   ,"c15_convenio"
                                   ,""
                                   ,output iid_4).
    
        if es-conf-uf-gnre.cont-emit then do:
            run addnode in hgenxml (iid_3
                                   ,"c16_razaoSocialEmitente"
                                   ,estabelec.nome
                                   ,output iid_4).
        
            /*run addnode in hgenxml (iid_3
                                   ,"c17_inscricaoEstadualEmitente"
                                   ,"..."
                                   ,output iid_4).*/
        
            run addnode in hgenxml (iid_3
                                   ,"c18_enderecoEmitente"
                                   ,estabelec.endereco
                                   ,output iid_4).
        
            find first mgcad.cidade no-lock where
                       cidade.cidade = estabelec.cidade no-error.
            if avail cidade then
                run addnode in hgenxml (iid_3
                                       ,"c19_municipioEmitente"
                                       ,substring(string(cidade.cdn-munpio-ibge),3,5)
                                       ,output iid_4).
        
            run addnode in hgenxml (iid_3
                                   ,"c20_ufEnderecoEmitente"
                                   ,estabelec.estado
                                   ,output iid_4).
        
            run addnode in hgenxml (iid_3
                                   ,"c21_cepEmitente"
                                   ,estabelec.cep
                                   ,output iid_4).
        
            /*run addnode in hgenxml (iid_3
                                   ,"c22_telefoneEmitente"
                                   ,"..."
                                   ,output iid_4).*/
        end.

        if es-conf-uf-gnre.cont-dest then do:
            find emitente no-lock where
                 emitente.nome-abrev = nota-fiscal.nome-ab-cli no-error.
            if avail emitente then do:
                run addnode in hgenxml (iid_3
                                       ,"c34_tipoIdentificacaoDestinatario"
                                       ,if emitente.natureza = 1 then 2 else 1
                                       ,output iid_4).
            
                run addnode in hgenxml (iid_3
                                       ,"c35_idContribuinteDestinatario"
                                       ,''
                                       ,output iid_4).
                    
                if emitente.natureza = 1 then
                    run addnode in hgenxml (iid_4
                                           ,"CPF"
                                           ,emitente.cgc
                                           ,output iid_5).
                else
                    run addnode in hgenxml (iid_4
                                           ,"CNPJ"
                                           ,emitente.cgc
                                           ,output iid_5).
            
                /*run addnode in hgenxml (iid_3
                                       ,"c36_inscricaoEstadualDestinatario"
                                       ,"..."
                                       ,output iid_4).*/
            
                run addnode in hgenxml (iid_3
                                       ,"c37_razaoSocialDestinatario"
                                       ,emitente.nome-emit
                                       ,output iid_4).

                find first cidade no-lock where
                           cidade.cidade = emitente.cidade no-error.
                if avail cidade then
                    run addnode in hgenxml (iid_3
                                           ,"c38_municipioDestinatario"
                                           ,substring(string(cidade.cdn-munpio-ibge),3,5)
                                           ,output iid_4).
            end.                               
        end.

        if es-conf-uf-gnre.dat-pagto then
            run addnode in hgenxml (iid_3
                                   ,"c33_dataPagamento"
                                   ,string(year(es-lote-gnre.dt-pagto)) + '-' + string(month(es-lote-gnre.dt-pagto), '99') + '-' + string(day(es-lote-gnre.dt-pagto), '99')
                                   ,output iid_4).
    
        if es-conf-uf-gnre.per-refer then do:
            run addnode in hgenxml (iid_3
                                   ,"c05_referencia"
                                   ,""
                                   ,output iid_4).
                
            if es-conf-uf-gnre.per-apur then do:
                run addnode in hgenxml (iid_4
                                       ,"periodo"
                                       ,0
                                       ,output iid_5).
            
                run addnode in hgenxml (iid_4
                                       ,"mes"
                                       ,string(month(today), '99')
                                       ,output iid_5).
            
                run addnode in hgenxml (iid_4
                                       ,"ano"
                                       ,string(year(today), '9999')
                                       ,output iid_5).

                if es-conf-uf-gnre.parcela then
                    run addnode in hgenxml (iid_4
                                           ,"parcela"
                                           ,'001'
                                           ,output iid_5).
            end.
        end.

        if es-conf-uf-gnre.chave-acesso then do:
            run addnode in hgenxml (iid_3
                                   ,"c39_camposExtras"
                                   ,""
                                   ,output iid_4).
        
            run addnode in hgenxml (iid_4
                                   ,"campoExtra"
                                   ,""
                                   ,output iid_5).
                
            run addnode in hgenxml (iid_5
                                   ,"codigo"
                                   ,es-conf-uf-gnre.cod-extra
                                   ,output iid_6).
        
            run addnode in hgenxml (iid_5
                                   ,"tipo"
                                   ,"T"
                                   ,output iid_6).
        
            run addnode in hgenxml (iid_5
                                   ,"valor"
                                   ,nota-fiscal.cod-chave-aces-nf-eletro
                                   ,output iid_6).
        end.
    
        /*run addnode in hgenxml (iid_3
                               ,"c42_identificadorGuia"
                               ,es-lote-gnre.cod-lote
                               ,output iid_4).*/
    end.
end.

procedure pi-le-retorno:

    def input param p-xml-retorno as longchar                no-undo.
    def input param p-cod-lote    like es-lote-gnre.cod-lote no-undo.

    def var c-codigo    as int  no-undo.
    def var c-descricao as char no-undo.
    def var c-numero    as int  no-undo.
    def var c-dt-rec    as char no-undo.
    def var c-tempo-est as char no-undo.
    def var i-seq-erro  as int  no-undo.
    
    output to 'c:\temp\log_ret_001.txt'.
    
    
    export p-xml-retorno.
    
    
    output close.

    def var hDocXmlRetorno as handle no-undo.
    
    if p-xml-retorno <> '' then do:
    
        create x-document hDocXmlRetorno.
    
        hDocXmlRetorno:load("longchar":U, p-xml-retorno, false).
    
        run reset in hGenXml.
    
        run loadxml in hGenXml (input hDocXmlRetorno).
        
        /* Localiza‡Æo do primeiro n¢ (n¢ raiz) */
        run searchTag in hGenXml (input "ns1:TRetLote_GNRE"
                                 ,input 0
                                 ,output iId_0).
                                 
        run searchTag in hGenXml (input "ns1:situacaoRecepcao"
                                 ,input iId_0
                                 ,output iId_1).
                                 
                                     
        if iId_1 <> ? then do:
        
            run getSonVal in hGenXml (input iId_1
                                     ,input "ns1:codigo"
                                     ,output c-codigo).
    
            run getSonVal in hGenXml (input iId_1
                                     ,input "ns1:descricao"
                                     ,output c-descricao).                             
        end.
    
    
        run searchTag in hGenXml (input "ns1:recibo"
                                 ,input iId_0
                                 ,output iId_1).
    
        if iId_1 <> ? then do:
    
            run getSonVal in hGenXml (input iId_1
                                     ,input "ns1:numero"
                                     ,output c-numero).
    
            run getSonVal in hGenXml (input iId_1
                                     ,input "ns1:dataHoraRecibo"
                                     ,output c-dt-rec).
    
            run getSonVal in hGenXml (input iId_1
                                     ,input "ns1:tempoEstimadoProc"
                                     ,output c-tempo-est).                                                        
    
        end.
        
        find last es-lote-gnre-erro no-lock where
                  es-lote-gnre-erro.cod-lote = p-cod-lote no-error.
        if avail es-lote-gnre-erro then
            assign i-seq-erro = es-lote-gnre-erro.seq-erro + 1.
        else
            assign i-seq-erro = 1.                        
        
        create es-lote-gnre-erro.
        assign es-lote-gnre-erro.cod-lote    = p-cod-lote
               es-lote-gnre-erro.seq-erro    = i-seq-erro
               es-lote-gnre-erro.cod-usuario = c-seg-usuario
               es-lote-gnre-erro.dt-proc     = today
               es-lote-gnre-erro.hr-proc     = string(time, "HH:MM:SS")
               es-lote-gnre-erro.cod-erro    = c-codigo
               es-lote-gnre-erro.des-erro    = c-descricao.
    
        if c-codigo = 100 and c-numero <> 0 then do:
            find es-lote-gnre exclusive-lock where
                 es-lote-gnre.cod-lote = p-cod-lote no-error.
            if avail es-lote-gnre then
                assign es-lote-gnre.num-recibo   = c-numero
                       es-lote-gnre.dt-proc      = c-dt-rec
                       es-lote-gnre.idi-situacao = 2.

            find current es-lote-gnre no-lock no-error.
        end.
    end.
    else do:
    
        find last es-lote-gnre-erro no-lock where
                  es-lote-gnre-erro.cod-lote = p-cod-lote no-error.
        if avail es-lote-gnre-erro then
            assign i-seq-erro = es-lote-gnre-erro.seq-erro + 1.
        else
            assign i-seq-erro = 1.                        
        
        create es-lote-gnre-erro.
        assign es-lote-gnre-erro.cod-lote    = p-cod-lote
               es-lote-gnre-erro.seq-erro    = i-seq-erro
               es-lote-gnre-erro.cod-usuario = c-seg-usuario
               es-lote-gnre-erro.dt-proc     = today
               es-lote-gnre-erro.hr-proc     = string(time, "HH:MM:SS")
               es-lote-gnre-erro.cod-erro    = 17006
               es-lote-gnre-erro.des-erro    = 'XML Inv lido':U.
            
    end.    
end.



