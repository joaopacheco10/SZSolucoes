/*
Adapter Gera‡Æo XML Consulta Config UF
*/
define variable hgenxml         as handle    no-undo.
define variable hmessagehandler as handle    no-undo.
define variable creturnvalue    as character no-undo.
define variable hdocxml         as handle    no-undo.
define variable iid_0           as integer   no-undo.
define variable iid_1           as integer   no-undo.
define variable iid_2           as integer   no-undo.
define variable iid_3           as integer   no-undo.
define variable iid_4           as integer   no-undo.
define variable iid_5           as integer   no-undo.
define variable iid_6           as integer   no-undo.
define variable iIDSon          as integer   no-undo.
define variable iIDSon_1        as integer   no-undo.
define variable iIDSon_2        as integer   no-undo.
define variable iIDSon_3        as integer   no-undo.

/* verifica se messagehandler.p est  na mem¢ria */
{xmlinc/xmlloadmessagehandler.i &messagehandler="hmessagehandler" &mhreturnvalue="creturnvalue"}

/* verifica se apixml esta na memoria */
{xmlinc/xmlloadgenxml.i &genxml="hgenxml" &gxreturnvalue="creturnvalue"}

/* verifica se o ut-genxml foi executado corretamente */
if creturnvalue <> "ok" then
    return creturnvalue.

procedure pi-gera-xml:

    def input param p-cod-estabel as char     no-undo.
    def input param p-cod-uf      as char     no-undo.
    def input param p-cod-receita as int      no-undo.
    def output param p-xml-envio  as longchar no-undo.

    create x-document hdocxml.
    
    run reset in hgenxml.
    run setencoding in hgenxml ("utf-8").
    
    /* n¢ principal*/
    run addnode in hgenxml (0
                           ,"TConsultaConfigUf"
                           ,""
                           ,output iid_1).
    
    find es-param-gnre no-lock where
         es-param-gnre.cod-estabel = p-cod-estabel no-error.
         
    message 'ambiente - ' es-param-gnre.ambiente
        view-as alert-box.
    
    run addnode in hgenxml (iid_1
                           ,"ambiente"
                           ,es-param-gnre.ambiente
                           ,output iid_2).
    
    run addnode in hgenxml (iid_1
                           ,"uf"
                           ,p-cod-uf
                           ,output iid_2).
    
    run addnode in hgenxml (iid_1
                           ,"receita"
                           ,p-cod-receita
                           ,output iid_2).
    
    run generatexml in hgenxml (output hdocxml).
    
    hdocxml:save ("longchar", p-xml-envio).
    
    delete object hdocxml   no-error.

end.

procedure pi-le-retorno:

    def input param p-xml-retorno as longchar no-undo.

    def var c-uf                 as char no-undo.
    def var c-cod-sit            as int  no-undo.
    def var c-des-sit            as char no-undo.
    def var c-cod-receita        as int  no-undo.
    def var c-des-receita        as char no-undo.
    def var c-exige-uf           as char no-undo.
    def var c-exige-receita      as char no-undo.
    def var c-exige-contrib-emit as char no-undo.
    def var c-exige-detalhe-rec  as char no-undo.
    def var c-cod-det-receita    as int  no-undo.
    def var c-des-det-receita    as char no-undo.
    def var c-exige-prod         as char no-undo.
    def var c-cod-prod           as int  no-undo.
    def var c-des-prod           as char no-undo.
    def var c-exige-per-ref      as char no-undo.
    def var c-exige-per-apur     as char no-undo.
    def var c-cod-per            as char no-undo.
    def var c-des-per            as char no-undo.
    def var c-exige-parc         as char no-undo.
    def var c-val-exig           as char no-undo.
    def var c-exige-doc-orig     as char no-undo.
    def var c-cod-tip-doc        as int  no-undo.
    def var c-des-tip-doc        as char no-undo.
    def var c-exige-contr-dest   as char no-undo.
    def var c-exige-dt-venc      as char no-undo.
    def var c-exige-dt-pagto     as char no-undo.
    def var c-exige-conv         as char no-undo.
    def var c-exige-camp-adic    as char no-undo.
    def var c-obrig-adic         as char no-undo.
    def var c-cod-adic           as char no-undo.
    def var c-tipo-adic          as char no-undo.
    def var c-tam-adic           as char no-undo.
    def var c-qt-casas-dec       as char no-undo.
    def var c-tit-adic           as char no-undo.
    def var c-exige-cont-emit-2  as char no-undo.
    def var c-exige-dt-venc-2    as char no-undo.
    def var c-exige-conv-2       as char no-undo.
    def var c-exige-dt-pagto-2   as char no-undo.

    def var hDocXmlRetorno as handle no-undo.
    

    if p-xml-retorno <> '' then do:        
        create x-document hDocXmlRetorno.
        
                
        hDocXmlRetorno:LOAD("longchar":U, p-xml-retorno, FALSE).
    
        run reset in hGenXml.
        
        run setencoding in hgenxml ("utf-8").      
        
        run loadxml in hGenXml (input hDocXmlRetorno).
        
        run searchTag in hGenXml (input "ns1:TConfigUf"
                                 ,input 0
                                 ,output iId_0).
        
        run getSonVal in hGenXml (input iId_0
                                 ,input "ns1:Uf"
                                 ,output c-uf).
        
        run searchTag in hGenXml (input "ns1:situacaoConsulta"
                                 ,input iId_0
                                 ,output iId_1).
        
        do while iId_1 <> ?:
            run getSonVal in hGenXml (input iId_1
                                     ,input "ns1:codigo"
                                     ,output c-cod-sit).
        
            run getSonVal in hGenXml (input iId_1
                                     ,input "ns1:descricao"
                                     ,output c-des-sit).
        
            run GetNextSonId in hGenXml (input iId_0
                                        ,input iIDSon_1
                                        ,output iId_1).
        end.
        
        if c-cod-sit = 450 then do:
            run getSonVal in hGenXml (input iId_0
                                     ,input "ns1:exigeUfFavorecida"
                                     ,output c-exige-uf).
            
            run getSonVal in hGenXml (input iId_0
                                     ,input "ns1:exigeReceita"
                                     ,output c-exige-receita).
            
            run searchTag in hGenXml (input "ns1:receitas"
                                     ,input iId_0
                                     ,output iId_1).
            
            do while iId_1 <> ?:
            
                run searchTag in hGenXml (input "ns1:receita"
                                         ,input iId_1
                                         ,output iId_2).
            
                do while iId_2 <> ?:
        
                    run getAttribute in hGenXml (input iId_2,
                                                 input "codigo",   
                                                 output c-cod-receita).                         
                
                    run getAttribute in hGenXml (input iId_2
                                                 ,input "descricao"
                                                 ,output c-des-receita).
        
                    if c-cod-receita = 100099 or
                       c-cod-receita = 100129 then do:
        
                        find es-conf-uf-gnre exclusive-lock where
                             es-conf-uf-gnre.cod-uf      = c-uf          and
                             es-conf-uf-gnre.cod-receita = c-cod-receita no-error.
                        if not avail es-conf-uf-gnre then do:
                            create es-conf-uf-gnre.
                            assign es-conf-uf-gnre.cod-uf      = c-uf
                                   es-conf-uf-gnre.cod-receita = c-cod-receita.
                        end.
                    
                        assign es-conf-uf-gnre.uf-favor = if c-exige-uf = 'S' then yes else no
                               es-conf-uf-gnre.receita  = if c-exige-receita = 'S' then yes else no.
                                                     
                        run getSonVal in hGenXml (input iId_2
                                                 ,input "ns1:exigeContribuinteEmitente"
                                                 ,output c-exige-contrib-emit).
                    
                        run getSonVal in hGenXml (input iId_2
                                                 ,input "ns1:exigeDetalhamentoReceita"
                                                 ,output c-exige-detalhe-rec).
                
                        assign es-conf-uf-gnre.det-receita = if c-exige-detalhe-rec = 'S' then yes else no
                               es-conf-uf-gnre.cont-emit   = if c-exige-contrib-emit = 'S' then yes else no.
                
                    
                        run searchTag in hGenXml (input "ns1:detalhamentosReceita"
                                                 ,input iId_2
                                                 ,output iId_3).
                                                 
                        do while iId_3 <> ?:
                    
                            run searchTag in hGenXml (input "ns1:detalhamentoReceita"
                                                     ,input iId_3
                                                     ,output iId_4).
                                                     
                            do while iId_4 <> ?:
                    
                                run getSonVal in hGenXml (input iId_4
                                                         ,input "ns1:codigo"
                                                         ,output c-cod-det-receita).
                    
                                run getSonVal in hGenXml (input iId_4
                                                         ,input "ns1:descricao"
                                                         ,output c-des-det-receita).
                
                                if not can-find(es-det-rec-gnre where
                                                es-det-rec-gnre.cod-uf          = es-conf-uf-gnre.cod-uf      and
                                                es-det-rec-gnre.cod-receita     = es-conf-uf-gnre.cod-receita and
                                                es-det-rec-gnre.cod-det-receita = c-cod-det-receita)          then do:
                                    create es-det-rec-gnre.
                                    assign es-det-rec-gnre.cod-uf          = es-conf-uf-gnre.cod-uf     
                                           es-det-rec-gnre.cod-receita     = es-conf-uf-gnre.cod-receita
                                           es-det-rec-gnre.cod-det-receita = c-cod-det-receita
                                           es-det-rec-gnre.des-det-receita = c-des-det-receita.
                                end.
                
                                                         
                                assign iIDSon_3 = iId_4.
                    
                                run GetNextSonId in hGenXml (input iId_3
                                                            ,input iIDSon_3
                                                            ,output iId_4).
                            end.
                    
                            assign iIDSon_2 = iId_3.
                    
                            run GetNextSonId in hGenXml (input iId_2
                                                        ,input iIDSon_2
                                                        ,output iId_3).
                        end.
                    
                        run getSonVal in hGenXml (input iId_2
                                                 ,input "ns1:exigeProduto"
                                                 ,output c-exige-prod).
                
                        assign es-conf-uf-gnre.produto = if c-exige-prod = 'S' then yes else no.
                    
                        run searchTag in hGenXml (input "ns1:produtos"
                                                 ,input iId_2
                                                 ,output iId_3).
                    
                        do while iId_3 <> ?:
                    
                            run searchTag in hGenXml (input "ns1:produto"
                                                     ,input iId_3
                                                     ,output iId_4).
                        
                            do while iId_4 <> ?:
                    
                                run getSonVal in hGenXml (input iId_4
                                                         ,input "ns1:codigo"
                                                         ,output c-cod-prod).
                    
                                run getSonVal in hGenXml (input iId_4
                                                         ,input "ns1:descricao"
                                                         ,output c-des-prod).
                
                                if not can-find(es-prod-gnre where
                                                es-prod-gnre.cod-uf      = es-conf-uf-gnre.cod-uf      and
                                                es-prod-gnre.cod-receita = es-conf-uf-gnre.cod-receita and
                                                es-prod-gnre.cod-produto = c-cod-prod)                 then do:
                
                                    create es-prod-gnre.
                                    assign es-prod-gnre.cod-uf      = es-conf-uf-gnre.cod-uf
                                           es-prod-gnre.cod-receita = es-conf-uf-gnre.cod-receita
                                           es-prod-gnre.cod-produto = c-cod-prod
                                           es-prod-gnre.des-produto = c-des-prod.
                
                                end.
                                                         
                                assign iIDSon_3 = iId_4.
                    
                                run GetNextSonId in hGenXml (input iId_3
                                                            ,input iIDSon_3
                                                            ,output iId_4).
                            end.
                    
                            assign iIDSon_2 = iId_3.
                    
                            run GetNextSonId in hGenXml (input iId_2
                                                        ,input iIDSon_2
                                                        ,output iId_3).
                        end.
                    
                        run getSonVal in hGenXml (input iId_2
                                                 ,input "ns1:exigePeriodoReferencia"
                                                 ,output c-exige-per-ref).
                        
                        run getSonVal in hGenXml (input iId_2
                                                 ,input "ns1:exigePeriodoApuracao"
                                                 ,output c-exige-per-apur).
                
                        assign es-conf-uf-gnre.per-apur  = if c-exige-per-apur = 'S' then yes else no
                               es-conf-uf-gnre.per-refer = if c-exige-per-ref = 'S' then yes else no.
                    
                        run getSonVal in hGenXml (input iId_2
                                                 ,input "ns1:exigeParcela"
                                                 ,output c-exige-parc).
                        
                        run getSonVal in hGenXml (input iId_2
                                                 ,input "ns1:valorExigido"
                                                 ,output c-val-exig).
                        
                        run getSonVal in hGenXml (input iId_2
                                                 ,input "ns1:exigeDocumentoOrigem"
                                                 ,output c-exige-doc-orig).
                
                        assign es-conf-uf-gnre.parcela  = if c-exige-parc = 'S' then yes else no
                               es-conf-uf-gnre.doc-orig = if c-exige-doc-orig = 'S' then yes else no.
                
                        if c-val-exig = 'P' then
                            assign es-conf-uf-gnre.tp-valor = 1.
                        else if c-val-exig = 'T' then
                            assign es-conf-uf-gnre.tp-valor = 2.
                        else if c-val-exig = 'A' then
                            assign es-conf-uf-gnre.tp-valor = 3.
                        else if c-val-exig = 'N' then
                            assign es-conf-uf-gnre.tp-valor = 4.
                
                        run searchTag in hGenXml (input "ns1:tiposDocumentosOrigem"
                                                 ,input iId_2
                                                 ,output iId_3).
                    
                        do while iId_3 <> ?:
                    
                            run searchTag in hGenXml (input "ns1:tipoDocumentoOrigem"
                                                     ,input iId_3
                                                     ,output iId_4).
                        
                            do while iId_4 <> ?:
                    
                                run getSonVal in hGenXml (input iId_4
                                                         ,input "ns1:codigo"
                                                         ,output c-cod-tip-doc).
                    
                                run getSonVal in hGenXml (input iId_4
                                                         ,input "ns1:descricao"
                                                         ,output c-des-tip-doc).
                
                
                                if not can-find(es-tp-docto-gnre where
                                                es-tp-docto-gnre.cod-uf      = es-conf-uf-gnre.cod-uf      and
                                                es-tp-docto-gnre.cod-receita = es-conf-uf-gnre.cod-receita and
                                                es-tp-docto-gnre.cod-tp-docto = c-cod-tip-doc)              then do:
                                    create es-tp-docto-gnre.
                                    assign es-tp-docto-gnre.cod-uf       = es-conf-uf-gnre.cod-uf
                                           es-tp-docto-gnre.cod-receita  = es-conf-uf-gnre.cod-receita
                                           es-tp-docto-gnre.cod-tp-docto = c-cod-tip-doc
                                           es-tp-docto-gnre.des-tp-docto = c-des-tip-doc.
                                end.
                                                         
                                assign iIDSon_3 = iId_4.
                    
                                run GetNextSonId in hGenXml (input iId_3
                                                            ,input iIDSon_3
                                                            ,output iId_4).
                            end.
                    
                            assign iIDSon_2 = iId_3.
                    
                            run GetNextSonId in hGenXml (input iId_2
                                                        ,input iIDSon_2
                                                        ,output iId_3).
                        end.
                    
                        run getSonVal in hGenXml (input iId_2
                                                 ,input "ns1:exigeContribuinteDestinatario"
                                                 ,output c-exige-contr-dest).
                        
                        run getSonVal in hGenXml (input iId_2
                                                 ,input "ns1:exigeDataVencimento"
                                                 ,output c-exige-dt-venc).
                        
                        run getSonVal in hGenXml (input iId_2
                                                 ,input "ns1:exigeDataPagamento"
                                                 ,output c-exige-dt-pagto).
                    
                        run getSonVal in hGenXml (input iId_2
                                                 ,input "ns1:exigeConvenio"
                                                 ,output c-exige-conv).
                        
                        run getSonVal in hGenXml (input iId_2
                                                 ,input "ns1:exigeCamposAdicionais"
                                                 ,output c-exige-camp-adic).
                
                        assign es-conf-uf-gnre.cont-dest  = if c-exige-contr-dest = 'S' then yes else no
                               es-conf-uf-gnre.dat-pagto  = if c-exige-dt-pagto = 'S' then yes else no
                               es-conf-uf-gnre.dat-vencto = if c-exige-dt-venc = 'S' then yes else no.
                
                        if c-exige-conv = 'N' then
                            assign es-conf-uf-gnre.convenio = 1.
                        else if c-exige-conv = 'S' then
                            assign es-conf-uf-gnre.convenio = 2.
                        else if c-exige-conv = 'O' then
                            assign es-conf-uf-gnre.convenio = 3.
                                                 
                        run searchTag in hGenXml (input "ns1:camposAdicionais"
                                                 ,input iId_2
                                                 ,output iId_3).
                    
                        do while iId_3 <> ?:
                    
                            run searchTag in hGenXml (input "ns1:campoAdicional"
                                                     ,input iId_3
                                                     ,output iId_4).
                        
                            do while iId_4 <> ?:
                    
                                run getSonVal in hGenXml (input iId_4
                                                         ,input "ns1:obrigatorio"
                                                         ,output c-obrig-adic).
                    
                                run getSonVal in hGenXml (input iId_4
                                                         ,input "ns1:codigo"
                                                         ,output c-cod-adic).
                    
                                run getSonVal in hGenXml (input iId_4
                                                         ,input "ns1:tipo"
                                                         ,output c-tipo-adic).
                    
                                run getSonVal in hGenXml (input iId_4
                                                         ,input "ns1:tamanho"
                                                         ,output c-tam-adic).
                    
                                run getSonVal in hGenXml (input iId_4
                                                         ,input "ns1:casasDecimais"
                                                         ,output c-qt-casas-dec).
                    
                                run getSonVal in hGenXml (input iId_4
                                                         ,input "ns1:titulo"
                                                         ,output c-tit-adic).
                
                                if c-tit-adic   = 'Chave de Acesso da NFe' and
                                   c-obrig-adic = 'S'                      then
                                    assign es-conf-uf-gnre.chave-acesso = yes
                                           es-conf-uf-gnre.cod-extra    = c-cod-adic.
                
                                                         
                                assign iIDSon_3 = iId_4.
                    
                                run GetNextSonId in hGenXml (input iId_3
                                                            ,input iIDSon_3
                                                            ,output iId_4).
                            end.
                    
                            assign iIDSon_2 = iId_3.
                    
                            run GetNextSonId in hGenXml (input iId_2
                                                        ,input iIDSon_2
                                                        ,output iId_3).
                    
                        end.
                    end.
                
                    assign iIDSon_1 = iId_2.
                
                    run GetNextSonId in hGenXml (input iId_1
                                                ,input iIDSon_1
                                                ,output iId_2).
                end.            
                
                assign iIDSon = iId_1.
                
                run GetNextSonId in hGenXml (input iId_0
                                            ,input iIDSon
                                            ,output iId_1).
            end.
        end.
    end.
end.
