/*

Adapter Gera‡Æo XML Consulta Lote

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

    def input param p-cod-lote    like es-lote-gnre.cod-lote no-undo.
    def input param p-cod-estabel like estabelec.cod-estabel no-undo.
    def output param p-xml-envio  as longchar                no-undo.

    create x-document hdocxml.
    
    run reset in hgenxml.
    run setencoding in hgenxml ("utf-8").
    
    find es-lote-gnre no-lock where
         es-lote-gnre.cod-lote = p-cod-lote no-error.
    if avail es-lote-gnre then do:
    
        find es-param-gnre no-lock where
             es-param-gnre.cod-estabel = p-cod-estabel no-error.
    
        /* n¢ principal*/
        run addnode in hgenxml (0
                               ,"TConsLote_GNRE"
                               ,""
                               ,output iid_1).
    
        run addnode in hgenxml (iid_1
                               ,"ambiente"
                               ,es-param-gnre.ambiente
                               ,output iid_2).
    
        run addnode in hgenxml (iid_1
                               ,"numeroRecibo"
                               ,es-lote-gnre.num-recibo
                               ,output iid_2).
    
    
    end.
    
    run generatexml in hgenxml (output hdocxml).
    
    hdocxml:save ("longchar", p-xml-envio).
    
    delete object hdocxml   no-error.
    
    output to 'c:\temp\xml_002_envio.xml'.
    
    export p-xml-envio.
    
    output close.

end.

procedure pi-le-retorno:

    def input param p-cod-lote    like es-lote-gnre.cod-lote no-undo.
    def input param p-xml-retorno as longchar                no-undo.

    def var c-codigo    as int  no-undo.
    def var c-descricao as char no-undo.
    def var i-seq-erro  as int  no-undo.
    
    output to 'c:\temp\xml_002_ret.xml'.
    
    export p-xml-retorno.
    
    output close.

    def var hDocXmlRetorno as handle no-undo.
    
    
    if p-xml-retorno <> '' and
       p-xml-retorno matches '*<ns1:*' then do:
    
        create x-document hDocXmlRetorno.
    
        hDocXmlRetorno:load("longchar":U, p-xml-retorno, false).
            
        run reset in hGenXml.
    
        run loadxml in hGenXml (input hDocXmlRetorno).
        
        run searchTag in hGenXml (input "ns1:TresultLote_GNRE"
                                 ,input 0
                                 ,output iId_0).
                                 
        run searchTag in hGenXml (input "ns1:situacaoProcess"
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
        
        if c-codigo <> 0 then do:
            find es-lote-gnre exclusive-lock where
                 es-lote-gnre.cod-lote = p-cod-lote no-error.
            if avail es-lote-gnre then do:
            
                if c-codigo = 402 then
                    assign es-lote-gnre.idi-situacao = 3.
                    
                if c-codigo = 404 or c-codigo = 403 then
                    assign es-lote-gnre.idi-situacao = 1.    
            end.   

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



