def input param p-cod-uf      as char                    no-undo.
def input param p-cod-receita as char                    no-undo.
def input param p-cod-estabel like estabelec.cod-estabel no-undo.
def output param p-xml-ret    as longchar                no-undo.

def var NetServer as com-handle no-undo.


DEF STREAM s-log.

OUTPUT STREAM s-log TO 'C:\Totvs\Progress\OEwork\log_consulta.log'.

PUT STREAM s-log UNFORMATTED
    'p-cod-uf - ' p-cod-uf SKIP
    'p-cod-receita - ' p-cod-receita SKIP
    'p-cod-estabel - ' p-cod-estabel SKIP.


find es-param-gnre no-lock where
     es-param-gnre.cod-estabel = p-cod-estabel no-error.
if avail es-param-gnre then do:

    create "ProgressNet.NetServer" NetServer.
    assign p-xml-ret = NetServer:GetConfig(input es-param-gnre.ambiente,        /*ambiente (1 = Prod / 2 = Homol)*/
                                           input es-param-gnre.url-ws-conf,     /*URL WS*/
                                           input es-param-gnre.cod-certificado, /*certificado (numero)*/
                                           input p-cod-uf,                      /*UF*/
                                           input p-cod-receita,                 /*Receita*/
                                           input es-param-gnre.proxy-ip,        /*Proxy Address*/
                                           input es-param-gnre.proxy-porta,     /*Proxy Port*/
                                           input es-param-gnre.proxy-user,      /*Proxy User*/
                                           input es-param-gnre.proxy-pass).     /*Proxy Password*/

    EXPORT STREAM s-log p-xml-ret.

end.

OUTPUT STREAM s-log CLOSE.
