def input param p-xml-lote    as longchar                no-undo.
def input param p-cod-estabel like estabelec.cod-estabel no-undo.
def output param p-xml-ret    as longchar                no-undo.

def var NetServer as com-handle no-undo.

find es-param-gnre no-lock where
     es-param-gnre.cod-estabel = p-cod-estabel no-error.
if avail es-param-gnre then do:

    create "ProgressNet.NetServer" NetServer.
    assign p-xml-ret = NetServer:EnviaLote(input es-param-gnre.ambiente,        /*ambiente (1 = Prod / 2 = Homol)*/
                                           input es-param-gnre.url-ws-env,      /*URL WS*/
                                           input es-param-gnre.cod-certificado, /*certificado (numero)*/
                                           input p-xml-lote,                    /*xml*/
                                           input es-param-gnre.proxy-ip,        /*Proxy Address*/
                                           input es-param-gnre.proxy-porta,     /*Proxy Port*/
                                           input es-param-gnre.proxy-user,      /*Proxy User*/
                                           input es-param-gnre.proxy-pass).     /*Proxy Password*/

end.
