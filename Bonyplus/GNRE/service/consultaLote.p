/*def input param p-xml-lote    as longchar                no-undo.*/
DEF INPUT PARAM p-recibo      LIKE es-lote-gnre.num-recibo NO-UNDO.
def input param p-cod-estabel like estabelec.cod-estabel no-undo.
def output param p-xml-ret    as longchar                no-undo.

def var NetServer as com-handle no-undo.

find es-param-gnre no-lock where
     es-param-gnre.cod-estabel = p-cod-estabel no-error.
if avail es-param-gnre then do:

    create "ProgressNet.NetServer" NetServer.
    assign p-xml-ret = NetServer:ConsultaRecibo(input es-param-gnre.ambiente,        /*ambiente (1 = Prod / 2 = Homol)*/
                                                input es-param-gnre.url-ws-cons,     /*URL WS*/
                                                input es-param-gnre.cod-certificado, /*certificado (numero)*/
                                                /*input p-xml-lote,                    /*xml*/*/
                                                INPUT p-recibo,
                                                input es-param-gnre.proxy-ip,        /*Proxy Address*/
                                                input es-param-gnre.proxy-porta,     /*Proxy Port*/
                                                input es-param-gnre.proxy-user,      /*Proxy User*/
                                                input es-param-gnre.proxy-pass).     /*Proxy Password*/

end.

