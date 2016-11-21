/**************************************************************************
**   Programa: upc/u01eq0506.p
**   Data    : Maio 2015
**   Autor   : Joao Pacheco - SZ Solu‡äes
**   Objetivo: Se grupo de usu rios NÇO FOR IGUAL A ?Sac? E imp_pedidos.envio = ?yes?
**             apresentar o pedido para embarque na tela acima (EQ0506B)
**   Versao..: 
**************************************************************************/

{include/i-bfems2.i}
{include/i-prgvrs.i u01eq0506b-b01.p 2.11.00.001}

{tools/fc-handle-obj.i}
{tools/fc-falso.i}
{utp/ut-glob.i}

DEF INPUT PARAMETER p-ind-event  AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-ind-object AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-wgh-object AS HANDLE            NO-UNDO.
DEF INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE     NO-UNDO.
DEF INPUT PARAMETER p-cod-table  AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-row-table  AS ROWID             NO-UNDO.


if p-ind-event = "CREATE-TEMP-TABLE-ped-venda" then do:

    find ped-venda no-lock where
         rowid(ped-venda) = p-row-table no-error.
    if avail ped-venda then do:

        if not can-find(first usuar_grp_usuar where
                              usuar_grp_usuar.cod_usuario   = ped-venda.user-impl and
                             (usuar_grp_usuar.cod_grp_usuar = "SAC"   or
                              usuar_grp_usuar.cod_grp_usuar = "GDD")) then do:            
            find imp_pedidos no-lock where
                 imp_pedidos.nr-pedido = ped-venda.nr-pedido no-error.
            if not avail imp_pedidos then
                return "NOK".   
            else
                if imp_pedidos.envio = no then
                    return "NOK".
                else
                    return "OK".      
        end.
        else
            return "OK". 
    end.
end.
