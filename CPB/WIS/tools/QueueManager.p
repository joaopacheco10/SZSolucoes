define input param p-cod-trans as char no-undo.
define input param p-chave     as char no-undo.

define buffer bfila-wms for esp-fila-wms.
define variable c-chave as char no-undo.
define variable i-id like esp-fila-wms.id no-undo.

do while true:
    find last bfila-wms use-index id no-lock no-error.
    
    if avail bfila-wms then 
        assign i-id = bfila-wms.id + 1.
    else 
        assign i-id = 1.
        
    find bfila-wms no-lock where
         bfila-wms.id = i-id no-error.
    if not avail bfila-wms then
        leave.         

end.



create esp-fila-wms.
assign esp-fila-wms.cod-trans            = p-cod-trans
       esp-fila-wms.data-hora-criacao    = now
       esp-fila-wms.chave                = p-chave
       esp-fila-wms.id                   = i-id
       esp-fila-wms.data-hora-integracao = ?.
