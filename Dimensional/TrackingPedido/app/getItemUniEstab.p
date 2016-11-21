/*
CD0140 (Lote M¡nimo / M£ltiplo e Lead Time)
envia c¢d do estabelecimento e o c¢digo do item.
O Lead Time ‚ igual ao "Ressupr Fornec" + "Ressupr Compras"
*/


def temp-table tt-item no-undo
    field it-codigo    like item-uni-estab.it-codigo   
    field cod-estabel  like item-uni-estab.cod-estabel 
    field lote-minimo  like item-uni-estab.lote-minimo 
    field lote-multipl like item-uni-estab.lote-multipl
    field lead-time    like item-uni-estab.res-for-comp.

def input param p-it-codigo   like item-uni-estab.it-codigo   no-undo.
def input param p-cod-estabel like item-uni-estab.cod-estabel no-undo.
def output param table for tt-item.


find item-uni-estab no-lock where
     item-uni-estab.it-codigo   = p-it-codigo   and
     item-uni-estab.cod-estabel = p-cod-estabel no-error.
if avail item-uni-estab then do:

    create tt-item.
    assign tt-item.it-codigo    = item-uni-estab.it-codigo
           tt-item.cod-estabel  = item-uni-estab.cod-estabel
           tt-item.lote-minimo  = item-uni-estab.lote-minimo
           tt-item.lote-multipl = item-uni-estab.lote-multipl
           tt-item.lead-time    = item-uni-estab.res-for-comp + item-uni-estab.res-int-comp.
end.
