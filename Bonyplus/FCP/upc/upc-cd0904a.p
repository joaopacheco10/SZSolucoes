/**************************************************************************
**   Programa: upc/upc-cd0904a.p
**   Data    : Mar‡os/2016
**   Autor   : Joao Pacheco
**   Objetivo: UPC Campos Gera‡Æo do c lculo do Fundo combate … pobreza.
**   Versao..: 
**************************************************************************/

define input param p-ind-event      as char          no-undo.
define input param p-ind-object     as char          no-undo.
define input param p-wgh-object     as handle        no-undo.
define input param p-wgh-frame      as widget-handle no-undo.
define input param p-cod-table      as char          no-undo.
define input param p-row-table      as rowid         no-undo.

/*message "Evento..........:" STRING(p-ind-event)    SKIP
        "Objeto..........:" STRING(p-ind-object)   SKIP
        "Handle do Objeto:" STRING(p-wgh-object)   SKIP
        "Handle da Frame.:" STRING(p-wgh-frame)    SKIP
        "Tabela..........:" p-cod-table            SKIP
        "Rowid...........:" STRING(p-row-table)    SKIP
        "p-wgh-object....:" p-wgh-object:file-name VIEW-AS ALERT-BOX.*/


def var c-objeto     as char          no-undo.
def var hperSubTri   as widget-handle no-undo.
def var hLabelFcp    as widget-handle no-undo.
def var hPercFcp     as widget-handle no-undo.
def var hLabelDesImp as widget-handle no-undo.
def var hDesImposto  as widget-handle no-undo.
def var hBtFacilt    as widget-handle no-undo.
def var hBtMsgFcp    as widget-handle no-undo.

def new global shared var g-row-item-uf-cd0904a as rowid no-undo.

assign c-objeto = entry(num-entries(p-wgh-object:private-data, "~/"), p-wgh-object:private-data, "~/"). 

if  p-ind-event  = 'choose':u   and
    p-ind-object = 'btMsgFcp':u then do:

    current-window:sensitive = false.

    run upc/upc-cd0904a1.w (input g-row-item-uf-cd0904a).

    current-window:sensitive = true.

end.

if  p-ind-event  = 'initialize':u and
    p-ind-object = 'container':u  then do:

    run findWidget (input 'per-sub-tri':u, 
                    input  'fill-in':u,
                    input  p-wgh-frame,  
                    output hperSubTri).
                    
    run findWidget (input 'bt-facilit':u, 
                    input  'button':u,
                    input  p-wgh-frame,  
                    output hBtFacilt).                
                    
                    
    create button hBtMsgFcp
    assign  frame     = hBtFacilt:frame
            width     = 4
            height    = 1.20
            row       = hBtFacilt:row
            col       = hBtFacilt:col + 5
            label     = "Mensagem FCP"
            tooltip   = "Mensagem FCP"
            sensitive = hBtFacilt:sensitive
            visible   = yes
            name      = "btMsgFcp".

    hBtMsgFcp:load-image-up("image\toolbar\im-comments.bmp").
    hBtMsgFcp:load-image-insensitive("image\toolbar\ii-comments.bmp").
    

    on "choose":U of hBtMsgFcp persistent run upc/upc-cd0904a.p (input "choose",
                                                                 input "btMsgFcp",
                                                                 input p-wgh-object,
                                                                 input p-wgh-frame,
                                                                 input p-cod-table,
                                                                 input p-row-table).

    create text hLabelFcp
    assign name      = 'lbl_fcp':u
           frame     = hperSubTri:frame
           row       = hperSubTri:row + 0.1
           format    = 'x(7)'
           col       = hperSubTri:col + 30.8
           width     = 7
           screen-value = '% FCP:'
           font      = hperSubTri:font
           fgcolor   = hperSubTri:fgcolor
           visible   = yes.

    create fill-in hPercFcp
    assign name      = 'perc_fcp':u
           frame     = hperSubTri:frame 
           width     = 5
           height    = hperSubTri:height 
           column    = hperSubTri:col + 36
           row       = hperSubTri:row
           font      = hperSubTri:font 
           fgcolor   = hperSubTri:fgcolor 
           data-type = "DECIMAL":U
           format    = ">>9.99"
           side-label-handle = hLabelFcp
           label     = '% FCP:'
           help      = 'Percentual FCP'
           tooltip   = 'Percentual FCP'
           visible   = yes.  
           
    
    create text hLabelDesImp
    assign name      = 'lbl_des_imp':u
           frame     = hperSubTri:frame
           row       = hperSubTri:row + 1.1
           format    = 'x(14)'
           col       = hperSubTri:col + 25.3
           width     = 14
           screen-value = 'Descri‡Æo FCP:':U
           font      = hperSubTri:font
           fgcolor   = hperSubTri:fgcolor
           visible   = yes.

    create fill-in hDesImposto
    assign name      = 'des_imposto':u
           frame     = hperSubTri:frame 
           width     = 16
           height    = hperSubTri:height 
           column    = hperSubTri:col + 36
           row       = hperSubTri:row + 1
           font      = hperSubTri:font 
           fgcolor   = hperSubTri:fgcolor 
           data-type = "CHARACTER":U
           format    = "x(20)"
           side-label-handle = hLabelDesImp
           label     = 'Descri‡Æo FCP:':U
           help      = 'Descri‡Æo Imposto'
           tooltip   = 'Descri‡Æo Imposto'
           visible   = yes.         

end.

if p-ind-event = 'display':U and 
   p-ind-object = 'viewer':u then do: 

    assign g-row-item-uf-cd0904a = ?.

    if p-row-table <> ? then do:
        find item-uf no-lock where 
             rowid(item-uf) = p-row-table no-error.
        if avail item-uf then do:

            assign g-row-item-uf-cd0904a = p-row-table.

            run getHandle.

            if valid-handle(hPercFcp) then do:
                
                find ext-item-uf-fcp no-lock where
                     ext-item-uf-fcp.it-codigo       = item-uf.it-codigo       and
                     ext-item-uf-fcp.cod-estado-orig = item-uf.cod-estado-orig and
                     ext-item-uf-fcp.estado          = item-uf.estado          and
                     ext-item-uf-fcp.cod-estab       = item-uf.cod-estab       no-error.
                if avail ext-item-uf-fcp then
                    assign hPercFcp:screen-value     = string(ext-item-uf-fcp.perc-fcp)
                           hDesImposto:screen-value  = ext-item-uf-fcp.des-imp.
                else
                    assign hPercFcp:screen-value     = '0'
                           hDesImposto:screen-value  = ''.
            end.
        end.
    end.
end.

if (p-ind-event = 'after-disable':u or  
    p-ind-event = 'after-enable':u) then do:

    run getHandle.

    if valid-handle(hPercFcp) then
        assign hPercFcp:sensitive = (p-ind-event = 'after-enable':u).
        
    if valid-handle(hDesImposto) then
        assign hDesImposto:sensitive = (p-ind-event = 'after-enable':u).   
end.

if p-ind-event  = 'add':u    and 
   p-ind-object = 'viewer':u then do:

    run getHandle.

    if valid-handle(hPercFcp) then
        assign hPercFcp:screen-value = '0':u.
               hPercFcp:label = hPercFcp:private-data.

    if valid-handle(hLabelFcp) then
        assign hLabelFcp:screen-value = '% FCP:':U.
        
    if valid-handle(hDesImposto) then
        assign hDesImposto:screen-value = '':u.
               hDesImposto:label = hDesImposto:private-data.

    if valid-handle(hLabelDesImp) then
        assign hLabelDesImp:screen-value = 'Descri‡Æo FCP:':U.    
end.

if p-ind-event  = 'assign':u and 
   p-ind-object = 'viewer':u then do:

    find item-uf no-lock where 
         rowid(item-uf) = p-row-table no-error.
    if avail item-uf then do:

        run getHandle.

        find ext-item-uf-fcp exclusive-lock where
             ext-item-uf-fcp.it-codigo       = item-uf.it-codigo       and
             ext-item-uf-fcp.cod-estado-orig = item-uf.cod-estado-orig and
             ext-item-uf-fcp.estado          = item-uf.estado          and
             ext-item-uf-fcp.cod-estab       = item-uf.cod-estab       no-error.
        if not avail ext-item-uf-fcp then do:
            create ext-item-uf-fcp.
            assign ext-item-uf-fcp.it-codigo       = item-uf.it-codigo      
                   ext-item-uf-fcp.cod-estado-orig = item-uf.cod-estado-orig
                   ext-item-uf-fcp.estado          = item-uf.estado         
                   ext-item-uf-fcp.cod-estab       = item-uf.cod-estab.
        end.

        assign ext-item-uf-fcp.perc-fcp    = hPercFcp:input-value
               ext-item-uf-fcp.des-imp     = hDesImposto:input-value.

        find current ext-item-uf-fcp no-lock no-error.
    end.
end.

return 'ok':u.

procedure getHandle private:

    run findWidget (input 'perc_fcp':u, 
                    input 'fill-in':u,
                    input p-wgh-frame, 
                    output hPercFcp).

    run findWidget (input 'lbl_fcp':u, 
                    input 'text':u,
                    input p-wgh-frame, 
                    output hLabelFcp).
                    
    run findWidget (input 'des_imposto':u, 
                    input 'fill-in':u,
                    input p-wgh-frame, 
                    output hDesImposto).

    run findWidget (input 'lbl_des_imp':u, 
                    input 'text':u,
                    input p-wgh-frame, 
                    output hLabelDesImp).                
end.

procedure findWidget:

    define input  parameter c-widget-name  as char   no-undo.
    define input  parameter c-widget-type  as char   no-undo.
    define input  parameter h-start-widget as handle no-undo.
    define output parameter h-widget       as handle no-undo.
    
    do while valid-handle(h-start-widget):
        if h-start-widget:name = c-widget-name and
           h-start-widget:type = c-widget-type then do:
            assign h-widget = h-start-widget:handle.
            leave.
        end.
    
        if h-start-widget:type = "field-group":u or
           h-start-widget:type = "frame":u or
           h-start-widget:type = "dialog-box":u then do:
            run findWidget (input  c-widget-name,
                            input  c-widget-type,
                            input  h-start-widget:first-child,
                            output h-widget).
    
            if valid-handle(h-widget) then
                leave.
        end.
        assign h-start-widget = h-start-widget:next-sibling.
    end.

end.

