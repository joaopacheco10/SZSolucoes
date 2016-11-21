/*************************************************
**                                              **
** Programa...: esco027                         **
** Data     ..: Fevereiro/2014                  **
** Autor      : SZ - Eder                       **
** Descriá∆o..: Reativar etiqueta devolvida     **
**                                              **
**************************************************/

def var i-etq as decimal format "99999999999999" no-undo
     label "Etiq" view-as fill-in size 14 by .88.

def var c-localiz as char format 'x(20)' no-undo
    label 'Localiz' view-as fill-in size 15 by .88.

def var i-cont as integer no-undo.

def new global shared var i-ep-codigo-usuario   like mguni.empresa.ep-codigo          no-undo.
def new global shared var c-seg-usuario as char no-undo.
/*{utp/ut-glob.i}*/
{esco/esco000.i}
/************************ Principal ***************************/

Define Frame f-devol
    i-etq       At Row 3 Col 1 auto-return
    c-localiz   at row 4 col 1
    "<F4 P/ Sair>" at row 8 col 1
    With Side-labels  Attr-space Row 1 size 21 by 12 no-box.

Repeat On Endkey Undo, leave transaction:

    hide message no-pause.

    update i-etq
           c-localiz
         with frame f-devol.

    i-cont = 0.
    find es-etiqueta-pai exclusive-lock where
         es-etiqueta-pai.cod-etiq-pai = string(i-etq,"99999999999999") no-error.
    if avail es-etiqueta-pai then do:
        i-cont = i-cont + 1.

        for each es-etiqueta exclusive-lock
           where es-etiqueta.cod-etiq-pai = es-etiqueta-pai.cod-etiq-pai:

            assign es-etiqueta.serie       = ""                   
                   es-etiqueta.nr-nota-fis = ""                 
                   es-etiqueta.qt-alocada  = 0                   
                   es-etiqueta.usuar-armaz = ""                   
                   es-etiqueta.data-armaz  = ?                   
                   es-etiqueta.hora-armaz  = ""                   
                   es-etiqueta.usuar-conf  = ""                   
                   es-etiqueta.data-conf   = ?                   
                   es-etiqueta.hora-conf   = ""                   
                   es-etiqueta.nr-romaneio = 0
                   es-etiqueta.situacao    = 3.

            find es-saldo-estoq exclusive-lock where 
                 es-saldo-estoq.cod-estabel = es-etiqueta-pai.cod-estabel and 
                 es-saldo-estoq.it-codigo   = es-etiqueta-pai.it-codigo   and 
                 es-saldo-estoq.cod-depos   = "EXL"                       and 
                 es-saldo-estoq.lote        = es-etiqueta-pai.lote        and 
                 es-saldo-estoq.cod-localiz = c-localiz                   no-error.
            if avail es-saldo-estoq then
                assign es-saldo-estoq.qt-total = es-saldo-estoq.qt-total + es-etiqueta.qt-orig
                       es-saldo-estoq.qt-saldo = es-saldo-estoq.qt-saldo + es-etiqueta.qt-orig.
            else do:
                create es-saldo-estoq.
                assign es-saldo-estoq.cod-estabel = es-etiqueta-pai.cod-estabel
                       es-saldo-estoq.it-codigo   = es-etiqueta-pai.it-codigo  
                       es-saldo-estoq.cod-depos   = 'EXL'                
                       es-saldo-estoq.lote        = es-etiqueta-pai.lote       
                       es-saldo-estoq.cod-localiz = c-localiz 
                       es-saldo-estoq.qt-total    = es-etiqueta.qt-orig
                       es-saldo-estoq.qt-saldo    = es-etiqueta.qt-orig.
            end.

            for each es-ped-etiqueta exclusive-lock
               where es-ped-etiqueta.cod-etiqueta = es-etiqueta.cod-etiqueta:
                delete es-ped-etiqueta.
            end.

            i-cont = i-cont + 1.
        end.

        assign es-etiqueta-pai.serie       = ""                   
               es-etiqueta-pai.nr-nota-fis = ""                   
               es-etiqueta-pai.nr-romaneio = 0
               es-etiqueta-pai.situacao    = 3.

        for each es-ped-etiqueta exclusive-lock
           where es-ped-etiqueta.cod-etiqueta = es-etiqueta-pai.cod-etiq-pai:
            delete es-ped-etiqueta.
        end.
        
    end.
    else do:
        find es-etiqueta exclusive-lock where
             es-etiqueta.cod-etiqueta = string(i-etq,"99999999999999") no-error.
        if avail es-etiqueta then do:
            assign es-etiqueta.serie       = ""                   
                   es-etiqueta.nr-nota-fis = ""
                   es-etiqueta.qt-alocada  = 0                   
                   es-etiqueta.usuar-armaz = ""                   
                   es-etiqueta.data-armaz  = ?                   
                   es-etiqueta.hora-armaz  = ""                   
                   es-etiqueta.usuar-conf  = ""                   
                   es-etiqueta.data-conf   = ?                   
                   es-etiqueta.hora-conf   = ""                   
                   es-etiqueta.nr-romaneio = 0
                   es-etiqueta.situacao    = 3.

            find es-etiqueta-pai no-lock where
                 es-etiqueta-pai.cod-etiq-pai = es-etiqueta.cod-etiq-pai no-error.
            if avail es-etiqueta-pai then do:
                find es-saldo-estoq exclusive-loc where 
                     es-saldo-estoq.cod-estabel = es-etiqueta-pai.cod-estabel and 
                     es-saldo-estoq.it-codigo   = es-etiqueta-pai.it-codigo   and 
                     es-saldo-estoq.cod-depos   = "EXL"                       and 
                     es-saldo-estoq.lote        = es-etiqueta-pai.lote        and 
                     es-saldo-estoq.cod-localiz = c-localiz                   no-error.
                if avail es-saldo-estoq then
                    assign es-saldo-estoq.qt-total = es-saldo-estoq.qt-total + es-etiqueta.qt-orig
                           es-saldo-estoq.qt-saldo = es-saldo-estoq.qt-saldo + es-etiqueta.qt-orig.
                else do:
                    create es-saldo-estoq.
                    assign es-saldo-estoq.cod-estabel = es-etiqueta-pai.cod-estabel
                           es-saldo-estoq.it-codigo   = es-etiqueta-pai.it-codigo  
                           es-saldo-estoq.cod-depos   = 'EXL'                
                           es-saldo-estoq.lote        = es-etiqueta-pai.lote       
                           es-saldo-estoq.cod-localiz = c-localiz 
                           es-saldo-estoq.qt-total    = es-etiqueta.qt-orig
                           es-saldo-estoq.qt-saldo    = es-etiqueta.qt-orig.
                end.
            end.
            else do:
                find es-saldo-estoq exclusive-loc where 
                     es-saldo-estoq.cod-estabel = es-etiqueta.cod-estabel and 
                     es-saldo-estoq.it-codigo   = es-etiqueta.it-codigo   and 
                     es-saldo-estoq.cod-depos   = "EXL"                   and 
                     es-saldo-estoq.lote        = es-etiqueta.lote        and 
                     es-saldo-estoq.cod-localiz = c-localiz               no-error.
                if avail es-saldo-estoq then
                    assign es-saldo-estoq.qt-total = es-saldo-estoq.qt-total + es-etiqueta.qt-orig
                           es-saldo-estoq.qt-saldo = es-saldo-estoq.qt-saldo + es-etiqueta.qt-orig.
                else do:
                    create es-saldo-estoq.
                    assign es-saldo-estoq.cod-estabel = es-etiqueta-pai.cod-estabel
                           es-saldo-estoq.it-codigo   = es-etiqueta-pai.it-codigo  
                           es-saldo-estoq.cod-depos   = 'EXL'                
                           es-saldo-estoq.lote        = es-etiqueta-pai.lote       
                           es-saldo-estoq.cod-localiz = c-localiz 
                           es-saldo-estoq.qt-total    = es-etiqueta.qt-orig
                           es-saldo-estoq.qt-saldo    = es-etiqueta.qt-orig.
                end.
            end.

            for each es-ped-etiqueta exclusive-lock
               where es-ped-etiqueta.cod-etiqueta = es-etiqueta.cod-etiqueta:
                delete es-ped-etiqueta.
            end.
        end.
        else do:
            run piMessage ("Etiqueta n∆o cadastrada.").
        
            next.
        end.
    end.

    run piMessage ("Etiqueta reativada com sucesso!").

    assign i-etq     = 0
           c-localiz = ''.
    
    pause 0 before-hide.

end.




