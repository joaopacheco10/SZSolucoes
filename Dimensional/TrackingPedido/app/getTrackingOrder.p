/*************************************************
** Programa: app/getOrderTracking.p             **
** Data       : Fevereiro/2016                  **
** Autor      : SZ - Eliziani                   **
** Descri‡Æo: Rastreio de pedido                **
**                                              **
** Altera‡Æo: Joao Pacheco - 21/11/16           **
**            - Enviar Dt/Hora Aprov            **
**            - Enviar Usr Aprov                **
**************************************************/

def temp-table tt-order-tracking no-undo
    field nr-pedcli          like ped-venda.nr-pedcli
    field nome-abrev         like ped-venda.nome-abrev
    field cod-emitente       like emitente.cod-emitente
    field nome-emit          like emitente.nome-emit
    field dt-implant         like ped-venda.dt-implant
    field vl-tot-ped         like ped-venda.vl-tot-ped
    field cdd-embarq         as char
    field dt-embarque        as char
    field des-sit-aval       as char
    field situacao-wms       as char
    field qtd-itens-tot      as char
    field qtd-itens-sep-conf as char
    field user-sep-conf      as char
    field nr-nota-fis        as char
    field nfs-canceladas     as char
    field dt-aprov           like ped-venda.dt-apr-cred
    field hr-aprov           as char
    field user-aprov         like ped-venda.quem-aprovou.

def input param pnome-abrev like ped-venda.nome-abrev no-undo.
def input param pnr-pedcli  like ped-venda.nr-pedcli  no-undo.
def output param table for tt-order-tracking.      

def temp-table tt-embarques no-undo
    field cdd-embarq  like embarque.cdd-embarq
    field nr-nota-fis like nota-fiscal.nr-nota-fis
    field nf-cancel   like nota-fiscal.nr-nota-fis.

def var c-cdd-embarq      as char    no-undo.
def var d-cdd-embarq      like embarque.cdd-embarq no-undo.
def var i-cont            as integer no-undo.
def var i-situacao        as integer no-undo.
def var c-users-separa    as char    no-undo.
def var c-situacao-wms    as char    no-undo.
def var c-dt-embarque     as char    no-undo.
def var d-qtd-tot-itens   as integer no-undo.
def var d-qtd-itens-proc  as integer no-undo.
def var c-lst-itens-tot   as char    no-undo.
def var c-lst-itens-proc  as char    no-undo.
def var c-lst-nr-nota-fis as char    no-undo.
def var c-nfs-canceladas  as char    no-undo.
def var c-dt-aprov        as date    no-undo.
def var c-hr-aprov        as char    no-undo.
def var c-quem-aprov      as char    no-undo.

def buffer bped-venda   for ped-venda.
def buffer bnota-fiscal for nota-fiscal.

for each ped-venda no-lock
   where ped-venda.nome-abrev = pnome-abrev
     and ped-venda.nr-pedcli  = pnr-pedcli,
   first emitente no-lock
   where emitente.nome-abrev = ped-venda.nome-abrev:

    assign c-dt-aprov   = ped-venda.dt-apr-cred
           c-hr-aprov   = ''
           c-quem-aprov = ped-venda.quem-aprovou.

    for each pre-fatur no-lock
       where pre-fatur.nome-abrev  = ped-venda.nome-abrev
         and pre-fatur.nr-pedcli   = ped-venda.nr-pedcli:
        if not can-find(first tt-embarques
                        where tt-embarques.cdd-embarq = pre-fatur.cdd-embarq) then do:
            create tt-embarques.
            assign tt-embarques.cdd-embarq = pre-fatur.cdd-embarq.
        end.
    end.

    find first dw-ped-venda 
         where dw-ped-venda.nr-pedido = ped-venda.nr-pedido no-lock no-error.
    if avail dw-ped-venda then do:
        find first bped-venda
             where bped-venda.nr-pedido = dw-ped-venda.nr-pedido-transf no-lock no-error.
        if avail bped-venda then do:
            for each pre-fatur no-lock
               where pre-fatur.nome-abrev = bped-venda.nome-abrev
                 and pre-fatur.nr-pedcli  = bped-venda.nr-pedcli:
                if not can-find(first tt-embarques
                                where tt-embarques.cdd-embarq = pre-fatur.cdd-embarq) then do:
                    create tt-embarques.
                    assign tt-embarques.cdd-embarq = pre-fatur.cdd-embarq.
                end.
            end.
        end.
    end.
    
    if can-find(first nota-fiscal no-lock
                where nota-fiscal.nr-pedcli   = ped-venda.nr-pedcli
                  and nota-fiscal.nome-ab-cli = ped-venda.nome-abrev) then do:
        for each nota-fiscal no-lock
           where nota-fiscal.nr-pedcli   = ped-venda.nr-pedcli
             and nota-fiscal.nome-ab-cli = ped-venda.nome-abrev:
            if nota-fiscal.cdd-embarq <> 0  then do:
                find first tt-embarques
                     where tt-embarques.cdd-embarq = nota-fiscal.cdd-embarq no-error.
                if not avail tt-embarques then do:
                    create tt-embarques.
                    assign tt-embarques.cdd-embarq = nota-fiscal.cdd-embarq.
                end.

                if nota-fiscal.dt-cancel <> ? then do:
                    assign tt-embarques.nf-cancel = nota-fiscal.nr-nota-fis.
                end.
                else do:
                    assign tt-embarques.nr-nota-fis = nota-fiscal.nr-nota-fis.
                end.
            end.

            find first dw-rastrea-transf no-lock
                 where dw-rastrea-transf.cod-estabel-fat = nota-fiscal.cod-estabel
                   and dw-rastrea-transf.serie-fat       = nota-fiscal.serie
                   and dw-rastrea-transf.nr-nota-fis-fat = nota-fiscal.nr-nota-fis no-error.
            if avail dw-rastrea-transf then do:
                for first bnota-fiscal no-lock
                    where bnota-fiscal.cod-estabel = dw-rastrea-transf.cod-estabel-ori
                      and bnota-fiscal.serie       = dw-rastrea-transf.serie-ori
                      and bnota-fiscal.nr-nota-fis = dw-rastrea-transf.nr-nota-fis-ori:
                    if bnota-fiscal.cdd-embarq <> 0  then do:
                        if not can-find(first tt-embarques
                                        where tt-embarques.cdd-embarq = bnota-fiscal.cdd-embarq) then do:
                            create tt-embarques.
                            assign tt-embarques.cdd-embarq = bnota-fiscal.cdd-embarq.
                        end.
                    end.
                end.

                find first tt-embarques
                     where tt-embarques.cdd-embarq = dw-rastrea-transf.cdd-embarq no-error.
                if not avail tt-embarques then do:
                    create tt-embarques.
                    assign tt-embarques.cdd-embarq = dw-rastrea-transf.cdd-embarq.
                end.

                if nota-fiscal.dt-cancel <> ? then do:
                    assign tt-embarques.nf-cancel = nota-fiscal.nr-nota-fis.
                end.
                else do:
                    assign tt-embarques.nr-nota-fis = nota-fiscal.nr-nota-fis.
                end.
            end.
        end.
    end.

    for each tt-embarques:
        assign i-cont = i-cont + 1.

        assign i-situacao   = 0
               d-cdd-embarq = tt-embarques.cdd-embarq.

        if i-cont > 1 then
            assign c-cdd-embarq      = c-cdd-embarq   + ";"
                   c-lst-nr-nota-fis = c-lst-nr-nota-fis + ";"
                   c-nfs-canceladas  = c-nfs-canceladas + ";"
                   c-users-separa    = c-users-separa + ";"
                   c-situacao-wms    = c-situacao-wms + ";"
                   c-lst-itens-tot   = c-lst-itens-tot + ";"
                   c-lst-itens-proc  = c-lst-itens-proc + ";"
                   c-dt-embarque     = c-dt-embarque + ";".

        assign c-cdd-embarq      = c-cdd-embarq + string(tt-embarques.cdd-embarq)
               c-lst-nr-nota-fis = c-lst-nr-nota-fis + tt-embarques.nr-nota-fis.
               c-nfs-canceladas  = c-nfs-canceladas + tt-embarques.nf-cancel.
    
        for each dw-wm-docto no-lock
           where dw-wm-docto.cdd-embarq = d-cdd-embarq:
    
            assign i-situacao = 1.
            find first usuar_mestre no-lock
                 where usuar_mestre.cod_usuario = dw-wm-docto.user-separacao no-error.
    
            if avail usuar_mestre then do:
                if index(c-users-separa, usuar_mestre.nom_usuario) = 0 then do:
                    if c-users-separa = "" then
                        assign c-users-separa = usuar_mestre.nom_usuario.
                    else
                        assign c-users-separa = c-users-separa + "/" + usuar_mestre.nom_usuario.
                end.
            end.
        end.
    
        if can-find(first dw-wm-docto
                    where dw-wm-docto.cdd-embarq = d-cdd-embarq
                      and dw-wm-docto.tipo       = 1) then do:
            if not can-find(first dw-wm-docto
                            where dw-wm-docto.cdd-embarq = d-cdd-embarq
                              and dw-wm-docto.tipo       = 1
                              and dw-wm-docto.situacao  <> 4) then do:
                assign i-situacao = 2.
            end.
        end.
        if can-find(first dw-wm-docto
                    where dw-wm-docto.cdd-embarq = d-cdd-embarq
                      and dw-wm-docto.situacao   = 3) then do:
            assign i-situacao = 3.
        end.
        if can-find(first dw-wm-docto
                    where dw-wm-docto.cdd-embarq = d-cdd-embarq
                      and dw-wm-docto.tipo       = 2) then do:
            if not can-find(first dw-wm-docto
                            where dw-wm-docto.cdd-embarq = d-cdd-embarq
                              and dw-wm-docto.tipo       = 2
                              and dw-wm-docto.situacao  <> 4) then do:
                assign i-situacao = 4.
            end.
        end.
        if can-find(first dw-wm-docto
                    where dw-wm-docto.cdd-embarq = d-cdd-embarq
                      and dw-wm-docto.tipo       = 3) then do:
            if not can-find(first dw-wm-docto
                            where dw-wm-docto.cdd-embarq = d-cdd-embarq
                              and dw-wm-docto.tipo       = 3
                              and dw-wm-docto.situacao  <> 4) then do:
                assign i-situacao = 5.
            end.
        end.
        if can-find(first dw-wm-docto
                    where dw-wm-docto.cdd-embarq = d-cdd-embarq
                      and dw-wm-docto.tipo       = 4) then do:
            assign i-situacao = 6.
            if not can-find(first dw-wm-docto
                            where dw-wm-docto.cdd-embarq = d-cdd-embarq
                              and dw-wm-docto.tipo       = 4
                              and dw-wm-docto.situacao   < 4) then do:
                assign i-situacao = 7.
            end.
        end.
    
        case i-situacao:
            when 0 then
                assign c-situacao-wms = c-situacao-wms + "Sem WMS".
            when 1 then
                assign c-situacao-wms = c-situacao-wms + "Em separa‡Æo".
            when 2 then
                assign c-situacao-wms = c-situacao-wms + "Separado".
            when 3 then
                assign c-situacao-wms = c-situacao-wms + "Em Divergˆncia".
            when 4 then
                assign c-situacao-wms = c-situacao-wms + "Conferido".
            when 5 then
                assign c-situacao-wms = c-situacao-wms + "Consolidado".
            when 6 then
                assign c-situacao-wms = c-situacao-wms + "Pendente Conf. Volumes".
            when 7 then
                assign c-situacao-wms = c-situacao-wms + "Conferido Volumes".
        end case.
    
        for each it-pre-fat no-lock
           where it-pre-fat.cdd-embarq = d-cdd-embarq:
            assign d-qtd-tot-itens = d-qtd-tot-itens + 1.
            for first wm-docto-itens no-lock
                where wm-docto-itens.cdd-embarq   = it-pre-fat.cdd-embarq
                  and wm-docto-itens.cod-item     = it-pre-fat.it-codigo:
                if i-situacao >= 3 then do:
                    if not can-find(first dw-etiq-separa no-lock
                                    where dw-etiq-separa.cdd-embarq   = wm-docto-itens.cdd-embarq
                                      and dw-etiq-separa.num-seq-item = wm-docto-itens.num-seq-item
                                      and dw-etiq-separa.situacao     < 4) then do :
                        assign d-qtd-itens-proc = d-qtd-itens-proc + 1.
                    end.
                end.
                else do:
                    if not can-find(first dw-etiq-separa no-lock
                                    where dw-etiq-separa.cdd-embarq   = wm-docto-itens.cdd-embarq
                                      and dw-etiq-separa.num-seq-item = wm-docto-itens.num-seq-item
                                      and dw-etiq-separa.situacao    <> 2) then do :
                        assign d-qtd-itens-proc = d-qtd-itens-proc + 1.
                    end.
                end.
            end.
        end.
        assign c-lst-itens-tot  = c-lst-itens-tot + string(d-qtd-tot-itens)
               c-lst-itens-proc = c-lst-itens-proc + string(d-qtd-itens-proc).
    
        find first embarque no-lock
             where embarque.cdd-embarq = d-cdd-embarq no-error.

        if avail embarque then
            assign c-dt-embarque = c-dt-embarque + string(embarque.dt-embarque).
    end.

    if can-find(dw-follow-up-ped where
                dw-follow-up-ped.nome-abrev = ped-venda.nome-abrev and
                dw-follow-up-ped.nr-pedcli  = ped-venda.nr-pedcli) then do:

        find dw-follow-up-ped no-lock where
             dw-follow-up-ped.nome-abrev = ped-venda.nome-abrev and
             dw-follow-up-ped.nr-pedcli  = ped-venda.nr-pedcli  no-error.

        if dw-follow-up-ped.user-aprov-auto <> '' then
            assign c-dt-aprov   = dw-follow-up-ped.dt-aprov-auto
                   c-hr-aprov   = dw-follow-up-ped.hr-aprov-auto
                   c-quem-aprov = dw-follow-up-ped.user-aprov-auto.

        if dw-follow-up-ped.user-aprov-cred <> '' then
            assign c-dt-aprov   = dw-follow-up-ped.dt-aprov-cred
                   c-hr-aprov   = dw-follow-up-ped.hr-aprov-cred
                   c-quem-aprov = dw-follow-up-ped.user-aprov-cred. 

    end.

    create tt-order-tracking.
    assign tt-order-tracking.nr-pedcli          = ped-venda.nr-pedcli 
           tt-order-tracking.nome-abrev         = emitente.nome-abrev
           tt-order-tracking.cod-emitente       = emitente.cod-emitente
           tt-order-tracking.nome-emit          = emitente.nome-emit
           tt-order-tracking.dt-implant         = ped-venda.dt-implant
           tt-order-tracking.vl-tot-ped         = ped-venda.vl-tot-ped
           tt-order-tracking.cdd-embarq         = c-cdd-embarq
           tt-order-tracking.dt-embarque        = c-dt-embarque 
           tt-order-tracking.des-sit-aval       = {diinc/i03di159.i 04 ped-venda.cod-sit-aval} 
           tt-order-tracking.situacao-wms       = c-situacao-wms
           tt-order-tracking.qtd-itens-tot      = c-lst-itens-tot
           tt-order-tracking.qtd-itens-sep-conf = c-lst-itens-proc
           tt-order-tracking.user-sep-conf      = c-users-separa
           tt-order-tracking.nr-nota-fis        = c-lst-nr-nota-fis
           tt-order-tracking.nfs-canceladas     = c-nfs-canceladas
           tt-order-tracking.dt-aprov           = c-dt-aprov
           tt-order-tracking.hr-aprov           = c-hr-aprov
           tt-order-tracking.user-aprov         = c-quem-aprov.
end.    

return "OK".

