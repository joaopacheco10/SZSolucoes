def temp-table tt-tit-venc no-undo
    field cod_estab          like tit_acr.cod_estab
    field cod_espec_docto    like tit_acr.cod_espec_docto
    field cod_ser_docto      like tit_acr.cod_ser_docto
    field cod_tit_acr        like tit_acr.cod_tit_acr
    field cod_parcela        like tit_acr.cod_parcela
    field cdn_cliente        like tit_acr.cdn_cliente
    field nom_abrev          like tit_acr.nom_abrev
    field dat_emis_docto     like tit_acr.dat_emis_docto
    field dat_vencto_tit_acr like tit_acr.dat_vencto_tit_acr
    field val_sdo_tit_acr    like tit_acr.val_sdo_tit_acr.

def input param p-cdn-cliente like tit_acr.cdn_cliente no-undo.
def output param table for tt-tit-venc.

for each tit_acr no-lock where
         tit_acr.cdn_cliente         = p-cdn-cliente and
         /*tit_acr.cod_espec_docto     = "DP"          and*/
         tit_acr.ind_sit_tit_acr     = "Normal"      and
         tit_acr.log_sdo_tit_acr     = yes           and
         tit_acr.log_tit_acr_estordo = no            and
         tit_acr.dat_vencto_tit_acr  < today:

    create tt-tit-venc.
    assign tt-tit-venc.cod_estab          = tit_acr.cod_estab         
           tt-tit-venc.cod_espec_docto    = tit_acr.cod_espec_docto   
           tt-tit-venc.cod_ser_docto      = tit_acr.cod_ser_docto     
           tt-tit-venc.cod_tit_acr        = tit_acr.cod_tit_acr       
           tt-tit-venc.cod_parcela        = tit_acr.cod_parcela       
           tt-tit-venc.cdn_cliente        = tit_acr.cdn_cliente       
           tt-tit-venc.nom_abrev          = tit_acr.nom_abrev         
           tt-tit-venc.dat_emis_docto     = tit_acr.dat_emis_docto    
           tt-tit-venc.dat_vencto_tit_acr = tit_acr.dat_vencto_tit_acr
           tt-tit-venc.val_sdo_tit_acr    = tit_acr.val_sdo_tit_acr.         
end.

/*
if not can-find(tt-tit-venc) then do:
    create tt-tit-venc.
    assign tt-tit-venc.cod_estab          = "999"
           tt-tit-venc.cod_espec_docto    = "DP"
           tt-tit-venc.cod_ser_docto      = ""
           tt-tit-venc.cod_tit_acr        = "0123456"
           tt-tit-venc.cod_parcela        = "01"
           tt-tit-venc.cdn_cliente        = 99999
           tt-tit-venc.nom_abrev          = "teste"
           tt-tit-venc.dat_emis_docto     = 01/01/2015
           tt-tit-venc.dat_vencto_tit_acr = 02/01/2015
           tt-tit-venc.val_sdo_tit_acr    = 999.99.


    create tt-tit-venc.
    assign tt-tit-venc.cod_estab          = "999"
           tt-tit-venc.cod_espec_docto    = "DP"
           tt-tit-venc.cod_ser_docto      = ""
           tt-tit-venc.cod_tit_acr        = "0123456"
           tt-tit-venc.cod_parcela        = "02"
           tt-tit-venc.cdn_cliente        = 99999
           tt-tit-venc.nom_abrev          = "teste"
           tt-tit-venc.dat_emis_docto     = 01/01/2015
           tt-tit-venc.dat_vencto_tit_acr = 02/01/2015
           tt-tit-venc.val_sdo_tit_acr    = 999.99. 
end.
*/
