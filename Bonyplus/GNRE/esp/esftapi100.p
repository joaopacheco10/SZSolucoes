def input param p-row-lote-gnre as rowid no-undo.

def var v_hdl_aux  as handle no-undo.
def var i-parc     as int    no-undo.
def var i-seq      as int    no-undo.
def var i-seq-erro as int    no-undo.

{utp/ut-glob.i}
{esp/esftapi100.i}

find es-lote-gnre no-lock where
     rowid(es-lote-gnre) = p-row-lote-gnre no-error.
if avail es-lote-gnre then do:

/*    message 'achou lote'
        view-as alert-box.*/
        
    for each es-nota-fiscal no-lock where
             es-nota-fiscal.cod-lote = es-lote-gnre.cod-lote:
        
        find nota-fiscal no-lock where
             nota-fiscal.cod-estabel = es-nota-fiscal-gnre.cod-estabel and
             nota-fiscal.serie       = es-nota-fiscal-gnre.serie       and
             nota-fiscal.nr-nota-fis = es-nota-fiscal-gnre.nr-nota-fis no-error.             
             
/*        message 'NOTA' skip
                'Estab - ' es-nota-fiscal-gnre.cod-estabel skip
                'Serie - ' es-nota-fiscal-gnre.serie skip
                'Nota  - ' es-nota-fiscal-gnre.nr-nota-fis skip
                'Rec   - ' es-nota-fiscal-gnre.cod-receita
            view-as alert-box.    */
    
        find es-conf-uf-gnre no-lock where
             es-conf-uf-gnre.cod-uf      = nota-fiscal.estado              and
             es-conf-uf-gnre.cod-receita = es-nota-fiscal-gnre.cod-receita no-error.
        if avail es-conf-uf-gnre then do:
    
            find first emscad.fornecedor no-lock where 
                       fornecedor.cdn_fornecedor = es-conf-uf-gnre.cod-fornec no-error.
            if avail fornecedor then
                find first fornec_financ no-lock where 
                           fornec_financ.cdn_fornecedor = fornecedor.cdn_fornecedor no-error.
        
            find first estabelecimento no-lock where 
                       estabelecimento.cod_estab = es-nota-fiscal-gnre.cod-estabel no-error.
        
            find first emsuni.empresa no-lock where 
                       empresa.cod_empresa  = estabelecimento.cod_empresa no-error.
                            
            find first param_empres_apb no-lock where 
                       param_empres_apb.cod_empresa  = empresa.cod_empresa no-error.
            
            find param_geral_apb of param_empres_apb no-lock no-error.
            
            find es-param-gnre no-lock where
                 es-param-gnre.cod-estabel = es-nota-fiscal-gnre.cod-estabel no-error.
        
            find first tt_integr_apb_lote_impl where
                       tt_integr_apb_lote_impl.tta_cod_estab = estabelecimento.cod_estab no-error.
            if not avail tt_integr_apb_lote_impl then do:
            
/*                message 'Cria Capa - ' estabelecimento.cod_estab
                    view-as alert-box.*/
                
                create tt_integr_apb_lote_impl.
                assign tt_integr_apb_lote_impl.tta_cod_empresa                 = empresa.cod_empresa
                       tt_integr_apb_lote_impl.tta_cod_estab                   = estabelecimento.cod_estab
                       tt_integr_apb_lote_impl.tta_cod_refer                   = string(es-lote-gnre.num-recibo)
                       tt_integr_apb_lote_impl.tta_cod_indic_econ              = "REAL"
                       tt_integr_apb_lote_impl.tta_dat_transacao               = today
                       tt_integr_apb_lote_impl.tta_val_tot_lote_impl_tit_ap    = es-nota-fiscal-gnre.vl-receita
                       tt_integr_apb_lote_impl.tta_ind_origin_tit_ap           = "APB".
                
                validate tt_integr_apb_lote_impl no-error.
                
                assign i-seq = 1.
            end.
            else
                assign tt_integr_apb_lote_impl.tta_val_tot_lote_impl_tit_ap = tt_integr_apb_lote_impl.tta_val_tot_lote_impl_tit_ap + es-nota-fiscal-gnre.vl-receita
                       i-seq = i-seq + 1.

            find last tit_ap no-lock where
                      tit_ap.cod_estab       = tt_integr_apb_lote_impl.tta_cod_estab and
                      tit_ap.cdn_fornecedor  = fornecedor.cdn_fornecedor             and
                      tit_ap.cod_espec_docto = es-param-gnre.cod-espec-docto         and
                      tit_ap.cod_ser_docto   = es-param-gnre.cod-ser-docto           and
                      tit_ap.cod_tit_ap      = nota-fiscal.nr-nota-fis               no-error.
            if avail tit_ap then
                assign i-parc = int(tit_ap.cod_parcela) + 1.
            else
                assign i-parc = 1.
                
            find last tt_integr_apb_item_lote_impl_3 where
                      tt_integr_apb_item_lote_impl_3.tta_cod_tit_ap = nota-fiscal.nr-nota-fis no-error.
            if avail tt_integr_apb_item_lote_impl_3 then
                assign i-parc = int(tt_integr_apb_item_lote_impl_3.tta_cod_parcela) + 1.                                             
                
/*            message 'Cria Item Lote - ' es-nota-fiscal-gnre.vl-receita skip
                    'Titulo         - ' nota-fiscal.nr-nota-fis skip
                    'Parcela        - ' i-parc skip
                    'Sequencia      - ' i-seq
                view-as alert-box.*/
            
            create tt_integr_apb_item_lote_impl_3.
            assign tt_integr_apb_item_lote_impl_3.ttv_rec_integr_apb_lote_impl  = recid(tt_integr_apb_lote_impl)
                   tt_integr_apb_item_lote_impl_3.ttv_rec_integr_apb_item_lote  = recid(tt_integr_apb_item_lote_impl_3)
                   tt_integr_apb_item_lote_impl_3.tta_num_seq_refer             = i-seq
                   tt_integr_apb_item_lote_impl_3.tta_cdn_fornec                = fornecedor.cdn_fornecedor
                   tt_integr_apb_item_lote_impl_3.tta_cod_espec_docto           = es-param-gnre.cod-espec-docto
                   tt_integr_apb_item_lote_impl_3.tta_cod_ser_docto             = es-param-gnre.cod-ser-docto
                   tt_integr_apb_item_lote_impl_3.tta_cod_tit_ap                = nota-fiscal.nr-nota-fis
                   tt_integr_apb_item_lote_impl_3.tta_cod_parcela               = string(i-parc)
                   tt_integr_apb_item_lote_impl_3.tta_cod_indic_econ            = "REAL"
                   tt_integr_apb_item_lote_impl_3.tta_cod_portador              = fornec_financ.cod_portador
                   tt_integr_apb_item_lote_impl_3.tta_cod_cart_bcia             = ""
                   tt_integr_apb_item_lote_impl_3.tta_dat_vencto_tit_ap         = es-lote-gnre.dt-vencto
                   tt_integr_apb_item_lote_impl_3.tta_dat_prev_pagto            = es-lote-gnre.dt-pagto
                   tt_integr_apb_item_lote_impl_3.tta_dat_desconto              = ?
                   tt_integr_apb_item_lote_impl_3.tta_dat_emis_docto            = today
                   tt_integr_apb_item_lote_impl_3.tta_val_tit_ap                = es-nota-fiscal-gnre.vl-receita
                   tt_integr_apb_item_lote_impl_3.tta_val_desconto              = 0
                   tt_integr_apb_item_lote_impl_3.tta_val_perc_desc             = 0
                   tt_integr_apb_item_lote_impl_3.tta_val_perc_juros_dia_atraso = 0
                   tt_integr_apb_item_lote_impl_3.tta_val_perc_multa_atraso     = 0.

            validate tt_integr_apb_item_lote_impl_3 no-error.
        
            create tt_integr_apb_aprop_ctbl_pend.
            assign tt_integr_apb_aprop_ctbl_pend.ttv_rec_integr_apb_item_lote   = recid(tt_integr_apb_item_lote_impl_3)
                   tt_integr_apb_aprop_ctbl_pend.ttv_rec_antecip_pef_pend       = ?
                   tt_integr_apb_aprop_ctbl_pend.ttv_rec_integr_apb_impto_pend  = ?
                   tt_integr_apb_aprop_ctbl_pend.tta_cod_plano_cta_ctbl         = es-param-gnre.cod-plano-cta-ctbl
                   tt_integr_apb_aprop_ctbl_pend.tta_cod_cta_ctbl               = es-param-gnre.cod-cta-ctbl
                   tt_integr_apb_aprop_ctbl_pend.tta_cod_unid_negoc             = es-param-gnre.cod-unid-negoc
                   tt_integr_apb_aprop_ctbl_pend.tta_cod_plano_ccusto           = '' /*es-param-gnre.cod-plano-ccusto*/
                   tt_integr_apb_aprop_ctbl_pend.tta_cod_ccusto                 = '' /*es-param-gnre.cod-ccusto*/
                   tt_integr_apb_aprop_ctbl_pend.tta_cod_tip_fluxo_financ       = es-param-gnre.cod-tip-fluxo-financ
                   tt_integr_apb_aprop_ctbl_pend.tta_val_aprop_ctbl             = es-nota-fiscal-gnre.vl-receita.
        
            validate tt_integr_apb_aprop_ctbl_pend no-error.
        end.
    end.
    
/*    message 'Antes Api'
    view-as alert-box.*/

    run prgfin/apb/apb900zg.py persistent set v_hdl_aux.
    
/*    message 'Depois Api'
        view-as alert-box.*/
    
    run pi_main_block_api_tit_ap_cria_4 in v_hdl_aux (input 5
                                                     ,input 'BP001' /*v_cod_matriz_trad_org_ext*/
                                                     ,input-output table tt_integr_apb_item_lote_impl_3).
    
    
    if not can-find(first tt_log_erros_atualiz) then do:
        
        find last es-lote-gnre-erro no-lock where
                  es-lote-gnre-erro.cod-lote = es-lote-gnre.cod-lote no-error.
        if avail es-lote-gnre-erro then
            assign i-seq-erro = es-lote-gnre-erro.seq-erro + 1.
        else
            assign i-seq-erro = 1.              
        
        create es-lote-gnre-erro.
        assign es-lote-gnre-erro.cod-lote    = es-lote-gnre.cod-lote
               es-lote-gnre-erro.seq-erro    = i-seq-erro
               es-lote-gnre-erro.cod-usuario = c-seg-usuario
               es-lote-gnre-erro.dt-proc     = today
               es-lote-gnre-erro.hr-proc     = string(time, "HH:MM:SS")
               es-lote-gnre-erro.cod-erro    = 100
               es-lote-gnre-erro.des-erro    = 'Titulos de Pagamento criados com sucesso!'.
        
        find current es-lote-gnre exclusive-lock no-error.
        assign es-lote-gnre.idi-situacao = 4.
        
    end.
    else do:
        find first tt_log_erros_atualiz no-error.
        
        find last es-lote-gnre-erro no-lock where
                  es-lote-gnre-erro.cod-lote = es-lote-gnre.cod-lote no-error.
        if avail es-lote-gnre-erro then
            assign i-seq-erro = es-lote-gnre-erro.seq-erro + 1.
        else
            assign i-seq-erro = 1.              
        
        create es-lote-gnre-erro.
        assign es-lote-gnre-erro.cod-lote    = es-lote-gnre.cod-lote
               es-lote-gnre-erro.seq-erro    = i-seq-erro
               es-lote-gnre-erro.cod-usuario = c-seg-usuario
               es-lote-gnre-erro.dt-proc     = today
               es-lote-gnre-erro.hr-proc     = string(time, "HH:MM:SS")
               es-lote-gnre-erro.cod-erro    = tt_log_erros_atualiz.ttv_num_mensagem
               es-lote-gnre-erro.des-erro    = tt_log_erros_atualiz.ttv_des_msg_erro.

        
    end.
    
    output to "c:\temp\log_integr_ems5.txt".
    
    for each tt_log_erros_atualiz:
    
        put unformatted
            tt_log_erros_atualiz.tta_cod_estab  ' | '
            tt_log_erros_atualiz.tta_cod_refer ' | '
            tt_log_erros_atualiz.tta_num_seq_refer ' | '
            tt_log_erros_atualiz.ttv_num_mensagem ' | '
            tt_log_erros_atualiz.ttv_des_msg_erro skip.        
            
    
    end.
       
    output close.
    
end.

delete procedure v_hdl_aux no-error.


