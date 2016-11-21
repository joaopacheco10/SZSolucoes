/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*********************************************************************************/
{include/i-prgvrs.i FT0518F1 2.00.00.001 } /*** 010001 ***/


&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i ft0518f1 MFT}
&ENDIF

{ftp/ft0518f.i5}
{include/tt-edit.i}
{include/pi-edit.i}
{utp/utapi019.i}

{include/i-epc200.i "ft0518f1":U}

{ftp/ft0518rp.i1} /* Definiá∆o temp-table ttCaracteres como SHARED */

{adapters/xml/ep2/axsep017extradeclarations.i} /* definiá∆o FUNCTION fn-tira-acento */

DEF TEMP-TABLE ttLinha NO-UNDO
    FIELD sequenciaLinha  AS INT 
    FIELD codigoColuna    AS CHAR
    FIELD conteudoCampo   AS CHAR
    INDEX idx1 IS PRIMARY UNIQUE sequenciaLinha codigoColuna
    INDEX idx2 codigoColuna
    INDEX idx3 sequenciaLinha DESC.

FORM
    "Erro:" VIEW-AS TEXT 
    tt-erros.cod-erro NO-LABEL SKIP
    tt-erros.desc-erro NO-LABEL SKIP(2)
    with frame f-print down stream-io width 300.
DEF INPUT  PARAM TABLE FOR ttDanfe.
DEF INPUT  PARAM TABLE FOR ttDanfeItem.
DEF INPUT  PARAM pcArq         AS CHAR NO-UNDO.
DEF INPUT  PARAM cModeloDanfe  AS CHAR NO-UNDO.
DEF INPUT  PARAM pSemWord      AS LOG  NO-UNDO.
DEF INPUT  PARAM pCodEstabel   AS CHAR NO-UNDO.

DEF VAR cLinha      AS CHAR       NO-UNDO.
DEF VAR cSeqAux     AS CHAR       NO-UNDO.
DEF VAR iSeqTotal   AS INT        NO-UNDO.
DEF VAR iPagTotal   AS INT        NO-UNDO.
DEF VAR iPagAtual   AS INT        NO-UNDO.
DEF VAR iSeqTtLinha AS INT        NO-UNDO.
DEF VAR cArqAux     AS CHAR       NO-UNDO.
DEF VAR ch-app-word AS COM-HANDLE NO-UNDO.
DEF VAR lFirst      AS LOG        NO-UNDO.
def var c-tracejado1 as character format 'x(180)' no-undo.
def var c-tracejado2 as character format 'x(249)' no-undo.
def var lEmiteTracejado as logical no-undo.

DEF VAR c-danfeaux AS CHAR NO-UNDO.
DEF VAR l-danfe    AS LOG  NO-UNDO.

DEFINE VARIABLE cCodProduto     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDescProduto    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDescProdutoComST  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDescProdutoComST2  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNCM            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCST            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCFOP           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cUn             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cQuantidade     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cValorUni       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cValorTotal     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cBaseCalcIcms   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cValorIcms      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cValorIpi       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAliquotaIcms   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAliquotaIpi    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cBaseCalcIcmsST AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cValorIcmsST    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSeparador      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cInfComplPedCli AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cUnTrib         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cQuantidadeTrib AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cValorUniTrib   AS CHARACTER   NO-UNDO.

DEFINE VARIABLE iNumLinhasProdPrimeiraPagina AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNumLinhasProdOutrasPaginas  AS INTEGER     NO-UNDO.

DEFINE VARIABLE lPrimeiraPagina             AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iLinhaPaginaAtual           AS INTEGER     NO-UNDO.
DEFINE VARIABLE lUltimaLinhaPagina          AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iNumLinhasOcupadasProduto   AS INTEGER     NO-UNDO.

DEFINE VARIABLE c-arquivo-danfe   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-arquivo-pdf     AS CHARACTER   NO-UNDO.

DEFINE VARIABLE c-cod-caminho-xml AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cChaveAcesso      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cArquivoXML       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE h-bodi135na       AS HANDLE      NO-UNDO.
DEFINE VARIABLE h-bodi520         AS HANDLE      NO-UNDO.
DEFINE VARIABLE iMaxEntries AS INTEGER     NO-UNDO.

DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.

DEFINE BUFFER empresa      FOR mguni.empresa.
DEF NEW GLOBAL SHARED VAR i-ep-codigo-usuario   like mguni.empresa.ep-codigo no-undo.
DEFINE SHARED VAR r-nota       AS ROWID.

DEF NEW GLOBAL SHARED VAR l-EnviaDanfXml  AS LOGICAL             NO-UNDO. /* Envia e-mail com o DANFE gerado*/
DEF NEW GLOBAL SHARED VAR c-seg-usuario   AS CHAR FORMAT "x(12)" NO-UNDO.
/* ASSIGN l-EnviaDanfXml = YES. */

{adapters/xml/ep2/axsep017.i} /*Temp-Tables da NF-e, ttNFe, ttIde, ttDet, etc.*/
RUN adapters/xml/ep2/axsep017.p PERSISTENT SET h-axsep017.
RUN pi-seta-nota-fiscal    IN h-axsep017 (INPUT r-nota).
RUN pi-prepara-dados       IN h-axsep017.
RUN pi-devolve-temp-tables IN h-axsep017 (OUTPUT  TABLE ttAdi       ,
                                          OUTPUT  TABLE ttArma      ,
                                          OUTPUT  TABLE ttAvulsa    ,
                                          OUTPUT  TABLE ttCobr      ,
                                          OUTPUT  TABLE ttCOFINSAliq,
                                          OUTPUT  TABLE ttCOFINSNT  ,
                                          OUTPUT  TABLE ttCOFINSOutr,
                                          OUTPUT  TABLE ttCOFINSQtde,
                                          OUTPUT  TABLE ttCOFINSST  ,
                                          OUTPUT  TABLE ttComb      ,
                                          OUTPUT  TABLE ttCompra    ,
                                          OUTPUT  TABLE ttDest      ,
                                          OUTPUT  TABLE ttDet       ,
                                          OUTPUT  TABLE ttDI        ,
                                          OUTPUT  TABLE ttDup       ,
                                          OUTPUT  TABLE ttEmit      ,
                                          OUTPUT  TABLE ttEntrega   ,
                                          OUTPUT  TABLE ttExporta   ,
                                          OUTPUT  TABLE ttICMS00    ,
                                          OUTPUT  TABLE ttICMS10    ,
                                          OUTPUT  TABLE ttICMS20    ,
                                          OUTPUT  TABLE ttICMS30    ,
                                          OUTPUT  TABLE ttICMS40    ,
                                          OUTPUT  TABLE ttICMS51    ,
                                          OUTPUT  TABLE ttICMS60    ,
                                          OUTPUT  TABLE ttICMS70    ,
                                          OUTPUT  TABLE ttICMS90    ,
                                          OUTPUT  TABLE ttICMSTot   ,
                                          OUTPUT  TABLE ttIde       ,
                                          OUTPUT  TABLE ttII        ,
                                          OUTPUT  TABLE ttInfAdic   ,
                                          OUTPUT  TABLE ttIPI       ,
                                          OUTPUT  TABLE ttISSQN     ,
                                          OUTPUT  TABLE ttISSQNtot  ,
                                          OUTPUT  TABLE ttLacres    ,
                                          OUTPUT  TABLE ttMed       ,
                                          OUTPUT  TABLE ttNFe       ,
                                          OUTPUT  TABLE ttrefNF     ,
                                          OUTPUT  TABLE ttObsCont   ,
                                          OUTPUT  TABLE ttObsFisco  ,
                                          OUTPUT  TABLE ttPISAliq   ,
                                          OUTPUT  TABLE ttPISNT     ,
                                          OUTPUT  TABLE ttPISOutr   ,
                                          OUTPUT  TABLE ttPISQtde   ,
                                          OUTPUT  TABLE ttPISST     ,
                                          OUTPUT  TABLE ttProcRef   ,
                                          OUTPUT  TABLE ttReboque   ,
                                          OUTPUT  TABLE ttRetirada  ,
                                          OUTPUT  TABLE ttRetTrib   ,
                                          OUTPUT  TABLE ttTransp    ,
                                          OUTPUT  TABLE ttVeic      ,
                                          OUTPUT  TABLE ttVol       ,
                                          OUTPUT  TABLE ttrefNFP    ,
                                          OUTPUT  TABLE ttrefCTe    ,
                                          OUTPUT  TABLE ttrefECF    ,
                                          OUTPUT  TABLE ttICMSPart  ,
                                          OUTPUT  TABLE ttICMSST    ,
                                          OUTPUT  TABLE ttICMSSN101 ,
                                          OUTPUT  TABLE ttICMSSN102 ,
                                          OUTPUT  TABLE ttICMSSN201 ,
                                          OUTPUT  TABLE ttICMSSN202 ,
                                          OUTPUT  TABLE ttICMSSN500 ,
                                          OUTPUT  TABLE ttICMSSN900 ,
                                          OUTPUT  TABLE ttCana      ,
                                          OUTPUT  TABLE ttForDia    ,
                                          OUTPUT  TABLE ttDeduc     ).



def var vl-unit-trib as dec FORMAT ">>>>>>>>>>>>9.99" DECIMALS 2.

ASSIGN l-danfe = CAN-FIND(FIRST funcao NO-LOCK
                          WHERE funcao.cd-funcao = "spp-danfe":U
                          AND   funcao.ativo).

DEF STREAM sInput.
DEF STREAM sOutput.

DEF NEW GLOBAL SHARED VAR gcDiretorioDanfe   AS CHAR NO-UNDO.
DEF NEW GLOBAL SHARED VAR gcDiretorioDanfeIt AS CHAR NO-UNDO.

IF  cModeloDANFE = '3' THEN /* Retrato */
    ASSIGN iNumLinhasProdPrimeiraPagina = 23
           iNumLinhasProdOutrasPaginas  = IF pSemWord THEN 71  /* Impress∆o SEM word */
                                                      ELSE 70. /* Impress∆o COM word */
ELSE
    ASSIGN iNumLinhasProdPrimeiraPagina = 7
           iNumLinhasProdOutrasPaginas  = IF pSemWord THEN 48  /* Impress∆o SEM word */
                                                      ELSE 47. /* Impress∆o COM word */

ASSIGN lPrimeiraPagina             = YES
       iLinhaPaginaAtual           = 0
       lUltimaLinhaPagina          = NO.

ASSIGN iSeqTtLinha = 0.
FOR EACH ttDanfeItem:

    RUN piTrataQuebraCampo (INPUT "CodProduto":U, 
                            INPUT ttDanfeItem.cprod,
                            OUTPUT cCodProduto,
                            OUTPUT cSeparador).

    find first ttdanfe.

        
        
       /* Verificar CST10 ou CST60 ou CST70 */
    find first ttICMS10 where ttICMS10.CodEstabelNF   = pCodEstabel     and
                              ttICMS10.SerieNF        = ttDanfe.ser     and
                              ttICMS10.NrNotaFisNF    = ttDanfe.nrnota  and
                              ttICMS10.ItCodigoNF     = ttDanfeItem.cprod no-lock no-error.
    if not avail ttICMS10 then do:

         find first ttICMS60 where ttICMS60.CodEstabelNF   = pCodEstabel     and
                                   ttICMS60.SerieNF        = ttDanfe.ser     and
                                   ttICMS60.NrNotaFisNF    = ttDanfe.nrnota  and
                                   ttICMS60.ItCodigoNF     = ttDanfeItem.cprod no-lock no-error.                                            
         if not avail ttICMS60 then do:
            find first ttICMS70 where ttICMS70.CodEstabelNF   = pCodEstabel     and
                                      ttICMS70.SerieNF        = ttDanfe.ser     and
                                      ttICMS70.NrNotaFisNF    = ttDanfe.nrnota  and
                                      ttICMS70.ItCodigoNF     = ttDanfeItem.cprod no-lock no-error.       

            if avail ttICMS70 then do:

                /* Calcula Valor Unitario Tributavel */
                find first it-nota-fisc where it-nota-fisc.serie       = ttDanfe.ser    and
                                              it-nota-fisc.nr-nota-fis = ttDanfe.nrnota and
                                              it-nota-fisc.cod-estabel = pCodEstabel    and
                                              it-nota-fisc.it-codigo   = ttDanfeItem.cprod no-lock no-error.
                IF AVAIL it-nota-fisc THEN
                    assign vl-unit-trib = round( dec(ttDanfeItem.vlunit) - (it-nota-fisc.val-desconto-total / dec(ttDanfeItem.quantItem)), 2). 
                ELSE
                    assign vl-unit-trib = 0.
                
                if ttICMS70.vICMSST = 0 and ttICMS70.vBCST = 0 then do:
                   find first item-nfs-st WHERE item-nfs-st.cod-estab-nfs = pCodEstabel
                                            AND item-nfs-st.cod-ser-nfs   = ttDanfe.ser
                                            AND item-nfs-st.cod-docto-nfs = ttDanfe.nrnota
                                            AND item-nfs-st.cod-item      = ttDanfeItem.cprod no-LOCK no-error.
                   if avail item-nfs-st then do:

                       /* Concatena ST para Sair no Danfe */
                       assign cDescProdutoComST = ttDanfeItem.descitem + " BC.ICMS ST: " + trim(string(truncate(item-nfs-st.val-livre-2 * 100 / 100, 2))) + " Vl.ICMS ST: " + trim(string(truncate(item-nfs-st.val-livre-3 * 100 / 100, 2))). 

                       /*find docum-est no-lock where
                            docum-est.serie-docto   = item-nfs-st.cod-ser-entr  and
                            docum-est.nro-docto     = item-nfs-st.cod-nota-entr and
                            docum-est.cod-emitente  = int(item-nfs-st.cod-emitente-entr) and
                            docum-est.nat-operacao  = item-nfs-st.cod-natur-operac-entr  no-error.
                       if avail docum-est then do:
                   
                           find nota-fiscal no-lock where
                                nota-fiscal.cod-estabel = docum-est.estab-de-or and
                                nota-fiscal.serie       = docum-est.serie-docto and
                                nota-fiscal.nr-nota-fis = docum-est.nro-docto   no-error.
                           if avail nota-fiscal then do:
                               find first it-nota-fisc no-lock where
                                          it-nota-fisc.cod-estabel = nota-fiscal.cod-estabel and
                                          it-nota-fisc.serie       = nota-fiscal.serie       and
                                          it-nota-fisc.nr-nota-fis = nota-fiscal.nr-nota-fis and
                                          it-nota-fisc.it-codigo   = item-nfs-st.cod-item      no-error.
                               if avail it-nota-fisc then
                                   assign cDescProdutoComST = cDescProdutoComST + " BASE ICMS: " + trim(string(truncate(it-nota-fisc.vl-bicms-it * 100 / 100, 2))) + " ICMS: " + trim(string(truncate(it-nota-fisc.vl-icms-it * 100 / 100, 2))).
                           end.
                       end.*/

                       assign cDescProdutoComST = cDescProdutoComST + " Vl.Unit.Trib: " + trim(string(vl-unit-trib)).

                   end.
                   else 
                       assign cDescProdutoComST = ttDanfeItem.descitem + " Vl.Unit.Trib: " + trim(string(vl-unit-trib)).
                end.
                ELSE DO:
                    /* Concatena ST para Sair no Danfe */
                    assign cDescProdutoComST = ttDanfeItem.descitem + " BC.ICMS ST: " + trim(string(ttICMS70.vBCST)) + " Vl.ICMS ST: " + trim(string(ttICMS70.vICMSST)). 

                    /*find first item-nfs-st NO-LOCK WHERE 
                               item-nfs-st.cod-estab-nfs = pCodEstabel       AND 
                               item-nfs-st.cod-ser-nfs   = ttDanfe.ser       AND 
                               item-nfs-st.cod-docto-nfs = ttDanfe.nrnota    AND 
                               item-nfs-st.cod-item      = ttDanfeItem.cprod no-error.
                    if avail item-nfs-st THEN do:                       

                        find docum-est no-lock where
                             docum-est.serie-docto   = item-nfs-st.cod-ser-entr  and
                             docum-est.nro-docto     = item-nfs-st.cod-nota-entr and
                             docum-est.cod-emitente  = int(item-nfs-st.cod-emitente-entr) and
                             docum-est.nat-operacao  = item-nfs-st.cod-natur-operac-entr  no-error.
                        if avail docum-est then do:
                    
                            find nota-fiscal no-lock where
                                 nota-fiscal.cod-estabel = docum-est.estab-de-or and
                                 nota-fiscal.serie       = docum-est.serie-docto and
                                 nota-fiscal.nr-nota-fis = docum-est.nro-docto   no-error.
                            if avail nota-fiscal then do:
                                find first it-nota-fisc no-lock where
                                           it-nota-fisc.cod-estabel = nota-fiscal.cod-estabel and
                                           it-nota-fisc.serie       = nota-fiscal.serie       and
                                           it-nota-fisc.nr-nota-fis = nota-fiscal.nr-nota-fis and
                                           it-nota-fisc.it-codigo   = item-nfs-st.cod-item      no-error.
                                if avail it-nota-fisc then
                                    assign cDescProdutoComST = cDescProdutoComST + " BASE ICMS: " + trim(string(truncate(it-nota-fisc.vl-bicms-it * 100 / 100, 2))) + " ICMS: " + trim(string(truncate(it-nota-fisc.vl-icms-it * 100 / 100, 2))).
                            end.
                        END.
                    END.*/

                    assign cDescProdutoComST = cDescProdutoComST + " Vl.Unit.Trib: " + trim(string(vl-unit-trib)).
                END.
            end.                            
            else do:
                find first it-nota-fisc where it-nota-fisc.serie       = ttDanfe.ser    and
                                              it-nota-fisc.nr-nota-fis = ttDanfe.nrnota and
                                              it-nota-fisc.cod-estabel = pCodEstabel    and
                                              it-nota-fisc.it-codigo   = ttDanfeItem.cprod no-lock no-error.                                                
                IF AVAIL it-nota-fisc THEN
                    assign vl-unit-trib = round( dec(ttDanfeItem.vlunit) - (it-nota-fisc.val-desconto-total / dec(ttDanfeItem.quantItem)), 2).
                ELSE
                    assign vl-unit-trib = 0.

                assign cDescProdutoComST = ttDanfeItem.descitem + " Vl.Unit.Trib: " + trim(string(vl-unit-trib)).
            end.
        end. 
        else do:

           /* Calcula Valor Unitario Tributavel */
           find first it-nota-fisc where it-nota-fisc.serie       = ttDanfe.ser    and
                                         it-nota-fisc.nr-nota-fis = ttDanfe.nrnota and
                                         it-nota-fisc.cod-estabel = pCodEstabel    and
                                         it-nota-fisc.it-codigo   = ttDanfeItem.cprod no-lock no-error.                    
           IF AVAIL it-nota-fisc THEN
               assign vl-unit-trib = round( dec(ttDanfeItem.vlunit) - (it-nota-fisc.val-desconto-total / dec(ttDanfeItem.quantItem)), 2). 
           ELSE
               assign vl-unit-trib = 0.             

           if ttICMS60.vICMSST = 0 and ttICMS60.vBCST = 0 then do:
               find first item-nfs-st WHERE item-nfs-st.cod-estab-nfs = pCodEstabel
                                        AND item-nfs-st.cod-ser-nfs   = ttDanfe.ser
                                        AND item-nfs-st.cod-docto-nfs = ttDanfe.nrnota
                                        AND item-nfs-st.cod-item      = ttDanfeItem.cprod no-LOCK no-error.
               if avail item-nfs-st then do:
                   /* Concatena ST para Sair no Danfe */
                   assign cDescProdutoComST = ttDanfeItem.descitem + " BC.ICMS ST: " + trim(string(truncate(item-nfs-st.val-livre-2 * 100 / 100, 2))) + " Vl.ICMS ST: " + trim(string(truncate(item-nfs-st.val-livre-3 * 100 / 100, 2))).
                   
                   /*find docum-est no-lock where
                        docum-est.serie-docto   = item-nfs-st.cod-ser-entr           and
                        docum-est.nro-docto     = item-nfs-st.cod-nota-entr          and
                        docum-est.cod-emitente  = int(item-nfs-st.cod-emitente-entr) and
                        docum-est.nat-operacao  = item-nfs-st.cod-natur-operac-entr  no-error.
                   if avail docum-est then do:

                       find nota-fiscal no-lock where
                            nota-fiscal.cod-estabel = docum-est.estab-de-or and
                            nota-fiscal.serie       = docum-est.serie-docto and
                            nota-fiscal.nr-nota-fis = docum-est.nro-docto   no-error.
                       if avail nota-fiscal then do:

                           find first it-nota-fisc no-lock where
                                      it-nota-fisc.cod-estabel = nota-fiscal.cod-estabel and
                                      it-nota-fisc.serie       = nota-fiscal.serie       and
                                      it-nota-fisc.nr-nota-fis = nota-fiscal.nr-nota-fis and
                                      it-nota-fisc.it-codigo   = item-nfs-st.cod-item      no-error.
                           if avail it-nota-fisc then
                               assign cDescProdutoComST = cDescProdutoComST + " BASE ICMS: " + trim(string(truncate(it-nota-fisc.vl-bicms-it * 100 / 100, 2))) + " ICMS: " + trim(string(truncate(it-nota-fisc.vl-icms-it * 100 / 100, 2))).
                       end.
                   end.*/                   
                    
                   assign cDescProdutoComST = cDescProdutoComST + " Vl.Unit.Trib: " + trim(string(vl-unit-trib)).
               end.
               else 
                   assign cDescProdutoComST = ttDanfeItem.descitem + " Vl.Unit.Trib: " + trim(string(vl-unit-trib)).
           end.
           ELSE DO:
                /* Concatena ST para Sair no Danfe */
                assign cDescProdutoComST = ttDanfeItem.descitem + " BC.ICMS ST: " + trim(string(ttICMS60.vBCST)) + " Vl.ICMS ST: " + trim(string(ttICMS60.vICMSST)). 

                /*find first item-nfs-st NO-LOCK WHERE 
                           item-nfs-st.cod-estab-nfs = pCodEstabel       AND 
                           item-nfs-st.cod-ser-nfs   = ttDanfe.ser       AND 
                           item-nfs-st.cod-docto-nfs = ttDanfe.nrnota    AND 
                           item-nfs-st.cod-item      = ttDanfeItem.cprod no-error.
                if avail item-nfs-st THEN do:                       

                    find docum-est no-lock where
                         docum-est.serie-docto   = item-nfs-st.cod-ser-entr  and
                         docum-est.nro-docto     = item-nfs-st.cod-nota-entr and
                         docum-est.cod-emitente  = int(item-nfs-st.cod-emitente-entr) and
                         docum-est.nat-operacao  = item-nfs-st.cod-natur-operac-entr  no-error.
                    if avail docum-est then do:
                
                        find nota-fiscal no-lock where
                             nota-fiscal.cod-estabel = docum-est.estab-de-or and
                             nota-fiscal.serie       = docum-est.serie-docto and
                             nota-fiscal.nr-nota-fis = docum-est.nro-docto   no-error.
                        if avail nota-fiscal then do:
                            find first it-nota-fisc no-lock where
                                       it-nota-fisc.cod-estabel = nota-fiscal.cod-estabel and
                                       it-nota-fisc.serie       = nota-fiscal.serie       and
                                       it-nota-fisc.nr-nota-fis = nota-fiscal.nr-nota-fis and
                                       it-nota-fisc.it-codigo   = item-nfs-st.cod-item      no-error.
                            if avail it-nota-fisc then
                                assign cDescProdutoComST = cDescProdutoComST + " BASE ICMS: " + trim(string(truncate(it-nota-fisc.vl-bicms-it * 100 / 100, 2))) + " ICMS: " + trim(string(truncate(it-nota-fisc.vl-icms-it * 100 / 100, 2))).
                        end.
                    END.
                END.*/

                assign cDescProdutoComST = cDescProdutoComST + " Vl.Unit.Trib: " + trim(string(vl-unit-trib)).
            END.
        end.
    end.
    else do:
        /* Calcula Valor Unitario Tributavel */
        find first it-nota-fisc where it-nota-fisc.serie       = ttDanfe.ser    and
                                      it-nota-fisc.nr-nota-fis = ttDanfe.nrnota and
                                      it-nota-fisc.cod-estabel = pCodEstabel    and
                                      it-nota-fisc.it-codigo   = ttDanfeItem.cprod no-lock no-error.                    
        IF AVAIL it-nota-fisc THEN
            assign vl-unit-trib = round( dec(ttDanfeItem.vlunit) - (it-nota-fisc.val-desconto-total / dec(ttDanfeItem.quantItem)), 2). 
        ELSE
            assign vl-unit-trib = 0.
        
        if ttICMS10.vICMSST = 0 and ttICMS10.vBCST = 0 then do:
            find first item-nfs-st WHERE item-nfs-st.cod-estab-nfs = pCodEstabel
                                     AND item-nfs-st.cod-ser-nfs   = ttDanfe.ser
                                     AND item-nfs-st.cod-docto-nfs = ttDanfe.nrnota
                                     AND item-nfs-st.cod-item      = ttDanfeItem.cprod no-LOCK no-error.
            if avail item-nfs-st then do:

                /* Concatena ST para Sair no Danfe */
                assign cDescProdutoComST = ttDanfeItem.descitem + " BC.ICMS ST: " + trim(string(truncate(item-nfs-st.val-livre-2 * 100 / 100, 2))) + " Vl.ICMS ST: " + trim(string(truncate(item-nfs-st.val-livre-3 * 100 / 100, 2))).

                /*find docum-est no-lock where
                     docum-est.serie-docto   = item-nfs-st.cod-ser-entr  and
                     docum-est.nro-docto     = item-nfs-st.cod-nota-entr and
                     docum-est.cod-emitente  = int(item-nfs-st.cod-emitente-entr) and
                     docum-est.nat-operacao  = item-nfs-st.cod-natur-operac-entr  no-error.
                if avail docum-est then do:
            
                    find nota-fiscal no-lock where
                         nota-fiscal.cod-estabel = docum-est.estab-de-or and
                         nota-fiscal.serie       = docum-est.serie-docto and
                         nota-fiscal.nr-nota-fis = docum-est.nro-docto   no-error.
                    if avail nota-fiscal then do:
                        find first it-nota-fisc no-lock where
                                   it-nota-fisc.cod-estabel = nota-fiscal.cod-estabel and
                                   it-nota-fisc.serie       = nota-fiscal.serie       and
                                   it-nota-fisc.nr-nota-fis = nota-fiscal.nr-nota-fis and
                                   it-nota-fisc.it-codigo   = item-nfs-st.cod-item      no-error.
                        if avail it-nota-fisc then
                            assign cDescProdutoComST = cDescProdutoComST + " BASE ICMS: " + trim(string(truncate(it-nota-fisc.vl-bicms-it * 100 / 100, 2))) + " ICMS: " + trim(string(truncate(it-nota-fisc.vl-icms-it * 100 / 100, 2))).
                    end.
                end.*/

                assign cDescProdutoComST = cDescProdutoComST + " Vl.Unit.Trib: " + trim(string(vl-unit-trib)).
            end.
            else 
                assign cDescProdutoComST = ttDanfeItem.descitem + " Vl.Unit.Trib: " + trim(string(vl-unit-trib)).
        end.          
        ELSE DO:
            /* Concatena ST para Sair no Danfe */
            assign cDescProdutoComST = ttDanfeItem.descitem + " BC.ICMS ST: " + trim(string(ttICMS10.vBCST)) + " Vl.ICMS ST: " + trim(string(ttICMS10.vICMSST)). 

            /*find first item-nfs-st NO-LOCK WHERE 
                       item-nfs-st.cod-estab-nfs = pCodEstabel       AND 
                       item-nfs-st.cod-ser-nfs   = ttDanfe.ser       AND 
                       item-nfs-st.cod-docto-nfs = ttDanfe.nrnota    AND 
                       item-nfs-st.cod-item      = ttDanfeItem.cprod no-error.
            if avail item-nfs-st THEN do:                       

                find docum-est no-lock where
                     docum-est.serie-docto   = item-nfs-st.cod-ser-entr  and
                     docum-est.nro-docto     = item-nfs-st.cod-nota-entr and
                     docum-est.cod-emitente  = int(item-nfs-st.cod-emitente-entr) and
                     docum-est.nat-operacao  = item-nfs-st.cod-natur-operac-entr  no-error.
                if avail docum-est then do:
            
                    find nota-fiscal no-lock where
                         nota-fiscal.cod-estabel = docum-est.estab-de-or and
                         nota-fiscal.serie       = docum-est.serie-docto and
                         nota-fiscal.nr-nota-fis = docum-est.nro-docto   no-error.
                    if avail nota-fiscal then do:
                        find first it-nota-fisc no-lock where
                                   it-nota-fisc.cod-estabel = nota-fiscal.cod-estabel and
                                   it-nota-fisc.serie       = nota-fiscal.serie       and
                                   it-nota-fisc.nr-nota-fis = nota-fiscal.nr-nota-fis and
                                   it-nota-fisc.it-codigo   = item-nfs-st.cod-item      no-error.
                        if avail it-nota-fisc then
                            assign cDescProdutoComST = cDescProdutoComST + " BASE ICMS: " + trim(string(truncate(it-nota-fisc.vl-bicms-it * 100 / 100, 2))) + " ICMS: " + trim(string(truncate(it-nota-fisc.vl-icms-it * 100 / 100, 2))).
                    end.
                END.
            END.*/

            assign cDescProdutoComST = cDescProdutoComST + " Vl.Unit.Trib: " + trim(string(vl-unit-trib)).
        END.
    end.
    
    /*Calculo FCP - INICIO*/    
    find first it-nota-fisc no-lock where 
               it-nota-fisc.serie       = ttDanfe.ser       and
               it-nota-fisc.nr-nota-fis = ttDanfe.nrnota    and
               it-nota-fisc.cod-estabel = pCodEstabel       and
               it-nota-fisc.it-codigo   = ttDanfeItem.cprod no-error.  
    if avail it-nota-fisc then do:
    
        find ext-it-nota-fisc-fcp no-lock where
             ext-it-nota-fisc-fcp.cod-estabel = it-nota-fisc.cod-estabel and
             ext-it-nota-fisc-fcp.serie       = it-nota-fisc.serie       and
             ext-it-nota-fisc-fcp.nr-nota-fis = it-nota-fisc.nr-nota-fis and
             ext-it-nota-fisc-fcp.nr-seq-fat  = it-nota-fisc.nr-seq-fat  and
             ext-it-nota-fisc-fcp.it-codigo   = it-nota-fisc.it-codigo   no-error.
        if avail ext-it-nota-fisc-fcp then
            assign cDescProdutoComST = cDescProdutoComST + " VL " + ext-it-nota-fisc-fcp.des-imp + ": " + string(ext-it-nota-fisc-fcp.val-fcp, ">>>,>>9.99").
    
    end.    
    /*Calculo FCP - FIM*/
       
    RUN piTrataQuebraCampo (INPUT "DescProduto":U, 
                            INPUT cDescProdutoComST,
                            OUTPUT cDescProdutoComST2,
                            OUTPUT cSeparador).

    RUN piTrataQuebraCampo (INPUT "NCM":U, 
                            INPUT ttDanfeItem.ncm,
                            OUTPUT cNCM,
                            OUTPUT cSeparador).

    RUN piTrataQuebraCampo (INPUT "CST":U, 
                            INPUT ttDanfeItem.s,
                            OUTPUT cCST,
                            OUTPUT cSeparador).

    RUN piTrataQuebraCampo (INPUT "CFOP":U, 
                            INPUT ttDanfeItem.cfop,
                            OUTPUT cCFOP,
                            OUTPUT cSeparador).

    RUN piTrataQuebraCampo (INPUT "Un":U, 
                            INPUT ttDanfeItem.u,
                            OUTPUT cUn,
                            OUTPUT cSeparador).

    RUN piTrataQuebraCampo (INPUT "Quantidade":U, 
                            INPUT ttDanfeItem.quantitem,
                            OUTPUT cQuantidade,
                            OUTPUT cSeparador).
    
    RUN piTrataQuebraCampo (INPUT "ValorUni":U, 
                            INPUT ttDanfeItem.vlunit,
                            OUTPUT cValorUni,
                            OUTPUT cSeparador).

    RUN piTrataQuebraCampo (INPUT "ValorTotal":U, 
                            INPUT ttDanfeItem.vltotitem,
                            OUTPUT cValorTotal,
                            OUTPUT cSeparador).
    
    RUN piTrataQuebraCampo (INPUT "BaseCalcIcms":U, 
                            INPUT ttDanfeItem.vlbcicmit,
                            OUTPUT cBaseCalcIcms,
                            OUTPUT cSeparador).

    RUN piTrataQuebraCampo (INPUT "ValorIcms":U, 
                            INPUT ttDanfeItem.vlicmit,
                            OUTPUT cValorIcms,
                            OUTPUT cSeparador).

    RUN piTrataQuebraCampo (INPUT "ValorIpi":U, 
                            INPUT ttDanfeItem.vlipiit,
                            OUTPUT cValorIpi,
                            OUTPUT cSeparador).

    RUN piTrataQuebraCampo (INPUT "AliquotaIcms":U, 
                            INPUT ttDanfeItem.icm,
                            OUTPUT cAliquotaIcms,
                            OUTPUT cSeparador).

    RUN piTrataQuebraCampo (INPUT "AliquotaIpi":U, 
                            INPUT ttDanfeItem.ipi,
                            OUTPUT cAliquotaIpi,
                            OUTPUT cSeparador).

/*     message ttDanfeItem.vlbcicmit-st skip */
/*             ttDanfeItem.vlicmit-st view-as alert-box. */

    RUN piTrataQuebraCampo (INPUT "BaseCalcIcmsST":U, 
                            INPUT ttDanfeItem.vlbcicmit-st,
                            OUTPUT cBaseCalcIcmsST,
                            OUTPUT cSeparador).

    RUN piTrataQuebraCampo (INPUT "ValorIcmsST":U,    
                            INPUT ttDanfeItem.vlicmit-st,
                            OUTPUT cValorIcmsST,
                            OUTPUT cSeparador).

    IF  DEC(ttDanfeItem.vlunit-trib) <> 0                       AND
        DEC(ttDanfeItem.vlunit-trib) <> DEC(ttDanfeItem.vlunit) THEN DO:

        RUN piTrataQuebraCampo (INPUT "Un":U, 
                                INPUT ttDanfeItem.u-trib,
                                OUTPUT cUnTrib,
                                OUTPUT cSeparador).

        RUN piTrataQuebraCampo (INPUT "Quantidade":U, 
                                INPUT ttDanfeItem.quantitem-trib,
                                OUTPUT cQuantidadeTrib,
                                OUTPUT cSeparador).

        RUN piTrataQuebraCampo (INPUT "ValorUni":U, 
                                INPUT ttDanfeItem.vlunit-trib,
                                OUTPUT cValorUniTrib,
                                OUTPUT cSeparador).
        
        ASSIGN iMaxEntries = MAX(NUM-ENTRIES(cUn        , cSeparador),
                                 NUM-ENTRIES(cQuantidade, cSeparador),
                                 NUM-ENTRIES(cValorUni  , cSeparador)).

        DO  iCont = 1 TO iMaxEntries:
            
            IF  NUM-ENTRIES(cUn, cSeparador) <= iCont THEN
                ASSIGN cUn = TRIM(cUn) + cSeparador.

            IF  NUM-ENTRIES(cQuantidade, cSeparador) <= iCont THEN
                ASSIGN cQuantidade = TRIM(cQuantidade) + cSeparador.

            IF  NUM-ENTRIES(cValorUni, cSeparador) <= iCont THEN
                ASSIGN cValorUni = TRIM(cValorUni) + cSeparador.

        END.

        ASSIGN cUn         = TRIM(cUn)         + TRIM(cUnTrib)
               cQuantidade = TRIM(cQuantidade) + TRIM(cQuantidadeTrib)
               cValorUni   = TRIM(cValorUni)   + TRIM(cValorUniTrib).

    END.
    ASSIGN iNumLinhasOcupadasProduto = MAX(NUM-ENTRIES(cCodProduto    , cSeparador),
                                           NUM-ENTRIES(cDescProdutoComST2   , cSeparador),
                                           NUM-ENTRIES(cNCM           , cSeparador),
                                           NUM-ENTRIES(cCST           , cSeparador),
                                           NUM-ENTRIES(cCFOP          , cSeparador),
                                           NUM-ENTRIES(cUn            , cSeparador),
                                           NUM-ENTRIES(cQuantidade    , cSeparador),
                                           NUM-ENTRIES(cValorUni      , cSeparador),
                                           NUM-ENTRIES(cValorTotal    , cSeparador),
                                           NUM-ENTRIES(cBaseCalcIcms  , cSeparador),
                                           NUM-ENTRIES(cValorIcms     , cSeparador),
                                           NUM-ENTRIES(cValorIpi      , cSeparador),
                                           NUM-ENTRIES(cAliquotaIcms  , cSeparador),
                                           NUM-ENTRIES(cAliquotaIpi   , cSeparador),
                                           NUM-ENTRIES(cBaseCalcIcmsST, cSeparador),
                                           NUM-ENTRIES(cValorIcmsST   , cSeparador)).
    
    DO  iCont = 1 TO iNumLinhasOcupadasProduto:
        
        ASSIGN iLinhaPaginaAtual = iLinhaPaginaAtual + 1
               iSeqTtLinha       = iSeqTtLinha + 1.

        RUN piControlePaginaAtual. 
        
        IF  iCont = 1              AND 
            lEmiteTracejado        AND 
            iLinhaPaginaAtual > 1  AND
            NOT lUltimaLinhaPagina THEN DO:

            RUN piCriaLinhaTracejada (INPUT iSeqTtLinha).

            ASSIGN iLinhaPaginaAtual = iLinhaPaginaAtual + 1
                   iSeqTtLinha       = iSeqTtLinha + 1
                   lEmiteTracejado   = NO.
                   
            RUN piControlePaginaAtual. 
        END.

        RUN piCriaTtLinha (INPUT iSeqTtLinha,
                           INPUT "CodProduto":U,
                           INPUT (IF  NUM-ENTRIES(cCodProduto, cSeparador) >= iCont 
                                  THEN ENTRY (iCont, cCodProduto, cSeparador)
                                  ELSE "") ).
        RUN piCriaTtLinha (INPUT iSeqTtLinha,
                           INPUT "DescProduto":U,
                           INPUT (IF  NUM-ENTRIES(cDescProdutoComST2, cSeparador) >= iCont 
                                         THEN ENTRY (iCont, cDescProdutoComST2, cSeparador)
                                         ELSE "")).
        RUN piCriaTtLinha (INPUT iSeqTtLinha,
                           INPUT "NCM":U,
                           INPUT (IF  NUM-ENTRIES(cNCM, cSeparador) >= iCont 
                                         THEN ENTRY (iCont, cNCM, cSeparador)
                                         ELSE "")).
        RUN piCriaTtLinha (INPUT iSeqTtLinha,
                           INPUT "CST":U,
                           INPUT (IF  NUM-ENTRIES(cCST, cSeparador) >= iCont 
                                         THEN ENTRY (iCont, cCST, cSeparador)
                                         ELSE "")).
        RUN piCriaTtLinha (INPUT iSeqTtLinha,
                           INPUT "CFOP":U,
                           INPUT (IF  NUM-ENTRIES(cCFOP, cSeparador) >= iCont 
                                         THEN ENTRY (iCont, cCFOP, cSeparador)
                                         ELSE "")).
        RUN piCriaTtLinha (INPUT iSeqTtLinha,
                           INPUT "Un":U,
                           INPUT (IF  NUM-ENTRIES(cUn, cSeparador) >= iCont 
                                         THEN ENTRY (iCont, cUn, cSeparador)
                                         ELSE "")). 
        RUN piCriaTtLinha (INPUT iSeqTtLinha,
                           INPUT "Quantidade":U,
                           INPUT (IF  NUM-ENTRIES(cQuantidade, cSeparador) >= iCont 
                                         THEN ENTRY (iCont, cQuantidade, cSeparador)
                                         ELSE "")).
        RUN piCriaTtLinha (INPUT iSeqTtLinha,
                           INPUT "ValorUni":U,
                           INPUT (IF  NUM-ENTRIES(cValorUni, cSeparador) >= iCont 
                                         THEN ENTRY (iCont, cValorUni, cSeparador)
                                         ELSE "")). 
        RUN piCriaTtLinha (INPUT iSeqTtLinha,
                           INPUT "ValorTotal":U,
                           INPUT (IF  NUM-ENTRIES(cValorTotal, cSeparador) >= iCont 
                                         THEN ENTRY (iCont, cValorTotal, cSeparador)
                                         ELSE "")).
        RUN piCriaTtLinha (INPUT iSeqTtLinha,
                           INPUT "BaseCalcIcms":U,
                           INPUT (IF  NUM-ENTRIES(cBaseCalcIcms, cSeparador) >= iCont 
                                         THEN ENTRY (iCont, cBaseCalcIcms, cSeparador)
                                         ELSE "")).
        RUN piCriaTtLinha (INPUT iSeqTtLinha,
                           INPUT "ValorIcms":U,
                           INPUT (IF  NUM-ENTRIES(cValorIcms, cSeparador) >= iCont 
                                         THEN ENTRY (iCont, cValorIcms, cSeparador)
                                         ELSE "")).
        RUN piCriaTtLinha (INPUT iSeqTtLinha,
                           INPUT "ValorIpi":U,
                           INPUT (IF  NUM-ENTRIES(cValorIpi, cSeparador) >= iCont 
                                         THEN ENTRY (iCont, cValorIpi, cSeparador)
                                         ELSE "")).
        RUN piCriaTtLinha (INPUT iSeqTtLinha,
                           INPUT "AliquotaIcms":U,
                           INPUT (IF  NUM-ENTRIES(cAliquotaIcms, cSeparador) >= iCont 
                                         THEN ENTRY (iCont, cAliquotaIcms, cSeparador)
                                         ELSE "")).
        RUN piCriaTtLinha (INPUT iSeqTtLinha,
                           INPUT "AliquotaIpi":U,
                           INPUT (IF  NUM-ENTRIES(cAliquotaIpi, cSeparador) >= iCont 
                                         THEN ENTRY (iCont, cAliquotaIpi, cSeparador)
                                         ELSE "")).
        RUN piCriaTtLinha (INPUT iSeqTtLinha,
                           INPUT "BaseCalcIcmsST":U,
                           INPUT (IF  NUM-ENTRIES(cBaseCalcIcmsST, cSeparador) >= iCont 
                                         THEN ENTRY (iCont, cBaseCalcIcmsST, cSeparador)
                                         ELSE "")).
        RUN piCriaTtLinha (INPUT iSeqTtLinha,
                           INPUT "ValorIcmsST":U,
                           INPUT (IF  NUM-ENTRIES(cValorIcmsST, cSeparador) >= iCont 
                                         THEN ENTRY (iCont, cValorIcmsST, cSeparador)
                                         ELSE "")). 
       
    END. /* REPEAT: */

END. /* FOR EACH ttDanfeItem: */

ASSIGN iSeqTotal = iSeqTtLinha.

ASSIGN iPagTotal = iSeqTotal - iNumLinhasProdPrimeiraPagina
       iPagTotal = IF iPagTotal <= 0 
                   THEN 1 
                   ELSE (2 + (IF iPagTotal MOD iNumLinhasProdOutrasPaginas = 0 
                              THEN -1 + 1 * INT(TRUNC(iPagTotal / iNumLinhasProdOutrasPaginas, 0)) 
                              ELSE INT(TRUNC(iPagTotal / iNumLinhasProdOutrasPaginas, 0)))).

FOR FIRST ttDanfe:

    IF pSemWord THEN DO:
       IF ttDanfe.chavedeacessoadicionalnfe <> "" THEN DO:
          
          IF cModeloDanfe = '3' THEN  /* Retrato */
             ASSIGN gcDiretorioDanfe   = SEARCH("layout/danfev2modwvr-cont":U + string(iPagTotal) + ".rtf":U). /* Retrato */
          ELSE 
             ASSIGN gcDiretorioDanfe   = SEARCH("layout/danfev2modwvp-cont":U + string(iPagTotal) + ".rtf":U). /* Paisagem */
       END.
       ELSE DO:
           IF cModeloDanfe = '3' THEN  /* Retrato */
             ASSIGN gcDiretorioDanfe   = SEARCH("layout/danfev2modwvr":U + string(iPagTotal) + ".rtf":U). /* Retrato */
          ELSE 
             ASSIGN gcDiretorioDanfe   = SEARCH("layout/danfev2modwvp":U + string(iPagTotal) + ".rtf":U). /* Paisagem */
       END.
       ASSIGN gcDiretorioDanfeIt = gcDiretorioDanfe.
    END. 
    ELSE DO:
       /*--- medida utilizada para somente apresentar o c¢digo de barras dos dados da DANFE quando contingància ---------*/
       IF  ttDanfe.chavedeacessoadicionalnfe <> "" THEN DO:         

           ASSIGN gcDiretorioDanfe   = IF cModeloDanfe = '3' THEN SEARCH("layout/danfev2mod1-cont.rtf":U) /* Retrato */
                                                             ELSE SEARCH("layout/danfev2mod2-cont.rtf":U). /* Paisagem */
           ASSIGN gcDiretorioDanfeIt = IF cModeloDanfe = '3' THEN SEARCH("layout/danfev2mod1it-cont.rtf":U) /* Retrato */
                                                             ELSE SEARCH("layout/danfev2mod2it-cont.rtf":U). /* Paisagem */
       END.
       ELSE DO:       
             
           /*assign gcDiretorioDanfe   = search(substitute("layout/danfemod1-&1.rtf", pCodEstabel))
                  gcDiretorioDanfeIt = search(substitute("layout/danfemod1it-&1.rtf", pCodEstabel)). */    

           ASSIGN gcDiretorioDanfe   = IF cModeloDanfe = '3' THEN SEARCH(substitute("layout/danfev2mod1-&1.rtf", pCodEstabel)) /* Retrato */
                                                             ELSE SEARCH(substitute("layout/danfev2mod2-&1.rtf", pCodEstabel)). /* Paisagem */
           ASSIGN gcDiretorioDanfeIt = IF cModeloDanfe = '3' THEN SEARCH(substitute("layout/danfev2mod1it-&1.rtf", pCodEstabel)) /* Retrato */
                                                             ELSE SEARCH(substitute("layout/danfev2mod2it-&1.rtf", pCodEstabel)). /* Paisagem */                                                          
                                                                        
                                                             
       END.
    END.

    IF  gcDiretorioDanfe   = ?  OR  gcDiretorioDanfe   = "" THEN NEXT.
    IF  gcDiretorioDanfeIt = ?  OR  gcDiretorioDanfeIt = "" THEN NEXT.

    /*----------------------------------------------------------------------------------------------------------------*/
    
    INPUT  STREAM sInput  FROM VALUE(gcDiretorioDanfe).
    OUTPUT STREAM sOutput TO VALUE(SESSION:TEMP-DIRECTORY + "/" + pcArq) NO-CONVERT.

    FIND FIRST nota-fiscal
        WHERE nota-fiscal.serie = ttDanfe.ser AND
              nota-fiscal.nr-nota-fis = ttDanfe.nrnota AND
              nota-fiscal.cod-estabel = pCodEstabel NO-LOCK NO-ERROR.
    IF AVAIL nota-fiscal THEN
        IF nota-fiscal.nr-pedcli = "" THEN DO:
            FIND FIRST es-ped-nft NO-LOCK
                WHERE es-ped-nft.cod-estabel = nota-fiscal.cod-estabel
                  AND es-ped-nft.serie       = nota-fiscal.serie
                  AND es-ped-nft.nr-nota-fis = nota-fiscal.nr-nota-fis NO-ERROR.
            IF AVAIL es-ped-nft THEN
                ASSIGN cInfComplPedCli = string(es-ped-nft.nr-pedido).
            ELSE
                ASSIGN cInfComplPedCli = "".
        END.
        ELSE
            ASSIGN cInfComplPedCli = nota-fiscal.nr-pedcli.
    ELSE
        ASSIGN cInfComplPedCli = "".

    REPEAT:                         
        IMPORT STREAM sInput UNFORMAT cLinha.
        IF  INDEX(cLinha,"#":U) > 0 THEN DO:
            ASSIGN cLinha = REPLACE(cLinha, "#BCCODE1281":U,                {ftp/ft0518f.i7 ttDanfe.BCCODE128-chave              }). /*Chave de Acesso NF-e*/
            ASSIGN cLinha = REPLACE(cLinha, "#BCCODE1282":U,                {ftp/ft0518f.i7 ttDanfe.BCCODE128-chaveadicional     }). /*Chave de Acesso Adicional - Para impress∆o em Contingància*/
            ASSIGN cLinha = REPLACE(cLinha, "#razaosocialempresa":U,        {ftp/ft0518f.i7 ttDanfe.razaosocialempresa           }).
            ASSIGN cLinha = REPLACE(cLinha, "#sn":U,                        {ftp/ft0518f.i7 ttDanfe.sn                           }).
            ASSIGN cLinha = REPLACE(cLinha, "#enderecoemp":U,               {ftp/ft0518f.i7 ttDanfe.enderecoemp                  }).
            ASSIGN cLinha = REPLACE(cLinha, "#bairroemp":U,                 {ftp/ft0518f.i7 ttDanfe.bairroemp                    }).
            ASSIGN cLinha = REPLACE(cLinha, "#cidadeemp":U,                 {ftp/ft0518f.i7 ttDanfe.cidadeemp                    }).
            ASSIGN cLinha = REPLACE(cLinha, "#ufemp":U,                     {ftp/ft0518f.i7 ttDanfe.ufemp                        }).
            ASSIGN cLinha = REPLACE(cLinha, "#cepemp":U,                    {ftp/ft0518f.i7 ttDanfe.cepemp                       }).
            ASSIGN cLinha = REPLACE(cLinha, "#foneemp":U,                   {ftp/ft0518f.i7 ttDanfe.foneemp                      }).
            ASSIGN cLinha = REPLACE(cLinha, "#siteemp":U,                   {ftp/ft0518f.i7 ttDanfe.siteemp                      }).
            ASSIGN cLinha = REPLACE(cLinha, "#nrnota":U,                    {ftp/ft0518f.i7 ttDanfe.nrnota                       }).
            ASSIGN cLinha = REPLACE(cLinha, "#ser":U,                       {ftp/ft0518f.i7 ttDanfe.ser                          }).
            ASSIGN cLinha = REPLACE(cLinha, "#n1":U,                        {ftp/ft0518f.i7 '1'                                  }).
            ASSIGN cLinha = REPLACE(cLinha, "#nnn":U,                       {ftp/ft0518f.i7 TRIM(STRING(iPagTotal))              }).
            ASSIGN cLinha = REPLACE(cLinha, "#naturezaoperacao":U,          {ftp/ft0518f.i7 caps(ttDanfe.naturezaoperacao)       }).
            ASSIGN cLinha = REPLACE(cLinha, "#inscrestadempresa":U,         {ftp/ft0518f.i7 ttDanfe.inscrestadempresa            }).
            ASSIGN cLinha = REPLACE(cLinha, "#inscrestadsubstituto":U,      {ftp/ft0518f.i7 ttDanfe.inscrestadsubstituto         }).
            ASSIGN cLinha = REPLACE(cLinha, "#cnpjempresa":U,               {ftp/ft0518f.i7 ttDanfe.cnpjempresa                  }).
            ASSIGN cLinha = REPLACE(cLinha, "#cnpjdestinatario":U,          {ftp/ft0518f.i7 ttDanfe.cnpjdestinatario             }).
            ASSIGN cLinha = REPLACE(cLinha, "#chavedeacessonfe":U,          {ftp/ft0518f.i7 ttDanfe.chavedeacessonfe             }).
            ASSIGN cLinha = REPLACE(cLinha, "#dadosdanfe":U,                {ftp/ft0518f.i7 ttDanfe.chavedeacessoadicionalnfe    }).
            ASSIGN cLinha = REPLACE(cLinha, "#protocoloautorizacao":U,      {ftp/ft0518f.i7 ttDanfe.protocoloautorizacao         }).
            ASSIGN cLinha = REPLACE(cLinha, "#razaosocialdestinatario":U,   {ftp/ft0518f.i7 caps(ttDanfe.razaosocialdestinatario)}).
            ASSIGN cLinha = REPLACE(cLinha, "#dataemissao":U,               {ftp/ft0518f.i7 ttDanfe.dataemissao                  }).
            ASSIGN cLinha = REPLACE(cLinha, "#dataentrega":U,               {ftp/ft0518f.i7 ttDanfe.dataentrega                  }).
            ASSIGN cLinha = REPLACE(cLinha, "#horasaida":U,                 {ftp/ft0518f.i7 ttDanfe.horasaida                    }).
            ASSIGN cLinha = REPLACE(cLinha, "#enderecodestinatario":U,      {ftp/ft0518f.i7 caps(ttDanfe.enderecodestinatario)   }).
            ASSIGN cLinha = REPLACE(cLinha, "#cidadedestinatario":U,        {ftp/ft0518f.i7 caps(ttDanfe.cidadedestinatario)     }).
            ASSIGN cLinha = REPLACE(cLinha, "#bairrodestinatario":U,        {ftp/ft0518f.i7 caps(ttDanfe.bairrodestinatario)     }).
            ASSIGN cLinha = REPLACE(cLinha, "#cepdestinatario":U,           {ftp/ft0518f.i7 ttDanfe.cepdestinatario              }).
            ASSIGN cLinha = REPLACE(cLinha, "#fonedestinatario":U,          {ftp/ft0518f.i7 ttDanfe.fonedestinatario             }).
            ASSIGN cLinha = REPLACE(cLinha, "#ufdest":U,                    {ftp/ft0518f.i7 caps(ttDanfe.ufdest)                 }).
            ASSIGN cLinha = REPLACE(cLinha, "#inscrestaddestinatario":U,    {ftp/ft0518f.i7 ttDanfe.inscrestaddestinatario       }).
            ASSIGN cLinha = REPLACE(cLinha, "#fatura1":U,                   {ftp/ft0518f.i7 ttDanfe.fatura1                      }).
            ASSIGN cLinha = REPLACE(cLinha, "#vencfat1":U,                  {ftp/ft0518f.i7 ttDanfe.vencfat1                     }).
            ASSIGN cLinha = REPLACE(cLinha, "#vlfat1":U,                    {ftp/ft0518f.i7 ttDanfe.vlfat1                       }).
            ASSIGN cLinha = REPLACE(cLinha, "#fatura2":U,                   {ftp/ft0518f.i7 ttDanfe.fatura2                      }).
            ASSIGN cLinha = REPLACE(cLinha, "#vencfat2":U,                  {ftp/ft0518f.i7 ttDanfe.vencfat2                     }).
            ASSIGN cLinha = REPLACE(cLinha, "#vlfat2":U,                    {ftp/ft0518f.i7 ttDanfe.vlfat2                       }).
            ASSIGN cLinha = REPLACE(cLinha, "#fatura3":U,                   {ftp/ft0518f.i7 ttDanfe.fatura3                      }).
            ASSIGN cLinha = REPLACE(cLinha, "#vencfat3":U,                  {ftp/ft0518f.i7 ttDanfe.vencfat3                     }).
            ASSIGN cLinha = REPLACE(cLinha, "#vlfat3":U,                    {ftp/ft0518f.i7 ttDanfe.vlfat3                       }).
            ASSIGN cLinha = REPLACE(cLinha, "#fatura4":U,                   {ftp/ft0518f.i7 ttDanfe.fatura4                      }).
            ASSIGN cLinha = REPLACE(cLinha, "#vencfat4":U,                  {ftp/ft0518f.i7 ttDanfe.vencfat4                     }).
            ASSIGN cLinha = REPLACE(cLinha, "#vlfat4":U,                    {ftp/ft0518f.i7 ttDanfe.vlfat4                       }).
            ASSIGN cLinha = REPLACE(cLinha, "#fatura5":U,                   {ftp/ft0518f.i7 ttDanfe.fatura5                      }).
            ASSIGN cLinha = REPLACE(cLinha, "#vencfat5":U,                  {ftp/ft0518f.i7 ttDanfe.vencfat5                     }).
            ASSIGN cLinha = REPLACE(cLinha, "#vlfat5":U,                    {ftp/ft0518f.i7 ttDanfe.vlfat5                       }).
            ASSIGN cLinha = REPLACE(cLinha, "#fatura6":U,                   {ftp/ft0518f.i7 ttDanfe.fatura6                      }).
            ASSIGN cLinha = REPLACE(cLinha, "#vencfat6":U,                  {ftp/ft0518f.i7 ttDanfe.vencfat6                     }).
            ASSIGN cLinha = REPLACE(cLinha, "#vlfat6":U,                    {ftp/ft0518f.i7 ttDanfe.vlfat6                       }).
            ASSIGN cLinha = REPLACE(cLinha, "#fatura7":U,                   {ftp/ft0518f.i7 ttDanfe.fatura7                      }).
            ASSIGN cLinha = REPLACE(cLinha, "#vencfat7":U,                  {ftp/ft0518f.i7 ttDanfe.vencfat7                     }).
            ASSIGN cLinha = REPLACE(cLinha, "#vlfat7":U,                    {ftp/ft0518f.i7 ttDanfe.vlfat7                       }).
            ASSIGN cLinha = REPLACE(cLinha, "#fatura8":U,                   {ftp/ft0518f.i7 ttDanfe.fatura8                      }).
            ASSIGN cLinha = REPLACE(cLinha, "#vencfat8":U,                  {ftp/ft0518f.i7 ttDanfe.vencfat8                     }).
            ASSIGN cLinha = REPLACE(cLinha, "#vlfat8":U,                    {ftp/ft0518f.i7 ttDanfe.vlfat8                       }).
            ASSIGN cLinha = REPLACE(cLinha, "#vlbcicmsnota":U,              {ftp/ft0518f.i7 ttDanfe.vlbcicmsnota                 }).
            ASSIGN cLinha = REPLACE(cLinha, "#vlicmsnota":U,                {ftp/ft0518f.i7 ttDanfe.vlicmsnota                   }).
            ASSIGN cLinha = REPLACE(cLinha, "#vlbcicmsstnota":U,            {ftp/ft0518f.i7 ttDanfe.vlbcicmsstnota               }).
            ASSIGN cLinha = REPLACE(cLinha, "#vlicmsstnota":U,              {ftp/ft0518f.i7 ttDanfe.vlicmsstnota                 }).
            ASSIGN cLinha = REPLACE(cLinha, "#vltotprod":U,                 {ftp/ft0518f.i7 ttDanfe.vltotprod                    }).
            ASSIGN cLinha = REPLACE(cLinha, "#vlfretenota":U,               {ftp/ft0518f.i7 ttDanfe.vlfretenota                  }).
            ASSIGN cLinha = REPLACE(cLinha, "#vlseguronota":U,              {ftp/ft0518f.i7 ttDanfe.vlseguronota                 }).
            ASSIGN cLinha = REPLACE(cLinha, "#vldescontonota":U,            {ftp/ft0518f.i7 ttDanfe.vldescontonota               }).
            ASSIGN cLinha = REPLACE(cLinha, "#vldespesasnota":U,            {ftp/ft0518f.i7 ttDanfe.vldespesasnota               }).
            ASSIGN cLinha = REPLACE(cLinha, "#vlipinota":U,                 {ftp/ft0518f.i7 ttDanfe.vlipinota                    }).
            ASSIGN cLinha = REPLACE(cLinha, "#vltotnota":U,                 {ftp/ft0518f.i7 ttDanfe.vltotnota                    }).
            ASSIGN cLinha = REPLACE(cLinha, "#nometransp":U,                {ftp/ft0518f.i7 caps(ttDanfe.nometransp)             }).
            ASSIGN cLinha = REPLACE(cLinha, "#idfr":U,                      {ftp/ft0518f.i7 ttDanfe.idfr                         }).
            ASSIGN cLinha = REPLACE(cLinha, "#codantt1":U,                  {ftp/ft0518f.i7 ttDanfe.codantt1                     }).
            ASSIGN cLinha = REPLACE(cLinha, "#codantt2":U,                  {ftp/ft0518f.i7 ttDanfe.codantt2                     }).
            ASSIGN cLinha = REPLACE(cLinha, "#placa1":U,                    {ftp/ft0518f.i7 caps(ttDanfe.placa1)                 }).
            ASSIGN cLinha = REPLACE(cLinha, "#placa2":U,                    {ftp/ft0518f.i7 caps(ttDanfe.placa2)                 }).
            ASSIGN cLinha = REPLACE(cLinha, "#ufpl1":U,                     {ftp/ft0518f.i7 caps(ttDanfe.ufpl1)                  }).
            ASSIGN cLinha = REPLACE(cLinha, "#ufpl2":U,                     {ftp/ft0518f.i7 caps(ttDanfe.ufpl2)                  }).
            ASSIGN cLinha = REPLACE(cLinha, "#cnpjtransp":U,                {ftp/ft0518f.i7 ttDanfe.cnpjtransp                   }).
            ASSIGN cLinha = REPLACE(cLinha, "#enderecotransp":U,            {ftp/ft0518f.i7 caps(ttDanfe.enderecotransp)         }).
            ASSIGN cLinha = REPLACE(cLinha, "#cidadetransp":U,              {ftp/ft0518f.i7 caps(ttDanfe.cidadetransp)           }).
            ASSIGN cLinha = REPLACE(cLinha, "#uftran":U,                    {ftp/ft0518f.i7 caps(ttDanfe.uftran)                 }).
            ASSIGN cLinha = REPLACE(cLinha, "#inscrestadtransp":U,          {ftp/ft0518f.i7 ttDanfe.inscrestadtransp             }).
            ASSIGN cLinha = REPLACE(cLinha, "#qtvolume":U,                  {ftp/ft0518f.i7 ttDanfe.qtvolume                     }).
            ASSIGN cLinha = REPLACE(cLinha, "#especievolume":U,             {ftp/ft0518f.i7 caps(ttDanfe.especievolume)          }).
            ASSIGN cLinha = REPLACE(cLinha, "#marcavolume":U,               {ftp/ft0518f.i7 caps(ttDanfe.marcavolume)            }).
            ASSIGN cLinha = REPLACE(cLinha, "#numeracaovolume":U,           {ftp/ft0518f.i7 ttDanfe.numeracaovolume              }).
            ASSIGN cLinha = REPLACE(cLinha, "#pesobrutototal":U,            {ftp/ft0518f.i7 ttDanfe.pesobrutototal               }).
            ASSIGN cLinha = REPLACE(cLinha, "#pesoliquidototal":U,          {ftp/ft0518f.i7 ttDanfe.pesoliquidototal             }).
            ASSIGN cLinha = REPLACE(cLinha, "#inscrmunicipaliss":U,         {ftp/ft0518f.i7 ttDanfe.inscrmunicipaliss            }).
            ASSIGN cLinha = REPLACE(cLinha, "#vltotalsevicos":U,            {ftp/ft0518f.i7 ttDanfe.vltotalsevicos               }).
            ASSIGN cLinha = REPLACE(cLinha, "#vlbciss":U,                   {ftp/ft0518f.i7 ttDanfe.vlbciss                      }).
            ASSIGN cLinha = REPLACE(cLinha, "#vlisstotal":U,                {ftp/ft0518f.i7 ttDanfe.vlisstotal                   }).
            ASSIGN cLinha = REPLACE(cLinha, "#informacoescomplementares":U, {ftp/ft0518f.i7 ttDanfe.informacoescomplementares    }).
            ASSIGN cLinha = REPLACE(cLinha, "#infcomplpedcli":U,            {ftp/ft0518f.i7 cInfComplPedCli                      }).
            ASSIGN cLinha = REPLACE(cLinha, "#contingencia":U,              {ftp/ft0518f.i7 ''                                   }).
            ASSIGN cLinha = REPLACE(cLinha, "#homologacao1":U,              {ftp/ft0518f.i7 ttDanfe.homologacao1                 }).
            ASSIGN cLinha = REPLACE(cLinha, "#homologacao2":U,              {ftp/ft0518f.i7 ttDanfe.homologacao2                 }).
            ASSIGN cLinha = REPLACE(cLinha, "#conteudovariavel1":U,         {ftp/ft0518f.i7 ttDanfe.conteudovariavel1            }).
            ASSIGN cLinha = REPLACE(cLinha, "#conteudovariavel2":U,         {ftp/ft0518f.i7 ttDanfe.conteudovariavel2            }).
            
        END.

        IF  INDEX(cLinha,"$":U) > 0 THEN DO:
            RUN piImprimeQuadroProdutos.
        END.

        PUT STREAM sOutput UNFORMATTED cLinha SKIP.
    END.
    PUT STREAM sOutput "}}":U.

    INPUT  STREAM sInput  CLOSE.
    OUTPUT STREAM sOutput CLOSE.
END.

IF  NOT pSemWord THEN DO:
    FOR FIRST ttDanfe:
        ASSIGN c-danfeaux = string(RANDOM(1,9999999)).
        DO  iPagAtual = 2 TO iPagTotal:
            INPUT  STREAM sInput  FROM VALUE(gcDiretorioDanfeit).
            IF  NOT l-danfe THEN
                OUTPUT STREAM sOutput TO VALUE(SESSION:TEMP-DIRECTORY + "/" + "danfeaux":U + TRIM(STRING(iPagAtual)) + ".doc":U) NO-CONVERT.
            ELSE 
                OUTPUT STREAM sOutput TO VALUE(SESSION:TEMP-DIRECTORY + "/" + "danfeaux":U + c-danfeaux + TRIM(STRING(iPagAtual)) + ".doc":U) NO-CONVERT.
            
            REPEAT:                             
                IMPORT STREAM sInput UNFORMAT cLinha.
                IF  INDEX(cLinha,"#":U) > 0 THEN DO:
                    ASSIGN cLinha = REPLACE(cLinha, "#BCCODE1281":U,           {ftp/ft0518f.i7 ttDanfe.BCCODE128-chave           }).
                    ASSIGN cLinha = REPLACE(cLinha, "#BCCODE1282":U,           {ftp/ft0518f.i7 ttDanfe.BCCODE128-chaveadicional  }).
                    ASSIGN cLinha = REPLACE(cLinha, "#razaosocialempresa":U,   {ftp/ft0518f.i7 ttDanfe.razaosocialempresa        }).
                    ASSIGN cLinha = REPLACE(cLinha, "#sn":U,                   {ftp/ft0518f.i7 ttDanfe.sn                        }).
                    ASSIGN cLinha = REPLACE(cLinha, "#enderecoemp":U,          {ftp/ft0518f.i7 ttDanfe.enderecoemp               }).
                    ASSIGN cLinha = REPLACE(cLinha, "#bairroemp":U,            {ftp/ft0518f.i7 ttDanfe.bairroemp                 }).
                    ASSIGN cLinha = REPLACE(cLinha, "#cidadeemp":U,            {ftp/ft0518f.i7 ttDanfe.cidadeemp                 }).
                    ASSIGN cLinha = REPLACE(cLinha, "#ufemp":U,                {ftp/ft0518f.i7 ttDanfe.ufemp                     }).
                    ASSIGN cLinha = REPLACE(cLinha, "#cepemp":U,               {ftp/ft0518f.i7 ttDanfe.cepemp                    }).
                    ASSIGN cLinha = REPLACE(cLinha, "#foneemp":U,              {ftp/ft0518f.i7 ttDanfe.foneemp                   }).
                    ASSIGN cLinha = REPLACE(cLinha, "#siteemp":U,              {ftp/ft0518f.i7 ttDanfe.siteemp                   }).
                    ASSIGN cLinha = REPLACE(cLinha, "#nrnota":U,               {ftp/ft0518f.i7 ttDanfe.nrnota                    }).
                    ASSIGN cLinha = REPLACE(cLinha, "#ser":U,                  {ftp/ft0518f.i7 ttDanfe.ser                       }).
                    ASSIGN cLinha = REPLACE(cLinha, "#n1":U,                   {ftp/ft0518f.i7 TRIM(STRING(iPagAtual))           }).
                    ASSIGN cLinha = REPLACE(cLinha, "#nnn":U,                  {ftp/ft0518f.i7 TRIM(STRING(iPagTotal))           }).
                    ASSIGN cLinha = REPLACE(cLinha, "#naturezaoperacao":U,     {ftp/ft0518f.i7 caps(ttDanfe.naturezaoperacao)    }).
                    ASSIGN cLinha = REPLACE(cLinha, "#inscrestadempresa":U,    {ftp/ft0518f.i7 ttDanfe.inscrestadempresa         }).
                    ASSIGN cLinha = REPLACE(cLinha, "#inscrestadsubstituto":U, {ftp/ft0518f.i7 ttDanfe.inscrestadsubstituto      }).
                    ASSIGN cLinha = REPLACE(cLinha, "#cnpjempresa":U,          {ftp/ft0518f.i7 ttDanfe.cnpjempresa               }).
                    ASSIGN cLinha = REPLACE(cLinha, "#chavedeacessonfe":U,     {ftp/ft0518f.i7 ttDanfe.chavedeacessonfe          }).
                    ASSIGN cLinha = REPLACE(cLinha, "#dadosdanfe":U,           {ftp/ft0518f.i7 ttDanfe.chavedeacessoadicionalnfe }).
                    ASSIGN cLinha = REPLACE(cLinha, "#protocoloautorizacao":U, {ftp/ft0518f.i7 ttDanfe.protocoloautorizacao      }).
                    ASSIGN cLinha = REPLACE(cLinha, "#conteudovariavel1":U,    {ftp/ft0518f.i7 ttDanfe.conteudovariavel1         }).
                    ASSIGN cLinha = REPLACE(cLinha, "#conteudovariavel2":U,    {ftp/ft0518f.i7 ttDanfe.conteudovariavel2         }).
                END.
                    
                IF  INDEX(cLinha,"$":U) > 0 THEN DO:
                    RUN piImprimeQuadroProdutos.
                END.

                PUT STREAM sOutput UNFORMATTED cLinha SKIP.
            END.

            PUT STREAM sOutput "}}":U.

            INPUT  STREAM sInput  CLOSE.
            OUTPUT STREAM sOutput CLOSE.
        END.
    END.
    
/*    message 'iPagTotal - ' iPagTotal
        view-as alert-box.            */
    
    IF  iPagTotal >= 2 THEN DO:
        CREATE 'Word.Application':U ch-app-word.                         /* Cria uma aplicaá∆o WORD */
        ch-app-word:WindowState = 2.                                     /* O estado dois para o Word Ç minimizado */
        ch-app-word:VISIBLE = NO.                                        /* Apenas para n∆o mostrar que o word est† sendo utilizado em tela */
        
        /* message 'session - ' SESSION:TEMP-DIRECTORY skip */
/*                 'arquivo - ' pcArq skip */
/*                 'search - ' search(SESSION:TEMP-DIRECTORY + '/' + pcArq). */
/*    */
        ch-app-word:Documents:ADD(SESSION:TEMP-DIRECTORY + '/' + pcArq). /* Inclui arquivo */
        
        /*message 'depois'.*/
        
/*        message 'teste 1'
            view-as alert-box.*/
        
        DO  iPagAtual = 2 TO iPagTotal:
            ch-app-word:SELECTION:EndKey(6).                             /* Posiciona cursor no final do arquivo */
            ch-app-word:SELECTION:InsertBreak(7).                        /* Qubra pagina antes de inserir arquivo */
            IF  NOT l-danfe THEN DO:
                ch-app-word:SELECTION:Insertfile(SESSION:TEMP-DIRECTORY + "/" + "danfeaux":U + TRIM(STRING(iPagAtual)) + ".doc":U). /* Insere arquivo no documento aberto */
                OS-DELETE VALUE(SESSION:TEMP-DIRECTORY + "/" + "danfeaux":U + TRIM(STRING(iPagAtual)) + ".doc":U) NO-ERROR. /* Elimina o arquivo auxiliar */
            END.
            ELSE DO:
                ch-app-word:SELECTION:Insertfile(SESSION:TEMP-DIRECTORY + "/" + "danfeaux":U + c-danfeaux + TRIM(STRING(iPagAtual)) + ".doc":U). /* Insere arquivo no documento aberto */
                OS-DELETE VALUE(SESSION:TEMP-DIRECTORY + "/" + "danfeaux":U + c-danfeaux + TRIM(STRING(iPagAtual)) + ".doc":U) NO-ERROR. /* Elimina o arquivo auxiliar */
            END.
        END.
        
        
/*        message 'teste 2'
            view-as alert-box.*/
        
        ch-app-word:ActiveDocument:SaveAs(SESSION:TEMP-DIRECTORY + "/" + pcArq). /* Salva o arquivo aberto no WORD com o nome final do arquivo */
        ch-app-word:ActiveDocument:CLOSE.                                /* Fecha o arquivo do WORD */
        ch-app-word:QUIT().                                              /* Fechar o WORD */
        RELEASE OBJECT ch-app-word.                                      /* Elimina o endereáo utilizado para o WORD na m†quina */
    END.

    IF l-EnviaDanfXml THEN DO:

        ASSIGN c-arquivo-danfe = SESSION:TEMP-DIRECTORY + pcArq.

        IF  NOT VALID-HANDLE(h-bodi135na) THEN DO:
            RUN dibo/bodi135na.p PERSISTENT SET h-bodi135na.
            RUN openQueryStatic IN h-bodi135na (INPUT "Main":U).
        END.
    
        RUN repositionRecord IN h-bodi135na (INPUT r-nota).
    
        RUN getCharField     IN h-bodi135na (INPUT  "cod-chave-aces-nf-eletro":U,
                                             OUTPUT cChaveAcesso).
        
        IF NOT VALID-HANDLE (h-bodi520) THEN DO:
            RUN dibo/bodi520.p PERSISTENT SET h-bodi520.
            RUN openQueryStatic IN h-bodi520 (INPUT "Main":U).
        END.
        
        RUN goToKey      IN h-bodi520 (INPUT pCodEstabel).
        RUN getCharField IN h-bodi520 (INPUT  "cod-caminho-xml":U,
                                       OUTPUT c-cod-caminho-xml).
        
        IF c-cod-caminho-xml <> "" THEN DO:
            ASSIGN cArquivoXML = c-cod-caminho-xml                                                             
                   cArquivoXML = REPLACE(cArquivoXML,"~\","/").                                                
                                                                                                                                   
            IF NOT SUBSTR(cArquivoXML, LENGTH(cArquivoXML), 1) = "/"  THEN                
                  ASSIGN cArquivoXML = cArquivoXML + "/".                                                        
                                                                                                                                   
            ASSIGN cArquivoXML = TRIM(cArquivoXML)                                                             
                                 + TRIM(STRING(pCodEstabel,"x(05)"))                                                
                                 + SUBSTR(cChaveAcesso,23,3)                                                     
                                 + TRIM(STRING(INTEGER(SUBSTR(cChaveAcesso,26,9)),">>9999999")).                
                                                                                                                                   
            ASSIGN cArquivoXML = cArquivoXML + ".xml":U.
        END.

        RUN pi-EnviaEmail.

        IF VALID-HANDLE(h-bodi520) THEN DO:
            DELETE PROCEDURE h-bodi520.
            ASSIGN h-bodi520 = ?.
        END.
    
        IF VALID-HANDLE(h-bodi135na) THEN DO:
            DELETE PROCEDURE h-bodi135na.
            ASSIGN h-bodi135na = ?.
        END.

    END.

END.

PROCEDURE piTrataQuebraCampo:
    DEFINE  INPUT PARAMETER cNomeCampo      AS CHARACTER NO-UNDO.
    DEFINE  INPUT PARAMETER cConteudoCampo  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER cConteudoQuebra AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER cCaracterQuebra AS CHARACTER NO-UNDO.

    FOR FIRST ttColunasDANFE 
        WHERE ttColunasDANFE.descricao = cNomeCampo:
    END.

    IF  AVAIL ttColunasDANFE THEN DO:
        RUN piQuebraColuna (INPUT cConteudoCampo, 
                            INPUT (IF cModeloDanfe = '3'  /* Retrato */
                                   THEN ttColunasDANFE.tamanhoRetrato
                                   ELSE ttColunasDANFE.tamanhoPaisagem),
                            OUTPUT cConteudoQuebra,
                            OUTPUT cCaracterQuebra).
        IF cNomeCampo = "CodProduto" THEN
            ASSIGN cConteudoQuebra = cConteudoCampo
                   cCaracterQuebra = "".

    END.
    ELSE
        ASSIGN cConteudoQuebra = cConteudoCampo.

END PROCEDURE.


PROCEDURE piQuebraColuna:
    DEFINE INPUT  PARAMETER cConteudo       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER dTamanhoColuna  AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER cConteudoQuebra AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER cCaracterQuebra AS CHARACTER NO-UNDO.

    DEFINE VARIABLE i                    AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cCaracterAtual       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dTamanhoChar         AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dTamanhoAtual        AS DECIMAL     NO-UNDO.
    
    ASSIGN cCaracterQuebra = CHR(1).

    IF  dTamanhoColuna > 0 THEN DO:

        ASSIGN cConteudo = TRIM(cConteudo).
    
        DO  i = 1 TO LENGTH(cConteudo):
    
            ASSIGN cCaracterAtual = SUBSTR(cConteudo, i, 1).
    
            FIND FIRST ttCaracteres
                 WHERE ttCaracteres.codigoAsc = ASC(cCaracterAtual) NO-ERROR.
    
            IF  NOT AVAIL ttCaracteres THEN
                FIND FIRST ttCaracteres
                 WHERE ttCaracteres.codigoAsc = ASC(fn-tira-acento(cCaracterAtual)) NO-ERROR.

            IF  NOT AVAIL ttCaracteres        OR
                ttCaracteres.tamanhoChar <= 0 THEN
                ASSIGN dTamanhoChar = 1.
            ELSE
                ASSIGN dTamanhoChar = ttCaracteres.tamanhoChar.
    
            ASSIGN dTamanhoAtual = dTamanhoAtual + dTamanhoChar.
    
            IF  dTamanhoAtual > dTamanhoColuna THEN DO:
                ASSIGN cConteudoQuebra = cConteudoQuebra + cCaracterQuebra
                       dTamanhoAtual   = dTamanhoChar.
            END.
    
            ASSIGN cConteudoQuebra = cConteudoQuebra + cCaracterAtual.

            /*IF  INDEX(cConteudo,"21.807") > 0 THEN
                MESSAGE cConteudoQuebra SKIP
                        cCaracterAtual SKIP
                        dTamanhoAtual SKIP
                        cConteudo
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    
        END.

    END.

END PROCEDURE.

PROCEDURE piImprimeQuadroProdutos:
    
    FOR EACH ttColunasDANFE
          BY ttColunasDANFE.codigo:

        RUN piReplaceColunaProduto (INPUT ttColunasDANFE.codigo).
       
    END.

END PROCEDURE.

PROCEDURE piReplaceColunaProduto:
    DEFINE INPUT PARAMETER cCodigoColuna AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cSeqLinha     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iSeqLinha     AS INTEGER     NO-UNDO.
    
    IF  INDEX(cLinha, cCodigoColuna) > 0 THEN
        ASSIGN cSeqLinha = SUBSTR(cLinha, (INDEX(cLinha, cCodigoColuna) + LENGTH(cCodigoColuna)), (IF pSemWord THEN 3 ELSE 2) ).

    IF  cSeqLinha <> "" THEN DO:

        IF  iPagAtual >= 2 THEN
            ASSIGN iSeqLinha = ( INT(cSeqLinha) + ((iPagAtual - 2) * iNumLinhasProdOutrasPaginas)  + iNumLinhasProdPrimeiraPagina ).
        ELSE
            ASSIGN iSeqLinha = INT(cSeqLinha).

        FOR FIRST ttLinha
            WHERE ttLinha.sequenciaLinha = iSeqLinha
              AND ttLinha.codigoColuna   = cCodigoColuna: END.
        
        ASSIGN cLinha = REPLACE(cLinha, (cCodigoColuna + TRIM(cSeqLinha)), (IF AVAIL ttLinha THEN ttLinha.conteudoCampo ELSE "") ).
        
    END.

END PROCEDURE.

PROCEDURE piRetornaCodigoColuna:
    DEFINE INPUT PARAMETER cDescricao AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER cCodigo    AS CHARACTER NO-UNDO.

    FOR FIRST ttColunasDANFE
        WHERE ttColunasDANFE.descricao = cDescricao:
        ASSIGN cCodigo = ttColunasDANFE.codigo.
    END.

END PROCEDURE.
              
PROCEDURE piCriaTtLinha:
    DEFINE INPUT PARAMETER iSequenciaLinha  AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER cDescricaoColuna AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER cConteudoCampo   AS CHAR NO-UNDO.

    DEFINE VARIABLE cCodigoColuna AS CHARACTER   NO-UNDO.
    
    RUN piRetornaCodigoColuna (INPUT  cDescricaoColuna,
                               OUTPUT cCodigoColuna).
    
    IF  cCodigoColuna <> "" THEN DO:
        CREATE ttLinha.
        ASSIGN ttLinha.sequenciaLinha = iSequenciaLinha
               ttLinha.codigoColuna   = cCodigoColuna
               ttLinha.conteudoCampo  = TRIM(cConteudoCampo).
    END.

END PROCEDURE.

PROCEDURE piCriaLinhaTracejada:
    DEFINE INPUT PARAMETER iSequenciaLinha AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE cCaracterLinha AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dTamanhoChar   AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE cConteudoCampo AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dTamanhoAtual  AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dTamanhoColuna AS DECIMAL     NO-UNDO.

    ASSIGN cCaracterLinha = "-":U.

    FIND FIRST ttCaracteres
         WHERE ttCaracteres.codigoAsc = ASC(cCaracterLinha) NO-ERROR.

    IF  NOT AVAIL ttCaracteres      OR
        ttCaracteres.tamanhoChar <= 0 THEN
        ASSIGN dTamanhoChar = 1.
    ELSE
        ASSIGN dTamanhoChar = ttCaracteres.tamanhoChar.
    
    FOR EACH ttColunasDANFE
          BY ttColunasDANFE.codigo:
        
        ASSIGN cConteudoCampo = ""
               dTamanhoAtual  = 0
               dTamanhoColuna = IF  cModeloDANFE = '3' /* Retrato */
                                THEN ttColunasDANFE.tamanhoRetrato
                                ELSE ttColunasDANFE.tamanhoPaisagem.

        REPEAT:
            ASSIGN dTamanhoAtual  = dTamanhoAtual + dTamanhoChar
                   cConteudoCampo = cConteudoCampo + cCaracterLinha.

            IF  dTamanhoAtual > dTamanhoColuna THEN
                LEAVE.
        END.

        RUN piCriaTtLinha (INPUT iSequenciaLinha,
                           INPUT ttColunasDANFE.descricao,
                           INPUT cConteudoCampo).
       
    END.

END PROCEDURE.

PROCEDURE piControlePaginaAtual:
    DEFINE VARIABLE iLinhasRestantesPaginaAtual AS INTEGER     NO-UNDO.

    IF  NOT lEmiteTracejado AND
        iCont > 1           THEN
        ASSIGN lEmiteTracejado = YES.

    IF  lPrimeiraPagina THEN DO:

        IF  iLinhaPaginaAtual = iNumLinhasProdPrimeiraPagina THEN
            ASSIGN lUltimaLinhaPagina = YES.
        ELSE IF  iLinhaPaginaAtual > iNumLinhasProdPrimeiraPagina THEN
            ASSIGN iLinhaPaginaAtual  = 1
                   lPrimeiraPagina    = NO
                   lUltimaLinhaPagina = NO.

    END.
    ELSE DO:
         
        IF  iLinhaPaginaAtual = iNumLinhasProdOutrasPaginas THEN
            ASSIGN lUltimaLinhaPagina = YES.
        ELSE IF  iLinhaPaginaAtual > iNumLinhasProdOutrasPaginas THEN
            ASSIGN iLinhaPaginaAtual  = 1
                   lUltimaLinhaPagina = NO.

    END.

    IF  iCont = 1 THEN DO:

        IF lPrimeiraPagina THEN
            ASSIGN iLinhasRestantesPaginaAtual = iNumLinhasProdPrimeiraPagina - iLinhaPaginaAtual + 1.
        ELSE
            ASSIGN iLinhasRestantesPaginaAtual = iNumLinhasProdOutrasPaginas  - iLinhaPaginaAtual + 1.
        
        IF  iNumLinhasOcupadasProduto >  (iLinhasRestantesPaginaAtual - (IF lEmiteTracejado THEN 1 ELSE 0)) THEN DO:
        
            ASSIGN iLinhaPaginaAtual  = 1
                   lPrimeiraPagina    = NO
                   lUltimaLinhaPagina = NO.

            ASSIGN iSeqTtLinha = iSeqTtLinha + iLinhasRestantesPaginaAtual.

        END.
    END.    

END PROCEDURE.
PROCEDURE pi-EnviaEmail:

    DEFINE VARIABLE c-assunto       AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE c-destino-email AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE c-email-empresa AS CHARACTER   NO-UNDO.

    def var c-subject        as char format "X(255)"    no-undo.    
    def var c-body-mail      as char format "X(255)"    no-undo.

    EMPTY TEMP-TABLE tt-envio2.
    EMPTY TEMP-TABLE tt-mensagem.

    ASSIGN c-destino-email = "".

    FOR FIRST param_email FIELDS(cod_servid_e_mail  num_porta log_servid_exchange) NO-LOCK: END.

    FIND empresa NO-LOCK
        WHERE empresa.ep-codigo = STRING(i-ep-codigo-usuario) NO-ERROR.


    IF pCodEstabel >= "301" AND pCodEstabel <= "309" THEN
        ASSIGN c-email-empresa = "nfe@logikacosmeticos.com.br"
               c-assunto = "DANFE/XML" + " - " + "LOGIKA".
    ELSE
        ASSIGN c-email-empresa = "nfe@bonyplus.com.br"
               c-assunto = "DANFE/XML" + " - " + "BONYPLUS".

    FIND emitente NO-LOCK
        WHERE emitente.cgc = replace(replace(replace(ttDanfe.cnpjdestinatario,".",""),"/",""),"-","") NO-ERROR.

    FOR EACH cont-emit NO-LOCK
        WHERE cont-emit.cod-emitente = emitente.cod-emitente :

        IF c-destino-email = "" THEN
            ASSIGN c-destino-email = cont-emit.e-mail.
        ELSE
            ASSIGN c-destino-email = c-destino-email + ";" + cont-emit.e-mail.
         
    END.

/*     ASSIGN c-destino-email = "jdchaves@live.com". */

    
    RUN pi-ConvertDocToPdf (INPUT c-arquivo-danfe, OUTPUT c-arquivo-pdf).

/*     MESSAGE "Novo Teste.....:" TODAY SKIP           */
/*             "Destino Email..:" c-destino-email SKIP */
/*             "Email Empresa..:" c-email-empresa SKIP */
/*             "Arquivo Pdf....:" c-arquivo-pdf SKIP   */
/*             "Arquivo xml....:" cArquivoXML          */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.          */

    

    IF  c-destino-email <> "":U THEN DO:

        CREATE tt-envio2.
        ASSIGN tt-envio2.versao-integracao = 1
               tt-envio2.exchange          = no
               tt-envio2.remetente         = c-email-empresa
               tt-envio2.destino           = c-destino-email  
               tt-envio2.copia             = ""
               tt-envio2.assunto           = c-assunto
               tt-envio2.importancia       = 2
               tt-envio2.formato           = "Texto"
               tt-envio2.arq-anexo         = c-arquivo-pdf + "," + cArquivoXML.

        ASSIGN c-body-mail = 'Para conhecimento, segue em anexo DANFE e XML emitido em ' + STRING(TODAY,"99/99/9999").

        CREATE tt-mensagem.
        ASSIGN tt-mensagem.seq-mensagem = 1
               tt-mensagem.mensagem     = c-body-mail.

        RUN pi-blat(INPUT TABLE tt-envio2).

        
        /*FIND FIRST tt-envio NO-ERROR.
        IF NOT AVAIL tt-envio THEN 
            CREATE tt-envio.
        
        ASSIGN tt-envio.versao-integracao = 1
               tt-envio.servidor    = param_email.cod_servid_e_mail  
               tt-envio.porta       = param_email.num_porta
               tt-envio.destino     = c-destino-email 
               tt-envio.remetente   = c-email-empresa 
               tt-envio.assunto     = c-assunto
               tt-envio.mensagem    = "Segue em anexo DANFE/XML conforme nota fiscal faturada!" 
               tt-envio.arq-anexo   = c-arquivo-danfe + "," + cArquivoXML  
               tt-envio.exchange    = param_email.log_servid_exchange.
    
        RUN utp/utapi009.p (INPUT  TABLE tt-envio,
                            OUTPUT TABLE tt-erros).     

        IF RETURN-VALUE = "NOK" THEN DO:
            OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "erromail.txt").             

            FOR EACH tt-erros:
                DISP tt-erros.cod-erro tt-erros.desc-erro WITH FRAME f-print NO-LABEL.
            END.

            OUTPUT CLOSE.    

            RUN utp/ut-msgs.p (INPUT "show",
                               INPUT 25694,
                               INPUT "").
        END.
        ELSE DO:
            RUN utp/ut-msgs.p (INPUT "show",
                               INPUT 34636,
                               INPUT "E-mail com XML/DANFE enviado /").
        END.*/
    END.

END PROCEDURE.

procedure pi-blat:
     DEFINE INPUT PARAM TABLE FOR tt-envio2.
    
     DEF VAR c-arquivo AS CHAR NO-UNDO.
     DEF VAR c-bat     AS CHAR NO-UNDO.
     DEFINE VARIABLE v-cod-lin-comando AS CHARACTER NO-UNDO.
    
     FIND FIRST tt-envio2 NO-ERROR.
     IF NOT AVAIL tt-envio2 THEN 
         RETURN "NOK".

     FOR FIRST param_email FIELDS(cod_servid_e_mail  num_porta log_servid_exchange) NO-LOCK: END.

     ASSIGN c-arquivo = "dtsemail" + STRING(ETIME) + ".txt"
            c-bat     = "dtsemail" + STRING(ETIME) + ".bat".

     ASSIGN tt-envio2.destino = REPLACE(tt-envio2.destino,CHR(10),",")       
            tt-envio2.copia = REPLACE(tt-envio2.copia,CHR(10),",")
            tt-envio2.destino = REPLACE(tt-envio2.destino,";",",")       
            tt-envio2.copia = REPLACE(tt-envio2.copia,";",",").

     ASSIGN tt-envio2.assunto = REPLACE(tt-envio2.assunto,"%","%%").
     
     IF tt-envio2.assunto = "" OR tt-envio2.assunto = ? THEN DO:
         {utp/ut-liter.i Conte£do_do_assunto * R}
         RUN utp/ut-msgs.p (INPUT "msg", INPUT 54, INPUT RETURN-VALUE).
         RUN pi-criar-erros (INPUT 54, INPUT RETURN-VALUE, INPUT "").
         RETURN "NOK". 
     END.                      
            
    OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + c-arquivo) CONVERT TARGET "iso8859-1". /* tech1139 - FO 1334.236 - 11/07/2006  */
    FOR EACH tt-mensagem USE-INDEX i-seq-mensagem:
        PUT UNFORMATTED REPLACE(tt-mensagem.mensagem,CHR(13),CHR(10)).
    END.
    OUTPUT CLOSE.
    FILE-INFO:FILE-NAME = SEARCH("interfac/mail/blat.exe").
    ASSIGN v-cod-lin-comando = "@" + "~"" + FILE-INFO:FULL-PATHNAME + "~""
                               + " "
                               + "~"" + SESSION:TEMP-DIRECTORY + c-arquivo + "~"" /* tech1139 - FO 1334.236 - 11/07/2006  */
                               + " -s" 
                               + ' "' 
                               + tt-envio2.assunto 
                               + '"' 
                               + " -f "
                               + tt-envio2.remetente
                               + " -t " 
                               + tt-envio2.destino 
                               + " -server " 
                               + "~"" + param_email.cod_servid_e_mail + "~""
                               + " -port " + "26" 
                               + " -mime"
                               + " -q"
                               + " -noh"
                               + " -noh2"
                               + " -debug -u bonyplus\nfe -pw a123456*".

    IF tt-envio2.copia <> "" THEN  
        ASSIGN v-cod-lin-comando = v-cod-lin-comando + " -c " + tt-envio2.copia.
    IF tt-envio2.formato <> "TEXTO" AND tt-envio2.formato <> "" THEN DO:
        IF tt-envio2.formato = "ENRICHED" THEN  
            ASSIGN v-cod-lin-comando = v-cod-lin-comando + " -enriched ".
        IF tt-envio2.formato = "HTML" THEN  
            ASSIGN v-cod-lin-comando = v-cod-lin-comando + " -html ".    	
    END.    
    DEFINE VARIABLE i-cont AS INTEGER     NO-UNDO.
/*     MESSAGE "tt-envio2.arq-anexo" tt-envio2.arq-anexo VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    IF tt-envio2.arq-anexo <> "" THEN DO:
        DO i-cont = 1 TO NUM-ENTRIES(tt-envio2.arq-anexo):
            IF OPSYS = "UNIX" THEN DO:
                ASSIGN tt-envio2.arq-anexo = REPLACE(tt-envio2.arq-anexo, "~\~\", "/").
            END.
            ELSE DO:
                ASSIGN tt-envio2.arq-anexo = REPLACE(tt-envio2.arq-anexo, "/", CHR(92)).
            END.
            ASSIGN v-cod-lin-comando = v-cod-lin-comando + " -attach ~"" + ENTRY(i-cont,tt-envio2.arq-anexo) + "~"".
        END.   
    END.

    OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + c-bat) CONVERT TARGET "IBM850". /* tech1139 - FO 1334.236 - 11/07/2006  */
    PUT UNFORMATTED v-cod-lin-comando.
    OUTPUT CLOSE.

    DOS SILENT VALUE("~"" + SESSION:TEMP-DIRECTORY + c-bat + "~""). 

    /*OS-DELETE VALUE(SESSION:TEMP-DIRECTORY + c-arquivo).
    OS-DELETE VALUE(SESSION:TEMP-DIRECTORY + c-bat). 
    MESSAGE SESSION:TEMP-DIRECTORY VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

    RETURN "OK".

END. /*procedure pi-blat*/


/*converte .doc em .pdf */
PROCEDURE pi-ConvertDocToPdf:
    DEFINE  INPUT  PARAMETER pDirDoc      AS CHARACTER NO-UNDO.
    DEFINE  OUTPUT PARAMETER pDirPdf      AS CHARACTER NO-UNDO.


    DEFINE VARIABLE chWord  AS com-handle   NO-UNDO.
    DEFINE VARIABLE cArqAux AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cArqPdf AS CHARACTER   NO-UNDO.

    CREATE 'Word.Application':U chWord.    
    chWord:WindowState = 2.    
    chWord:VISIBLE = NO.    
    chWord:Documents:ADD(pDirDoc).

    cArqAux = pDirDoc.
    OS-DELETE VALUE(pDirDoc) .


    cArqPdf = replace(cArqAux,".doc",".pdf").
    pDirPdf = cArqPdf.
    chWord:ActiveDocument:SaveAs(cArqAux).     
    chWord:ActiveDocument:ExportAsFixedFormat(cArqPdf,17,FALSE,false,,,,,,,,,,).
    chWord:ActiveDocument:CLOSE.    
    chWord:QUIT().    
    RELEASE OBJECT chWord.        
    os-delete cArqAux.
END PROCEDURE.

