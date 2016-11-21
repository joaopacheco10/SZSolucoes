/**************************************************************************
**
**   CEAPI001K.i   - INCLUDE DE DEFINICAO DA TEMP-TABLE TT-MOVTO
**
**************************************************************************/

{cdp/cdcfgmat.i}

DEF TEMP-TABLE tt-movto NO-UNDO
    FIELD cod-versao-integracao AS INTEGER FORMAT "999"
    FIELD cod-prog-orig         LIKE movto-estoq.cod-prog-orig
    FIELD l-mov-erro            AS LOGICAL INITIAL NO
    FIELD r-mov-inv             AS ROWID    
    FIELD r-mov-orig            AS ROWID /*registro original para valorizar o estorno, devolu‡Æo,retorno*/
    FIELD sequen-nf             LIKE movto-estoq.sequen-nf
    FIELD cod-depos             LIKE movto-estoq.cod-depos
    FIELD cod-emitente          LIKE movto-estoq.cod-emitente
    FIELD cod-estabel           LIKE movto-estoq.cod-estabel
    FIELD cod-refer             LIKE movto-estoq.cod-refer
    FIELD ct-codigo             LIKE movto-estoq.ct-codigo
    FIELD descricao-db          LIKE movto-estoq.descricao-db
    FIELD dt-nf-saida           LIKE movto-estoq.dt-nf-saida
    FIELD dt-trans              LIKE movto-estoq.dt-trans
    FIELD esp-docto             LIKE movto-estoq.esp-docto
    FIELD it-codigo             LIKE movto-estoq.it-codigo
    FIELD cod-localiz           LIKE movto-estoq.cod-localiz
    FIELD lote                  LIKE movto-estoq.lote
    FIELD nat-operacao          LIKE movto-estoq.nat-operacao
    FIELD nro-docto             LIKE movto-estoq.nro-docto
    FIELD num-sequen            LIKE movto-estoq.num-sequen
    FIELD numero-ordem          LIKE movto-estoq.numero-ordem
    FIELD nr-ord-produ          LIKE movto-estoq.nr-ord-produ
    FIELD peso-liquido          LIKE movto-estoq.peso-liquido
    FIELD quantidade            LIKE movto-estoq.quantidade
    FIELD referencia            LIKE movto-estoq.referencia
    FIELD sc-codigo             LIKE movto-estoq.sc-codigo
    FIELD serie-docto           LIKE movto-estoq.serie-docto
    FIELD tipo-preco            LIKE movto-estoq.tipo-preco
    FIELD tipo-trans            LIKE movto-estoq.tipo-trans
    FIELD tipo-valor            LIKE movto-estoq.tipo-valor
    FIELD un                    LIKE movto-estoq.un         
    FIELD valor-mat-m           LIKE movto-estoq.valor-mat-m
    FIELD valor-mat-o           LIKE movto-estoq.valor-mat-o
    FIELD valor-mat-p           LIKE movto-estoq.valor-mat-p
    FIELD valor-mob-m           LIKE movto-estoq.valor-mob-m
    FIELD valor-mob-o           LIKE movto-estoq.valor-mob-o
    FIELD valor-mob-p           LIKE movto-estoq.valor-mob-p
    FIELD valor-ggf-m           LIKE movto-estoq.valor-ggf-m
    FIELD valor-ggf-o           LIKE movto-estoq.valor-ggf-o
    FIELD valor-ggf-p           LIKE movto-estoq.valor-ggf-p
    FIELD valor-nota            LIKE movto-estoq.valor-nota
    FIELD vl-nota-fasb          LIKE movto-estoq.vl-nota-fasb
    FIELD nr-ord-refer          LIKE movto-estoq.nr-ord-refer
    FIELD nr-req-sum            LIKE movto-estoq.nr-req-sum
    FIELD cod-roteiro           LIKE movto-estoq.cod-roteiro
    FIELD nr-reporte            LIKE movto-estoq.nr-reporte
    FIELD item-pai              LIKE movto-estoq.item-pai
    FIELD op-codigo             LIKE movto-estoq.op-codigo
    FIELD cod-usu-ult-alter     LIKE movto-estoq.cod-usu-ult-alter
    FIELD ct-db                 LIKE movto-estoq.ct-codigo
    FIELD sc-db                 LIKE movto-estoq.sc-codigo
    FIELD dt-vali-lote          LIKE saldo-estoq.dt-vali-lote
    FIELD op-seq                LIKE movto-estoq.op-seq
    FIELD usuario               LIKE movto-estoq.usuario
    FIELD nr-trans              LIKE movto-estoq.nr-trans 
    FIELD cod-estabel-des       LIKE movto-estoq.cod-estabel-des
    FIELD origem-valor          LIKE movto-estoq.origem-valor
    FIELD num-ord-des           LIKE movto-estoq.num-ord-des
    FIELD num-seq-des           LIKE movto-estoq.num-seq-des
    FIELD num-ord-inv           LIKE movto-estoq.num-ord-inv
    FIELD valor-ipi             LIKE movto-estoq.valor-ipi
    FIELD valor-iss             LIKE movto-estoq.valor-iss
    FIELD valor-icm             LIKE movto-estoq.valor-icm
    FIELD vl-icm-fasb           LIKE movto-estoq.vl-icm-fasb
    FIELD vl-iss-fasb           LIKE movto-estoq.vl-iss-fasb
    FIELD vl-ipi-fasb           LIKE movto-estoq.vl-ipi-fasb 
    FIELD per-ppm               LIKE movto-estoq.per-ppm
    FIELD atualiza-ul-ent       AS LOGICAL
    FIELD i-sequen              AS INTEGER
    FIELD gera-saldo            AS LOGICAL INIT NO
    FIELD qt-alocada            AS DECIMAL
    FIELD cod-unid-negoc        LIKE movto-estoq.cod-unid-negoc
    FIELD cod-unid-negoc-db     LIKE movto-estoq.cod-unid-negoc
    FIELD cod-unid-negoc-sdo    LIKE movto-estoq.cod-unid-negoc
  &if '{&bf_lote_avancado_liberado}' = 'yes' &then
    field i-sequen-pai             as integer
    field dat-valid-lote-fabrican  like movto-estoq.dat-valid-lote-fabrican
    field dat-fabricc-lote         like movto-estoq.dat-fabricc-lote
    field nom-fabrican             like movto-estoq.nom-fabrican
    field cod-lote-fabrican        like movto-estoq.cod-lote-fabrican
    field log-ficha                as log
  &endif
  .

/* FIM INCLUDE CEAPI001K.i */
