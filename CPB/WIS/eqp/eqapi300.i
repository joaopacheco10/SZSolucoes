def temp-table tt-embarque no-undo like embarque use-index ch-emb
    field i-sequen as int
    field ind-oper as int. /* 1 - Inclus∆o
                              2 - Alteraá∆o
                              3 - Eliminaá∆o */

def temp-table tt-ped-venda no-undo
    field i-sequen     as int
    field cdd-embarq   like embarque.cdd-embarq
    field nome-abrev   like ped-venda.nome-abrev
    field nr-pedcli    like ped-venda.nr-pedcli
    field ind-oper     as int /* 1 - Alocar
                                2 - Desalocar */

    index ch-pedido is primary
        nome-abrev
        nr-pedcli.

def temp-table tt-ped-ent no-undo
    field nome-abrev   like ped-ent.nome-abrev
    field nr-pedcli    like ped-ent.nr-pedcli
    field nr-sequencia like ped-ent.nr-sequencia
    field it-codigo    like ped-ent.it-codigo
    field cod-refer    like ped-ent.cod-refer
    field qt-alocada   like ped-ent.qt-alocada
    field qt-log-aloca like ped-ent.qt-log-aloca
    field qt-pedida    like ped-ent.qt-pedida 
    field qt-atendida  like ped-ent.qt-atendida
    field dt-entrega   like ped-ent.dt-entrega
    field nr-entrega   like ped-ent.nr-entrega 
    field hr-entrega   like ped-ent.hr-entrega
    
  &if defined(bf_dis_proc_ent) &then 
    field nr-proc-exp  like ped-ent.nr-proc-exp
  &endif

    &if defined(bf_dis_desc_bonif) &then
        field num-sequencia-bonif like ped-item.num-sequencia-bonif
    &endif    
    
    field nat-operacao    like ped-item.nat-operacao
    
    field dec-1           like ped-ent.dec-1
    field qt-a-alocar     as decimal
    field i-sequen        as int
    field cdd-embarq      as DEC
    field nr-seq-registro as integer init 1
    
    index ch-ent is primary
    &if defined(bf_comex206b-1) &then
        nr-proc-exp
    &endif
        nome-abrev
        nr-pedcli
        nr-sequencia
        it-codigo
        cod-refer         
        nr-entrega.
        
def temp-table tt-it-pre-fat no-undo
    field cdd-embarq   as dec
    field nr-resumo    as int
    field nome-abrev   as char
    field nr-pedcli    as char
    field nr-sequencia as int
    field it-codigo    as char
    field qt-a-alocar  as deci
    field i-sequen     as int
    field nr-entrega   as int
    index ch-it-pre-fat is primary
        cdd-embarq
        nr-resumo
        nome-abrev
        nr-pedcli
        nr-sequencia
        it-codigo
        nr-entrega.
        
def temp-table tt-aloc-man no-undo
    field cdd-embarq   as dec
    field nr-resumo    as int
    field nome-abrev   as char
    field nr-pedcli    as char
    field nr-sequencia as int
    field it-codigo    as char
    field cod-estabel  as char
    field cod-depos    as char
    field cod-localiz  as char
    field lote         as char
    field cod-refer    as char
    field qt-a-alocar  as deci
    field i-sequen     as int
    field nr-entrega   as int.

def temp-table tt-deposito no-undo
    field sequen as int
    field cod-depos like deposito.cod-depos
    index ch-aeq-dep is primary unique
        sequen.
    
def temp-table tt-it-narrativa no-undo
    field nome-abrev   as char
    field nr-pedcli    as char
    field nr-sequencia as int
    field it-codigo    as char
    field cod-refer    as char
    field nr-entrega   as int
    field cdd-embarq   as DEC
    field nr-resumo    as int
    field narrativa    as char.

def temp-table tt-item-filho no-undo
    field nome-abrev   as char
    field nr-pedcli    as char
    field nr-sequencia as int
    field it-codigo    as char
    field cod-refer    as char
    field nr-entrega   as int
    field qt-a-alocar  as deci
    index ch-entrega is primary
        nome-abrev
        nr-pedcli
        nr-sequencia
        it-codigo
        cod-refer
        nr-entrega.

def temp-table tt-aloc-comp-man no-undo
    field cdd-embarq   as dec
    field nr-resumo    as int
    field nome-abrev   as char
    field nr-pedcli    as char
    field nr-sequencia as int
    field it-codigo    as char
    field cod-estabel  as char
    field cod-depos    as char
    field cod-localiz  as char
    field lote         as char
    field cod-refer    as char
    field qt-a-alocar  as deci
    field i-sequen     as int
    field nr-entrega   as int.

def temp-table tt-aloc-neg no-undo
    field cod-depos   as char
    field cod-localiz as char.

DEF TEMP-TABLE tt-deposito-local NO-UNDO
    FIELD sequen     AS INT 
    FIELD cod-depos  LIKE deposito.cod-depos
    FIELD dep-wms    AS LOGICAL
    FIELD it-codigo  LIKE ITEM.it-codigo
    INDEX ch-seq-dep IS PRIMARY sequen.

/* temp-table auxiliar criada para n∆o provocar erro 3241*/
DEF TEMP-TABLE tt-manut-alocacao NO-UNDO LIKE tt-aloc-man.

/* temp table usada em cotas, para o AC1002 */
DEF TEMP-TABLE tt-ped-item-cotas NO-UNDO 
    FIELD nr-sequencia AS INT 
    FIELD it-codigo    AS CHAR
    FIELD cod-refer    AS CHAR.

def temp-table tt-depos-wms no-undo
    field cod-depos as char
    field log-wms   as log.

DEFINE TEMP-TABLE tt-estabelec NO-UNDO
   FIELD cod-estabel LIKE estabelec.cod-estabel
   FIELD est-padrao  AS INTEGER  /* 0 para estabelecimento do pedido/embarque */
   index codigo      is primary est-padrao.

DEFINE TEMP-TABLE tt-integra NO-UNDO
    FIELD r-pedido AS ROWID.
