/**************************************************************************
**   Programa: esp/eswms016.p
**   Data    : junho/2016
**   Autor   : joao pacheco
**   Objetivo: Integracao FIS Transportadoras
**   Versao..: 
**************************************************************************/

{utp/ut-glob.i}

def new global shared var g-integrator as log no-undo.

define param buffer transporte for transporte.

def temp-table ti_e_transportadora no-undo
    field cd_empresa        as int
    field cd_transportadora as int
    field ds_transportadora as char
    field ds_nome_fantasia  as char /*n*/
    field nu_cgc            as char
    field nu_ie             as char
    field ds_endereco       as char
    field numero            as char
    field complemento       as char /*n*/
    field ds_bairro         as char
    field ds_cidade         as char
    field cd_municipio      as char /*n*/
    field sg_uf             as char
    field nu_telefone_1     as char /*n*/
    field nu_telefone_2     as char /*n*/
    field nm_contato        as char /*n*/
    field dt_addrow         as date
    field id_processado     as char initial 'n'.
    

def var c-sql      as char        no-undo.
def var h-api      as handle      no-undo.
def var c-rua      as character   no-undo.
def var c-nro      as character   no-undo.
def var c-comp     as character   no-undo.
def var h-cdapi704 as handle      no-undo.


if not g-integrator then do:
    run tools/queuemanager.p ("Transportadorafis",
                              string(transporte.cod-transp)).        
    return "ok".
end.

function fn_only_numbers returns character
    (input cvaluestring as character):

    define variable ipos     as integer   no-undo.
    define variable ilength  as integer   no-undo.
    define variable cdata    as character no-undo.
    define variable cdatanew as character no-undo.


    assign ilength = length(cvaluestring).

    do ipos = 1 to ilength:
        assign cdata = substring(cvaluestring,ipos,1).
        
        if cdata < '0' or cdata > '9' then
            next.

        assign cdatanew = cdatanew + cdata.
    end.

    return cdatanew.
end function.

/*metodo que busca o logradouro e o numero*/
if not valid-handle(h-cdapi704) then
    run cdp/cdapi704.p persistent set h-cdapi704.

run pi-trata-endereco in h-cdapi704 (input transporte.endereco, 
                                     output c-rua, 
                                     output c-nro, 
                                     output c-comp).

create ti_e_transportadora.
assign ti_e_transportadora.cd_empresa        = if int(i-ep-codigo-usuario) <> 0 then int(i-ep-codigo-usuario) else 1
       ti_e_transportadora.cd_transportadora = transporte.cod-transp
       ti_e_transportadora.ds_transportadora = if transporte.nome <> "" then substring(transporte.nome,1,100) else "nao cadastrado"
       ti_e_transportadora.nu_cgc            = string(fn_only_numbers(transporte.cgc))
       ti_e_transportadora.nu_ie             = string(fn_only_numbers(transporte.ins-estadual))
       ti_e_transportadora.ds_endereco       = c-rua
       ti_e_transportadora.numero            = c-nro
       ti_e_transportadora.complemento       = c-comp
       ti_e_transportadora.ds_bairro         = transporte.bairro
       ti_e_transportadora.ds_cidade         = transporte.cidade
       ti_e_transportadora.cd_municipio      = ''
       ti_e_transportadora.sg_uf             = transporte.estado
       ti_e_transportadora.nu_telefone_1     = string(fn_only_numbers(transporte.telefone))
       ti_e_transportadora.nu_telefone_2     = ''
       ti_e_transportadora.nm_contato        = transporte.contato
       ti_e_transportadora.dt_addrow         = today
       ti_e_transportadora.id_processado     = 'N'.

run esp/eswmapi998.p persistent set h-api.

run pi_insert in h-api (temp-table ti_e_transportadora:default-buffer-handle).

if return-value <> "ok" then do:
    for each esp-fila-wms exclusive-lock where 
             esp-fila-wms.cod-trans = "Transportadorafis" and 
             esp-fila-wms.chave     = string(transporte.cod-transp):
         
         create esp-erro-wms.
         assign esp-erro-wms.id             = esp-fila-wms.id
                esp-erro-wms.data-hora-erro = now
                esp-erro-wms.mensagem       = return-value.
         
    end.
end.
else
    for each esp-fila-wms exclusive-lock where 
             esp-fila-wms.cod-trans = "Transportadorafis" and 
             esp-fila-wms.chave     = string(transporte.cod-transp):
         if esp-fila-wms.data-hora-integracao = ? then
             assign esp-fila-wms.data-hora-integracao = now.
    end.

run pi_finalizar in h-api.

delete procedure h-api.
delete procedure h-cdapi704.
