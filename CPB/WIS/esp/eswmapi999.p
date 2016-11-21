/**************************************************************************
**   Programa: esp/eswmapi999.p
**   Data    : Dezembro de 2009
**   Autor   : Eder
**   Objetivo: Gerenciamento da conexÆo ODBC
**   Versao..: 
**************************************************************************/

def buffer prog_dtsul   for emsfnd.prog_dtsul.
def buffer usuar_mestre for emsfnd.usuar_mestre.

{include/i-prgvrs.i eswmapi999.p 2.06.00.000}

/* Sample Code */
define variable ObjRecordSet   as com-handle no-undo.
define variable ObjConnection  as com-handle no-undo.
define variable ObjCommand     as com-handle no-undo.
define variable ODBC-SQL-STMT  as character  no-undo.
define variable ODBC-STATUS    as character  no-undo.
define variable ODBC-RECCOUNT  as integer    no-undo.
define variable ODBC-NULL      as character  no-undo.
define variable ODBC-CURSOR    as integer    no-undo.
define variable ODBC-CONNECTED as logical    no-undo.

find first esp-param-integr-wms no-lock no-error.

if not avail esp-param-integr-wms then
    return error.
    
   
create "ADODB.Connection" ObjConnection.    
create "ADODB.RecordSet"  ObjRecordSet.      
create "ADODB.Command"    ObjCommand. 

ObjConnection:open ( "data source=" + ODBC-DSN + ";server=" + ODBC-SERVER, ODBC-USERID, ODBC-PASSWD, 0 ) . 

if ( error-status:num-messages > 0 ) then
    ODBC-STATUS = "Erro: NÆo foi poss¡vel conectar ODBC." + error-status:get-message(1).
else
    assign ODBC-CONNECTED = yes
           ObjCommand:ActiveConnection  = ObjConnection
           ObjCommand:CommandType       = 1. /* adCmdText */ 
          
            
procedure isConnected:
    def output param l-connected as logical no-undo.

    assign l-connected = ODBC-CONNECTED.
end.

procedure pi_select:
    def input param h-bf as handle no-undo.

    def var i as integer no-undo.

    assign ODBC-SQL-STMT = "SELECT ".

    do i = 1 to h-bf:num-fields:
        assign ODBC-SQL-STMT =  ODBC-SQL-STMT + h-bf:buffer-field(i):name.
    
        if i < h-bf:num-fields then
            assign ODBC-SQL-STMT =  ODBC-SQL-STMT + ",".
    end.

    assign ODBC-SQL-STMT = ODBC-SQL-STMT + " FROM " + h-bf:name.
     
    if h-bf:name <> "V_ESTOQUE_WMS" then
        assign ODBC-SQL-STMT = ODBC-SQL-STMT  + " WHERE (ID_PROCESSADO <> 'S' OR ID_PROCESSADO IS NULL)".
       
    if h-bf:name = "INT_S_CAB_PEDIDO_SAIDA" then
        assign ODBC-SQL-STMT = ODBC-SQL-STMT + " AND CD_SITUACAO = 57 AND DT_ENTREGA >= '" + STRING(day(today - 15),'99') + '/' + string(month(today - 15), '99') + '/' + substring(string(year(today - 15)),3,2) + "'".

    /*if h-bf:name = "INT_S_DET_PEDIDO_SAIDA" then
        assign ODBC-SQL-STMT = ODBC-SQL-STMT + " AND DT_ENTREGA = '" + STRING(day(today - 15),'99') + '/' + string(month(today - 15), '99') + '/' + substring(string(year(today - 15)),3,2) + "'".*/
       

    PUT UNFORMATTED ODBC-SQL-STMT SKIP.

    assign ObjCommand:CommandText       = ODBC-SQL-STMT
           ObjConnection:CursorLocation = 3 /* adUseClient */
           ObjRecordSet:CursorType      = 3 /* adOpenStatic */
           ObjRecordSet = ObjCommand:execute ( output ODBC-NULL, "", 32 )
           ODBC-RECCOUNT = ObjRecordSet:RecordCount.

    if ( ODBC-RECCOUNT > 0 ) and not ( ODBC-RECCOUNT = ? ) then do:
        
        ObjRecordSet:MoveFirst no-error.
        
        do while ODBC-CURSOR < ODBC-RECCOUNT:
            h-bf:buffer-create().
            do i = 1 to h-bf:num-fields:
                h-bf:buffer-field(i):buffer-value = ObjRecordSet:fields (h-bf:buffer-field(i):name):value.
            end.

            assign ODBC-CURSOR = ODBC-CURSOR + 1.
            ObjRecordSet:MoveNext no-error.
        end. 
    end. 
    else
        assign ODBC-STATUS = "No records found.".
        
    ObjRecordSet:close().        
end. 

procedure pi_insert :
    def input param h-bf as handle no-undo.

    def var c-valor as char no-undo.
    def var c-ano   as char no-undo.
    def var c-mes as char no-undo.
    def var c-dia as char no-undo.

    def var i as integer no-undo.
       
    assign ODBC-SQL-STMT = "INSERT INTO " + h-bf:name + " (".
    
    do i = 1 to h-bf:num-fields:
        assign ODBC-SQL-STMT =  ODBC-SQL-STMT + h-bf:buffer-field(i):name.
    
        if i < h-bf:num-fields then
            assign ODBC-SQL-STMT =  ODBC-SQL-STMT + ",".
        else
            assign ODBC-SQL-STMT =  ODBC-SQL-STMT + ")".    
        
    end.
    
    assign ODBC-SQL-STMT = ODBC-SQL-STMT + " VALUES (".
     
    do i = 1 to h-bf:num-fields:
        assign c-valor = h-bf:buffer-field(i):buffer-value.
        
        if h-bf:buffer-field(i):data-type = "character" then
            assign c-valor = replace(c-valor,"~'"," ")
                   c-valor = replace(c-valor,"~""," ")
                   c-valor = replace(c-valor,"~&"," ")
                   c-valor = replace(c-valor,"~,"," ").     
        else if h-bf:buffer-field(i):data-type = "decimal" then do:
            assign c-valor = replace(c-valor,".",",").  
            if substring(c-valor,1,1) = "," then
                assign c-valor = "0" + c-valor.
        end.   
                
        if h-bf:buffer-field(i):data-type = "date" then do:
            assign c-valor = string(h-bf:buffer-field(i):buffer-value,"99/99/9999")
                   c-ano = substring(c-valor,7,4)
                   c-mes = substring(c-valor,4,2)
                   c-dia = substring(c-valor,1,2)
                   c-valor = c-ano + "-" + c-mes + "-" + c-dia + " " + string(time, "HH:MM")
                   c-valor = "TO_DATE(" + "~'" + c-valor + "~' , " + "~'" + "yyyy-mm-dd hh24:mi" + "~')"
                   ODBC-SQL-STMT =  ODBC-SQL-STMT + c-valor.            
        end.
        else
            assign ODBC-SQL-STMT =  ODBC-SQL-STMT + "~'" + c-valor + "~'".
             
        if i < h-bf:num-fields then
            assign ODBC-SQL-STMT =  ODBC-SQL-STMT + ",".
        else
            assign ODBC-SQL-STMT =  ODBC-SQL-STMT + ");". 
            
    end.
           
    put unformatted
        '- ANTES - ' string(time, "HH:MM:SS") skip.       
            
    put unformat ODBC-SQL-STMT skip.   
    
    /*assign ODBC-SQL-STMT = "INSERT INTO INT_E_CODIGO_BARRAS (CD_EMPRESA,CD_PRODUTO,CD_BARRAS,TP_CODIGO_BARRAS,QT_EMBALAGEM,ID_CODIGO_PRINCIPAL,ID_PROCESSADO,DT_ADDROW)
            VALUES ('1','123','123456788','1','20','N','S',TO_DATE('2015-03-25 00:00'  , 'yyyy-mm-dd hh24:mi'));". */
                 
    
    ObjCommand:CommandText = ODBC-SQL-STMT.
    ObjCommand:execute ( output ODBC-NULL, "", 32 ) no-error.
    
    put unformatted
        '- DEPOIS - ' string(time, "HH:MM:SS") skip. 
    
    if error-status:num-messages > 0 THEN DO:
        
        put UNFORMAT error-status:get-message(1) skip.
        return error-status:get-message(1).
    END.
    else 
        return "OK".
end.

procedure pi_update_status :
    def input param h-bf as handle no-undo.
    
    def var c-valor as char no-undo.
    def var c-ano   as char no-undo.
    def var c-mes as char no-undo.
    def var c-dia as char no-undo.
    
    assign c-valor = string(today,"99/99/9999")
           c-ano = substring(c-valor,7,4)
           c-mes = substring(c-valor,4,2)
           c-dia = substring(c-valor,1,2)
           c-valor = c-ano + "-" + c-mes + "-" + c-dia + " " + string(time, "HH:MM")
           c-valor = "TO_DATE(" + "~'" + c-valor + "~' , " + "~'" + "yyyy-mm-dd hh24:mi" + "~')".


    assign ODBC-SQL-STMT = "UPDATE " + h-bf:name +
                               " SET ID_PROCESSADO = 'S', DT_PROCESSADO = " + c-valor + " " +
                               "WHERE CD_REGISTRO = " + h-bf:buffer-field("CD_REGISTRO"):buffer-value  .
                               
    put unformatted 
        ODBC-SQL-STMT skip.                          

    ObjCommand:CommandText       = ODBC-SQL-STMT.
    ObjCommand:execute ( output ODBC-NULL, "", 32 ).
end.

procedure pi_update_status_interface :
    def input param h-bf as handle no-undo.
    
    def var c-valor as char no-undo.
    def var c-ano   as char no-undo.
    def var c-mes as char no-undo.
    def var c-dia as char no-undo.
    
    assign c-valor = string(today,"99/99/9999")
           c-ano = substring(c-valor,7,4)
           c-mes = substring(c-valor,4,2)
           c-dia = substring(c-valor,1,2)
           c-valor = c-ano + "-" + c-mes + "-" + c-dia + " " + string(time, "HH:MM")
           c-valor = "TO_DATE(" + "~'" + c-valor + "~' , " + "~'" + "yyyy-mm-dd hh24:mi" + "~')".

    assign ODBC-SQL-STMT = "UPDATE " + h-bf:name +
                               " SET ID_PROCESSADO = 'S', DT_PROCESSADO = " + c-valor + " " +
                               "WHERE NU_INTERFACE = " + h-bf:buffer-field("NU_INTERFACE"):buffer-value  .  
                               
    put unformatted 
        ODBC-SQL-STMT skip.                              

    ObjCommand:CommandText       = ODBC-SQL-STMT.
    ObjCommand:execute ( output ODBC-NULL, "", 32 ).
end.

procedure pi_finalizar:

    ObjConnection:close no-error.

    RELEASE OBJECT ObjConnection NO-ERROR.
    RELEASE OBJECT ObjCommand    NO-ERROR.
    RELEASE OBJECT ObjRecordSet  NO-ERROR.
    
    ASSIGN ObjConnection = ? 
           ObjCommand = ? 
           ObjRecordSet = ?.

end.

procedure pi_select_view_estoque:
    def input param h-bf as handle no-undo.
    def input param p-cod-estabel as char no-undo.
    def input param p-cod-depos   as char no-undo.
    
    def var i as integer no-undo.

    assign ODBC-SQL-STMT = "SELECT ".

    do i = 1 to h-bf:num-fields:
        assign ODBC-SQL-STMT =  ODBC-SQL-STMT + h-bf:buffer-field(i):name.
    
        if i < h-bf:num-fields then
            assign ODBC-SQL-STMT =  ODBC-SQL-STMT + ",".
    end.

    assign ODBC-SQL-STMT = ODBC-SQL-STMT + 
                         " FROM " + h-bf:name + 
                         " WHERE CD_DEPOSITO = ~'" + p-cod-estabel + "~'" +
                         " AND CD_LOCAL_ESTOQUE = ~'" + p-cod-depos + "~'" .

    assign ObjCommand:CommandText       = ODBC-SQL-STMT
           ObjConnection:CursorLocation = 3 /* adUseClient */
           ObjRecordSet:CursorType      = 3 /* adOpenStatic */
           ObjRecordSet = ObjCommand:execute ( output ODBC-NULL, "", 32 )
           ODBC-RECCOUNT = ObjRecordSet:RecordCount.

    if ( ODBC-RECCOUNT > 0 ) and not ( ODBC-RECCOUNT = ? ) then do:
        
        ObjRecordSet:MoveFirst no-error.
        
        do while ODBC-CURSOR < ODBC-RECCOUNT:
            h-bf:buffer-create().
            do i = 1 to h-bf:num-fields:
                h-bf:buffer-field(i):buffer-value = ObjRecordSet:fields (h-bf:buffer-field(i):name):value.
            end.

            assign ODBC-CURSOR = ODBC-CURSOR + 1.
            ObjRecordSet:MoveNext no-error.
        end. 
    end. 
    else
        assign ODBC-STATUS = "No records found.".
end.
