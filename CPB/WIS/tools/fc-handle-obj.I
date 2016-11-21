/*
**
**   Programa: FUNCAO - Busca handle de campo desejado
**      Autor: Maurilio Macedo
**       Data: 22/07/2001
**     Versao: 1.00
** Parametros: p-obj   = Objetos a serem pesquisados. ex: "it-codigo,descricao"
**             p-frame = Handle do frame para pesquisa
**    Retorno: Handle dos objetos encontrados na mesma ordem de envio do campo
**             p-obj.         
**
*/

function fc-handle-obj RETURN character (p-obj as char, p-frame as widget-handle):

def var wh-objeto as widget-handle no-undo.

IF p-frame:TYPE = "browse" THEN 
   ASSIGN wh-objeto = p-frame:FIRST-COLUMN.
ELSE
   ASSIGN wh-objeto = p-frame:first-child.

do while valid-handle(wh-objeto):

   if wh-objeto:type = "field-group" then do:
      p-obj = fc-handle-obj(p-obj,wh-objeto).
   end. 
   
   if wh-objeto:type = "frame" then do:
      p-obj = fc-handle-obj(p-obj,wh-objeto).
   end. 

       
   if lookup(wh-objeto:name,p-obj) <> 0 and
      lookup(wh-objeto:name,p-obj) <> ? then do:
      entry(lookup(wh-objeto:name,p-obj),p-obj) = string(wh-objeto:handle).
   end.   
     
   IF p-frame:TYPE = "browse" THEN 
      ASSIGN wh-objeto = wh-objeto:NEXT-COLUMN.
   ELSE
      ASSIGN wh-objeto = wh-objeto:next-sibling.

/*       MESSAGE wh-objeto:NAME                 */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK. */
end.
    
return p-obj.

end function.

