/******************************************************************************
**  Programa.....: upc/u01cd0204.p
**  Elaboracao...: Dezembro/2009.
**  Autor........: Eder
**  Cadastrar UPC: CD0204.
**  Objetivo.....: Exportar dados do item para WMS.
**  Versoes......: 001 - 20/12/2009 - Desenvolvimento Programa
******************************************************************************/
DEF INPUT PARAM p-ind-event                     AS CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object                    AS CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object                    AS HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame                     AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table                     AS CHAR          NO-UNDO.
DEF INPUT PARAM p-row-table                     AS ROWID         NO-UNDO.

if (p-ind-event  = "AFTER-END-UPDATE" or
    p-ind-event  = "BEFORE-ADD")      and 
    p-ind-object = "VIEWER"           and 
    p-wgh-object:file-name = 'invwr/v34in172.w':U then do:

    find item no-lock where
         rowid(item) = p-row-table no-error.
         
    run esp/eswms001.p (buffer item).
    
    /*run esp/eswms015.p (buffer item).*/

end.
