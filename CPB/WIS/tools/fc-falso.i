/*
**    Programa: fc-falso.i
**    Objetivo: Fun‡Æo para criacao de objetos falsos (botoes, fill-in, ...)
**       Autor: Maurilio Macedo - Datasul
** Atualiza‡Æo: 16/07/2001
**
*/

/*----- DEFINICAO DE FUNCOES -----*/
FUNCTION fc-falso RETURN WIDGET-HANDLE (p-objeto AS WIDGET-HANDLE,
                                        p-frame  AS WIDGET-HANDLE,
                                        p-programa-choose AS CHAR).

DEFINE VAR fc-wh-objeto AS WIDGET-HANDLE NO-UNDO.

IF  p-objeto:TYPE = "BUTTON" THEN DO:

    CREATE BUTTON fc-wh-objeto
        ASSIGN 
           FRAME       = p-frame
           WIDTH       = p-objeto:WIDTH
           HEIGHT      = p-objeto:HEIGHT
           LABEL       = p-objeto:LABEL + " *"
           ROW         = p-objeto:ROW
           COLUMN      = p-objeto:COLUMN
           TOOLTIP     = p-objeto:TOOLTIP
           HELP        = p-objeto:HELP
           NAME        = "falso-" + p-objeto:NAME
           SENSITIVE   = YES
           VISIBLE     = YES
           FONT        = p-objeto:FONT
        TRIGGERS:
           ON 'choose':U PERSISTENT RUN value(p-programa-choose).
        END triggers.

    p-objeto:SENSITIVE = NO.
    fc-wh-objeto:MOVE-AFTER-TAB-ITEM(p-objeto) NO-ERROR.
    return fc-wh-objeto.

END.

IF  p-objeto:TYPE = "FILL-IN" THEN DO:

    CREATE FILL-IN fc-wh-objeto
        ASSIGN 
           FRAME       = p-frame
           WIDTH       = p-objeto:WIDTH
           HEIGHT      = p-objeto:HEIGHT
           ROW         = p-objeto:ROW
           COLUMN      = p-objeto:COLUMN
           DATA-TYPE   = p-objeto:DATA-TYPE
           FORMAT      = p-objeto:FORMAT
           NAME        = "falso-" + p-objeto:NAME
           SENSITIVE   = YES
           VISIBLE     = YES
        TRIGGERS:
           ON "F5", 
              "F8",
              "LEAVE",
              "ENTRY",
              "MOUSE-SELECT-DBLCLICK" PERSISTENT RUN value(p-programa-choose).
        END triggers.

    p-objeto:SENSITIVE = NO.
    
    return fc-wh-objeto.

END.

IF  p-objeto:TYPE = "TOGGLE-BOX" THEN DO:

    CREATE TOGGLE-BOX fc-wh-objeto
        ASSIGN 
           FRAME       = p-frame
           WIDTH       = p-objeto:WIDTH
           HEIGHT      = p-objeto:HEIGHT
           ROW         = p-objeto:ROW
           COLUMN      = p-objeto:COLUMN
           FORMAT      = p-objeto:FORMAT
           LABEL       = p-objeto:LABEL  
           FONT        = p-objeto:FONT   
           NAME        = "falso-" + p-objeto:NAME
           SENSITIVE   = YES
           VISIBLE     = YES
        TRIGGERS:
           ON "VALUE-CHANGED"
              PERSISTENT RUN value(p-programa-choose).
        END triggers.

    p-objeto:SENSITIVE = NO.
    
    return fc-wh-objeto.

END.


END FUNCTION.
