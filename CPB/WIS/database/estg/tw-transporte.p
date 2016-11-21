/******************************************************************************
**  Programa.....: estg/tw-transporte.p
**  Elaboracao...: Dezembro/2009.
**  Autor........: Eder
**  Cadastrar TRG: transporte.
**  Objetivo.....: Exportar dados de transportadoras para WMS.
**  Versoes......: 001 - 20/12/2009 - Desenvolvimento Programa
******************************************************************************/

def param buffer transporte     for transporte.
def param buffer old-transporte for transporte.

run esp/eswms003.p (buffer transporte).

return "OK".  





