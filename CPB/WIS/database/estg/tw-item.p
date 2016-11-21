/******************************************************************************
**  Programa.....: estg/tw-item.p
**  Elaboracao...: Novembro/2016.
**  Autor........: Joao Pacheco
**  Cadastrar TRG: Item.
**  Objetivo.....: Exportar dados de item para WMS.
******************************************************************************/

DEF PARAM BUFFER item     FOR item.
DEF PARAM BUFFER old-item FOR item.

run esp/eswms001.p (buffer item).

RETURN "OK".  





