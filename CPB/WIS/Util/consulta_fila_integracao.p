SELECT cod-trans, COUNT(*) FROM esp-fila-wms WHERE data-hora-integracao IS NULL
GROUP BY cod-trans.


SELECT COUNT(*) FROM esp-fila-wms WHERE data-hora-integracao IS NULL.


DELETE FROM esp-fila-wms WHERE cod-trans = "emitente".
