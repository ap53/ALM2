# Proyecto Alarmas: ALM
# Versión 0.2

# Parametros generales
path <- "..\\data\\"
archivo_salida <- "salida_Antoine.csv"

source('./R/ALM_0_Funciones.R', encoding = 'UTF-8')

familias_importantes <- c('VIX', 'MACROMODEL', 'Alfa Total', 'TOTAL T+ L','DONCHIAN',
                          'BREAKOUTS', 'BREAKOUTS+DONCHIAN', 'FUTUROS ADMINISTRADOS', 
                          'VIX + STRANGLES', 'PRIMA MERCADO DESARROLLADOS','VIX','STRANGLES','STRANGLES EN VIX')
carga_datos(familias_importantes)

#### Corridas ----------------------------------------------------------------------

corrida_alarmas('AlarmasDataContableX.R')
corrida_alarmas('AlarmasDataBBGX.R')
corrida_alarmas('alarmas_varX.R')

dif_familias <- comparar_familias(fecha_base, paste0(path, 'Backup Data Contable/'))
