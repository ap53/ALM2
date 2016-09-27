### testthat_alm.R

library(testthat)
if (!exists('dtb')) source("AlarmasMain.R")

#### Inicio ---------------------------------------------------------------------------------------

test_that("los datos están cargados", {
  expect_is(dtb, 'data.table' )
  expect_equal(ncol(dtb), 5 )
})

# NO CAMBIAR ESTA fecha_base: los tests están armados sobre datos correspondientes a la misma
# En los casos en que se quiere probar otra fecha, setearla dentro del test, nunca afuera
fecha_base_old <- fecha_base
fecha_base <- cambiar_fecha_base('2/9/2016')

#### Fechas ---------------------------------------------------------------------------------------
test_that("obtener fecha con duracion de tipo string del tipo 'day|month|year'", {
  fecha_final <- obtener_fecha('1 month', fecha_base, start = 0, Ix_base = 0)
  
  expect_is(fecha_final, "Date")
  expect_equal(day(fecha_final), day(fecha_base))
  expect_equal(month(fecha_final), month(fecha_base) - 1)
  expect_equal(year(fecha_final), year(fecha_base))
})

test_that("probar procesar_to_date", {
  fecha_base <- dmy('29/08/2016')
  fecha_inicial <- fecha_base
  fecha_final <- procesar_to_date('ytd', fecha_inicial)
  
  expect_is(fecha_final, "Date")
  expect_equal(day(fecha_final), 1)
  expect_equal(month(fecha_final), 1)
  expect_equal(year(fecha_final), year(fecha_base))
})


test_that("probar procesar_to_date del 1º de enero", {
  fecha_base <- dmy('1/1/2016')
  fecha_inicial <- fecha_base
  fecha_final <- procesar_to_date('ytd', fecha_inicial)
  
  expect_is(fecha_final, "Date")
  expect_equal(day(fecha_final), 1)
  expect_equal(month(fecha_final), 1)
  expect_equal(year(fecha_final), year(fecha_base))
})


test_that("probar devolver_fecha_de_duracion del tipo '*td' ", {
  fecha_base <- dmy('29/08/2016')
  fecha_inicial <- fecha_base
  start <- 0
  fecha_final <- devolver_fecha_de_duracion('mtd - 1', fecha_inicial, start)
  
  expect_is(fecha_final, "Date")
  
  # expect '29/jul/16' ya que el '31/jul/16' es domingo
  expect_equal(day(fecha_final), 29)
  expect_equal(month(fecha_final), 7)
  expect_equal(year(fecha_final), 2016)
})


#### Redondeos ---------------------------------------------------------------------------------------

test_that("calcular el factor de redondeo a usar para mostrar resultados de la alarma", {
  v1 <- 'a'; v2 <- 2
  expect_error(get_factor_redondeo(v1, v2), 'is.numeric\\(v1\\) is not TRUE')
  v1 <- 1; v2 <- 'a'
  expect_error(get_factor_redondeo(v1, v2), 'is.numeric\\(v2\\) is not TRUE')
  
  # Si la diferencia es < 1, mostrar los decimales que hagan falta,
  # independientemente del "valor" de los argumentos.
  v1 <- 0.12345; v2 <- 0.123456
  expect_is( get_factor_redondeo(v1, v2), "numeric" )
  expect_equal( get_factor_redondeo(v1, v2), 6 )
  
  v1 <- 12345.123; v2 <- 12345.1234
  expect_equal( get_factor_redondeo(v1, v2), 4 )
  
  # Si la diferencia es >= 1, mostrar uno o dos decimales,
  # segun si el "tamaño" de los argumentos.  
  v1 <- 1.123; v2 <- 2.1234
  expect_equal( get_factor_redondeo(v1, v2), 2 )
  
  v1 <- 101.123; v2 <- 102.1234
  expect_equal( get_factor_redondeo(v1, v2), 1 )
})

# test_that("redondear los resultados de la alarma", {
#   res <- list(izq_valor = 12345.1234, der_valor = 12345.1244)
#   print(1000000*res$der_valor)
#   res <- redondear_dif(res)
#   
#   print(1000000*res$der_valor)
#   expect_equal(res$izq_valor, 12345.1234)
#   expect_equal(res$der_valor, 12345.1234)
#   
# })

#### procesar_alarma ---------------------------------------------------------------------------------------

test_that("probar expresion con expr. algebraicas puras", {
  old_dbg <- dbg(0)
  fecha_base <- cambiar_fecha_base('2/9/2016')
  
  res <- probar_expresion((abs(2 + sign(-2)) + {3/3.0}) ^ 2)
  expect_equal(res$valor_ult, 4)
  expect_equal(res$flias, list())
  
  dbg(old_dbg)
})

test_that("probar expresion básica", {
  old_dbg <- dbg(0)
  fecha_base <- cambiar_fecha_base('2/9/2016')

  res <- probar_expresion(Margen_Over_Equity_Ratio('TOTAL T+ L'))

  expect_equal(res$valor_ult, 0.07108039)
  expect_equal(res$flias, list("TOTAL T+ L"))
  
  dbg(old_dbg)
})

test_that("probar expresion básica SIN flia", {
  old_dbg <- dbg(0)
  fecha_base <- cambiar_fecha_base('2/9/2016')
  
  expect_error(
    res <- probar_expresion(Margen_Over_Equity_Ratio()),
    'Como m.{1,2}nimo debe estar explicitado el par.{1,2}metro familia.')
  
  dbg(old_dbg)
})

test_that("probar expresion básica con duración", {
  old_dbg <- dbg(0)
  fecha_base <- cambiar_fecha_base('2/9/2016')
  
  res <- probar_expresion(Margen_Over_Equity_Ratio(2,'TOTAL T+ L', post_proceso = 'suma'))
  
  expect_equal(res$valor_ult, 0.14272, tolerance = 0.00001)
  expect_equal(res$flias, list("TOTAL T+ L"))
  
  dbg(old_dbg)
})

tmp_flia <- 'TOTAL T+ L'
test_that("probar expresion básica con variable", {
  old_dbg <- dbg(0)
  fecha_base <- cambiar_fecha_base('2/9/2016')
  
  res <- probar_expresion(Margen_Over_Equity_Ratio(2, tmp_flia, post_proceso = 'suma'))

  expect_equal(res$valor_ult, 0.14272, tolerance = 0.00001)
  expect_equal(res$flias, list("TOTAL T+ L"))
  
  dbg(old_dbg)
})

test_that("probar expresion básica con duración y start", {
  old_dbg <- dbg(0)
  fecha_base <- cambiar_fecha_base('2/9/2016')

  res <- probar_expresion(Margen_Over_Equity_Ratio(2, 3,'TOTAL T+ L', post_proceso = 'suma'))
  
  expect_equal(res$valor_ult, 0.13985, tolerance = 0.00001)
  expect_equal(res$flias, list("TOTAL T+ L"))
  
  dbg(old_dbg)
})

test_that("probar expresion un poco más compleja", {
  old_dbg <- dbg(0)
  fecha_base <- cambiar_fecha_base('2/9/2016')
  
  Ix_base <- calc_Ix_base()
  res <- list(tipo = 'tipo', fecha_base = fecha_base, Ix_base = Ix_base, importancia_potencial = 5,  importancia = NA, 
              mensaje_corto <- 'mensaje_corto', mensaje <- 'mensaje', 
              flias = list(), variables = list(), paso_crossover = 0 )
  
  res <- probar_expresion(abs(Margen_Over_Equity_Ratio('TOTAL T+ L')) + 
               Margen_Over_Equity_Ratio(2, 3,'TOTAL T+ L', post_proceso = 'suma'))
  
  expect_equal(res$valor_ult, 0.2109304, tolerance = 0.00001)
  expect_equal(res$flias, list("TOTAL T+ L"))
  
  dbg(old_dbg)
})


tmp_flia <- 'TOTAL T+ L'
test_that("probar expresion un poco más compleja con variable", {
  old_dbg <- dbg(0)
  fecha_base <- cambiar_fecha_base('2/9/2016')
  
  res <- probar_expresion(abs(Margen_Over_Equity_Ratio(tmp_flia)) + 
               Margen_Over_Equity_Ratio(2, 3, tmp_flia, post_proceso = 'suma'))
  
  expect_equal(res$valor_ult, 0.2109304, tolerance = 0.00001)
  expect_equal(res$flias, list("TOTAL T+ L"))
  
  dbg(old_dbg)
})


#### Llaves ---------------------------------------------------------------------------------------

test_that("probar expresion básica con llave", {
  old_dbg <- dbg(0)
  fecha_base <- cambiar_fecha_base('2/9/2016')
  
  res <- probar_expresion({Margen_Cuentas('TOTAL T+ L') / PatFamiliaFinal('TOTAL T+ L')}(200, p = 0.95))

  expect_equal(res$valor_ult, 0.09046, tolerance = 0.00001)
  expect_equal(res$flias, 
               list("Margen_Cuentas(TOTAL T+ L)/PatFamiliaFinal(TOTAL T+ L)")
  )
  
  dbg(old_dbg)
})

#### call_proc_alm ---------------------------------------------------------------------------------------

test_that("probar call_proc_alm", {
  old_dbg <- dbg(0)
  fecha_base <- cambiar_fecha_base('2/9/2016')
  
  res <- probar_expresion(Margen_Cuentas('TOTAL T+ L') / PatFamiliaFinal('TOTAL T+ L') > 
  {Margen_Cuentas('TOTAL T+ L') / PatFamiliaFinal('TOTAL T+ L')}(200, p = 0.95))
  
  # browser()
  expect_equal(res$valor_ult, FALSE)
  expect_equal(res$flias, list('TOTAL T+ L', "Margen_Cuentas(TOTAL T+ L)/PatFamiliaFinal(TOTAL T+ L)"))
  
  dbg(old_dbg)
})


#### Duración anclada ---------------------------------------------------------------------------------------

test_that("probar expresion con duración anclada", {
  old_dbg <- dbg(0)
  fecha_base <- cambiar_fecha_base('2/9/2016')
  
  Ix_base <- calc_Ix_base()
  res <- list(tipo = 'tipo', fecha_base = fecha_base, Ix_base = Ix_base, importancia_potencial = 5,  importancia = NA, 
              mensaje_corto <- 'mensaje_corto', mensaje <- 'mensaje', 
              flias = list(), variables = list(), paso_crossover = 0 )
  
  res <- probar_expresion(nav('ytd-1', 0, 'TOTAL T+ L', post_proc='primero'))
  
  expect_equal(res$valor_ult, 132.072, tolerance = 0.00001)
  expect_equal(res$flias, list("TOTAL T+ L"))

  dbg(old_dbg)
})


#### Final ---------------------------------------------------------------------------------------

# Reseteo la fecha_base a lo que tenía antes de los tests
fecha_base <- cambiar_fecha_base(fecha_base_old)
rm(fecha_base_old)



