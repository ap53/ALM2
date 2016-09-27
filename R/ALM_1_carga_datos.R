#### ALM_1_carga_datos ####

# Contiene todo lo referente a la lectura de los .RDS que traen info de bulk y de bbg.
# Obviamente debe correr antes que cualquier alarma...

carga_datos <- function(flias){

  ### Escondo las funciones que se llaman una única vez dentro de carga_datos,
  ### para reducir el ruido en el panel de Environment
  
  leer_datos_long <- function(){
    as.data.frame(readRDS(paste0(path, "data_long.RDS")))
  }
  
  cargar_long <- function() {
    # Lee el .RDS y genera el índice sobre los campos 'ticker' y 'variable
    # Asigna el resultado al dataframe/datatable 'dtb' en el globalenv
    dtb <- leer_datos_long() # Reads the RDS file
    keycols <- c('ticker', 'variable')
    setDT(dtb, keycols, keep.rownames=FALSE)
    dtb <- copy(dtb)
    #unlockBinding("dtb", environment)
    assign('dtb', dtb, globalenv(), inherits = FALSE)
    
    invisible(NULL)
  }
  
  
  leer_bulk <- function(){
    bulk <- as.data.frame(readRDS(paste0(path, "bulk.RDS")))
    
    # Sólo me quedo con los campos relevantes
    bulk <- bulk %>% 
      select(CarteraNom, Periodo, Especie, EspecieNombre, Codex1, Codex2, Tipo, 
             TipoNombre, Cantidad, CantPorLote, Precio, MonCodigoPrecio, TCambioPrecio, 
             Valuacion, Exposure, LongExposure, ShortExposure, Resultado, 
             PatrimonioCartera, Cartera, AT13, AT12, Estructura, AT2, AT11, Margin)
  }

  leer_familias <- function(){
    familias <- as.data.frame(readRDS(paste0(path, "familias.RDS")))
  }
  
  extraer_de_bulk <- function(bulk) {
    # Algunas variables vienen en Bulk, con un formato incómodo.
    # Las reformateo y preparo para agregarlas a dtb
    prep_variables <- familias %>% tbl_df %>% 
      filter(Descripcion %in% familias_importantes) %>% 
      select(Familia = Descripcion, CarteraNom) %>% 
      inner_join(bulk %>% 
                   select(Periodo, CarteraNom, Valuacion, Cantidad, 
                          AT12, TipoNombre, EspecieNombre) %>% 
                   mutate(Margen = Valuacion, Exposure = abs(Valuacion)), 
                 by = 'CarteraNom') %>% 
      gather(variable, value, Margen, Cantidad, Exposure) %>% 
      mutate(borrar = 
               ifelse (
                 (variable == 'Margen' & 
                    ((AT12 != 'Cash Margin Accounts') | 
                       (TipoNombre != 'Cash and Banks') | 
                       (!str_detect(EspecieNombre, ' Margin ')))
                 ) |
                   variable == 'Exposure' & (TipoNombre %in% noExposureID), 1, 0) ) %>% 
      select(date = Periodo, ticker = Familia, variable, value, 
             borrar, AT12, TipoNombre, EspecieNombre) %>% 
      filter(borrar != 1) %>% 
      tbl_df
    
    prep_variables %>% select(date, ticker, variable, value) %>% 
      group_by(date, ticker, variable) %>% summarise(value = sum(value)) %>% 
      mutate(source = 'bulk') %>% ungroup
  }
  
  armar_diccionario <- function() {
    # Armo un diccionario de términos que pueden llegar a aparecer como 
    # funciones en las expresiones de alarmas

        dtb %>% as_data_frame() %>% 
      select(variable, source) %>% unique %>% 
      arrange(source, variable)
  }
  
  # Filtro para excluir ciertas cuentas de los cálculos de Bulk..Exposure 
  noExposureID <- c("Cash and Banks",
                    "Accounts Payable",
                    "Sales - Pending Settlement",
                    "Accounts Receivable",
                    "Purchases - Pending Settlement",
                    "Other Receivables ",
                    "Other Payables",
                    "Div or Coup-Pending Settlement",
                    "Redemption Debt",
                    "Contingent Liabilities")
  
  
  ######### Rutina de carga_datos propiamente dicha **************************************************

  # Los resultados que van a JIRA (alarmas que se dispararon) se backupean
  # individualmente en una subcarpeta de aquella que contiene los datos.
  if(!dir.exists(file.path(path, 'Backups_Alarmas')))
    dir.create(file.path(path, 'Backups_Alarmas'))
  
  # Datos iniciales
  # Mientras ALM esté en desarrollo, quiero poder acceder a todos los datos desde globalenv
  cargar_long()
  assign('familias', leer_familias(), globalenv())
  
  # No es realmente necesario tener a bulk en el globalenv... Mientras dure el
  # desarrollo lo dejo para poder hacer verificaciones. Después lo podremos transformar en
  # variable local y permitir que se libere la memoria cuando termine de correr carga_datos()
  assign('bulk', leer_bulk(), globalenv()) 
  
  #  Creación de un índice de días, que permite cambiar rápidamente de fecha_base
  assign('dias', 
         dtb %>% filter(source == 'pnl') %>% 
           select(date) %>% unique %>% arrange(desc(date)) %>% 
           mutate(IxDia = row_number() - 1), 
         globalenv())
  
  # Definir fecha_base como global, para poder usarla interactivamente
  alarm_env$max_fecha_datos <- dtb[.('TOTAL T+ L', 'nav'), .(date)][order(-date), date][1]
  assign('fecha_base', alarm_env$max_fecha_datos, globalenv())
  
  # Algunas variables 'pseudo-pnl' se construyen en ALM, sacando datos de bulk
  variables_bulk <- extraer_de_bulk(bulk)
  # Las agrego a dtb
  l <- list(dtb, variables_bulk)
  dtb <<- rbindlist(l, use.names=TRUE)
  keycols <- c('ticker', 'variable')
  setDT(dtb, keycols, keep.rownames=FALSE)
  
  # Armo un diccionario de términos que pueden llegar a aparecer como funciones
  # en las expresiones de alarmas
  diccionario <<- armar_diccionario()
  
  return(invisible())
}

#### Armar diccionario ------------------------------------------------------------------------
diccionario_terminos <- function(mostrar_llaves = FALSE){
  terminos <- (diccionario %>% select(variable))[[1]]
  
  if (mostrar_llaves) return(terminos) else return(terminos[which(!str_detect(terminos, '_Ovr_'))])
}

verificar_diccionario <- function(nombre){
  as.list(diccionario %>% filter(variable == nombre))
}


ver <- function(ticker_, variable_ = '', slice_ = 1:10) {
  # Utilitario para ser usado manualmente para mirar (o buscar) info de dtb
  # ToDo: mini-manual de uso
  
  if (ticker_ != '' && variable_ != ''){
    d <- dtb[.(ticker_, variable_), nomatch = 0]
    if (length(slice_) == 1 && slice_ == 0 ) {
      print(dim(d))
      print(head(d))
      print(tail(d))
    } else {
      primer_fila_pedida <- slice_[1]
      ultima_fila_pedida <- slice_[length(slice_)]
      if (nrow(d) == 0) {
        print('No hay ningún dato en la serie ')
      } else if (nrow(d) < primer_fila_pedida) {
        print('No hay ningún registro en el rango pedido (sólo hay', nrow(d), 'filas) ')
      } else if (nrow(d) < ultima_fila_pedida) {
        print('No hay suficientes datos en el rango pedido (sólo hay', nrow(d), 'filas) ')
      }
      return(d[order(-date)] %>% slice(slice_))
    }
    
  } else if (ticker_ != '') {
    d <- dtb[ticker_, .(ticker, variable), nomatch = 0] %>% unique()
    if (nrow(d) == 0) {
      cat('No se encontró el ticker "', 
          ticker_, 
          '", valores posibles:\n', sep = '')
      return(dtb %>% select(ticker) %>% 
               filter(str_detect(str_to_lower(ticker), str_to_lower(ticker_))) 
             %>% unique %>% arrange())
    } else {
      return(d)
    }
    
  }  else if (variable_ != '') {
    d <- dtb[.(unique(ticker), variable_), .(ticker, variable), nomatch = 0] %>% unique()
    if (nrow(d) == 0) {
      cat('No se encontró la variable "', 
          variable_, 
          '", valores posibles:\n', sep = '')
      return(dtb %>% select(variable) %>% 
               filter(str_detect(str_to_lower(variable), str_to_lower(variable_)))
             %>% unique %>% arrange())
    } else {
      return(d)
    }
  }
}

limpiar_llaves <- function(){
  habia <- nrow(dtb)
  dtb <<- dtb %>% filter(variable != 'llave') %>% tbl_df
  keycols <- c('ticker', 'variable')
  setDT(dtb, keycols, keep.rownames=FALSE)
  
  sprintf("Se limpiaron %d filas de tipo 'llave'.", habia - nrow(dtb))
}
#### alarm_env ----------------------------------------------------------------------------------
# Escondo algunas variables y funciones en un environment propio...
init_alarm_env <- function(){
  alarm_env <- new.env(parent = emptyenv())
  
  
  alarm_env$get_next_alarm_id <- function(){
    # El resultado de cada alarma que se corre tiene un id propio
    # Esta función devuelve el siguente id
    
    alarm_env$id_alarma <- alarm_env$id_alarma + 1
    return(alarm_env$id_alarma)
  }
  
  alarm_env$reset_alarm_id <- function(valor = 0L){
    # Utilitario para volver los ids a un valor determinado.
    # Por ahora no tiene mucho uso, tal vez cuando pasemos a base de datos...
    
    alarm_env$id_alarma <- valor
    return(alarm_env$id_alarma)
  }
  
  alarm_env$show_envs <- function(){
    # Utilitario para ver el contenido de alarm_env.
    # Más que nada para debugging, no tiene uso dentro de ALM...
    
    print(parent.frame())
    environment()
  }
  
  
  ## Rutinas para contar alarmas (por importancia) en cada corrida_alarmas()
  alarm_env$cant_alarmas <- numeric(6)
  
  alarm_env$init_cant_alarmas <- function() {
    alarm_env$cant_alarmas <- numeric(6)
  }
  
  alarm_env$contar_alarma <- function(importancia) {
    # Voy contando en casilleros diferentes de acuerdo a la importancia de cada alarma checkeada
    
    importancia <- trunc(importancia)
    alarm_env$cant_alarmas[importancia + 1] <- 
      alarm_env$cant_alarmas[importancia + 1] + 1
    
    return(invisible(NULL))
  }
  
  alarm_env$ver_cuantas_corrieron <- function() {
    # Informe final de la corrida
    
    msg <- paste('Alarmas corrida:', sum(alarm_env$cant_alarmas), 
                 '[', paste(alarm_env$cant_alarmas, collapse = ', '), ']')
  }
  
  ### Código para la ejecución propiamente dicha de init_alarm_env() ************************************
  # Read last id_alarma used from file
  if (file.exists('id_alarmas_file.txt')) {
    alarm_env$id_alarma <- as.numeric(readLines('id_alarmas_file.txt'))
  } else {
    alarm_env$id_alarma <- 0L
    cat(0, file="id_alarmas_file.txt", sep = "\n")
  }
  
  #           *** ¡¡¡Esta variable todavía no se usa!! ***
  # Cuando termine el desarrollo la lista 'res', que acumula info a lo largo de
  # la evaluación de una alarma, debería pasar acá...
  alarm_env$res <- list()
  
  
  if (exists('dtb') || exists('datos_long')) {
    # A veces, por ejemplo al hacer un upgrade de algún package, tengo dtb pero falta alguna librería
    library(data.table)
    
    # Este es el valor "correcto" de max_fecha_datos
    
    # Normalmente no debería ser necesario cargarlo aquí, ya que cargar_datos()
    # lo va a hacer más adelante, pero se generan problemas cuando re-sourceo el fuente 
    # sin volver a correr cargar_datos()...
    alarm_env$max_fecha_datos <- dtb[.('TOTAL T+ L', 'nav'), .(date)][order(-date), date][1]
  } else {
    # En este caso, sol genero la variable. (¿Necesario?)
    alarm_env$max_fecha_datos <- Sys.Date()
  }
  
  assign('fecha_base', alarm_env$max_fecha_datos, globalenv())
  
  return(alarm_env)
}


### Inicializo (o re-inicializo) alarm_env cuando se sourcea el archivo fuente ***************************
if (!exists('dtb')) {
  alarm_env <- init_alarm_env()
}

