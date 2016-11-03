### ALM_5_utils.R


dbg <- function(que_hacer) {
  # Permite tener un toggle para hacer algo cuando estoy debuggeando
  # OJO: los llamados/retornos a/de esta función son medio inconsistentes:
  #     dbg(1) 'prende' el modo DEBUG: devuelve 'old', o sea el estado
  #            anterior, para poder volver a dejarlo igual.
  # 
  #     dbg(0) 'apaga' al modo DEBUG: también devuelve 'old'
  # 
  #     dbg()  sin argumentos devuelve el estado actual: TRUE o FALSE 
  #            según si DEBUG está prendido o no
  
  if (missing(que_hacer)) {
    if (exists('DEBUG')) {
      return(DEBUG)
    } else {
      return(FALSE)
    }
  } else {  # Significa que estoy prendiendo o apagando: 
    if (exists('DEBUG')) {  # para poder resetearlo al salir
      old <- DEBUG
    } else {
      old <- FALSE
    }
    
    if (que_hacer == 0) {
      if (exists('DEBUG')) rm(DEBUG, envir = globalenv())
    } else {
      assign('DEBUG', TRUE, globalenv())
    }
    return(old)
  }
}


llamado_desde <- function(f_name) {
  ### llamado_desde
  
  # Detecta si la función denominada f_name está en la cadena de llamados
  # Devuelve 0 si no está, o el número del frame (es un número positivo)
  vect_coincidencia <- str_detect(sapply(sys.calls(), 
                                         function(x) as.character(x)[[1]][1]), 
                                  paste0('^', f_name, '$') )
  
  if (any(vect_coincidencia)) {
    which(vect_coincidencia)
  } else {0}
}

#### fecha_base ----------------------------------------------

cambiar_fecha_base <- function(fecha = NULL){
  # The default is to calculate alarms as seen from the most recent day in the data.
  # This date is stored in a global variable, "fecha_base"
  #
  # To ensure that we don't run the alarms with an old "fecha_base", this variable is
  # explicitely reset both at the end of carga_datos and at the beginning of corrida_alarmas.
  #
  # fecha_base can be changed, EVEN IN THE MIDDLE of a 'corrida_alarmas' file by calling: 
  # cambiar_fecha_base(new_date), with a dmy format. Example: 
  # fecha_base <- cambiar_fecha_base('22/8/2015')
  # 
  # After being changed, fecha_base can be easily reset to its original value by calling: 
  # cambiar_fecha_base(), with no argument.
  
  if(as.character(sys.calls()[[1]])[1] == 'corrida_alarmas' && length(sys.calls()) > 2) {
    warning('Las solicitudes de cambio de fecha base se ignoran durante las corridas de alarmas...')
    return(fecha_base)
  }
  
  if (is.null(fecha)) {
    return((dias %>% select(date) %>% slice(n()))[[1]])
  } else {
    if (class(fecha) == 'character') {
      
      try(ff <- suppressWarnings(dmy(fecha)))
      suppressWarnings(
        if (is.na(ff)) {
          stop(paste(fecha, 'no es una fecha válida'))
        } else {
          fecha <- ff
        }
      )
    }
    
    if (class(fecha) != 'Date') stop(paste(fecha, 'no es una fecha válida'))
    
    suppressWarnings(
      if (length(fecha) > 1) stop('Sólo se puede pasar una fecha')
    )
    
    if (fecha <= (dias %>% select(date) %>% slice(n()))[[1]])
      stop(paste(fecha, 'es demasiado temprano'))
    
    if (wday(fecha) %in% c(1, 7)) stop(paste(fecha, 'es fin de semana'))
    
    ff <- dias %>% filter(date == fecha) %>% select(date)
    if (ff %>% nrow == 0 ) stop(paste(fecha, 'no figura en datos'))
    return(ff[[1]])
  }
}


#### duracion anclada ----------------------------------------------
obtener_fecha <- function(duracion, fecha_inicial, start, Ix_base) {
  durac_num <- str_replace_all(duracion, '[^0-9]', '')
  durac_unid <- str_replace_all(duracion, '[^a-zA-Z_]', '') %>% str_replace('s', '')
  
  if (durac_unid == 'pnl_day') {
    palabra_tiempo = 'días [pnl]'
    unidad_tiempo <- 'day'
    duracion_max <- 1000
  } else if (durac_unid == 'day') {
    palabra_tiempo = 'dias'
    unidad_tiempo <- 'days'
    duracion_max <- 1500
  } else if (durac_unid == 'month') {
    palabra_tiempo = 'meses'
    unidad_tiempo <- 'months'
    duracion_max <- 60
  } else if (durac_unid == 'year') {
    palabra_tiempo = 'años'
    unidad_tiempo <- 'years'
    duracion_max <- 5
  }
  
  if (durac_num == '') durac_num <- 1
  
  try(duracion <- as.numeric(durac_num))
  if (inherits(duracion, 'try-error') || inherits(duracion, 'error')) {
    stop('duracion:', duracion, ' cantidad de ', palabra_tiempo, ' inválida.')
  } else if (duracion < 1 || duracion > duracion_max) {
    stop('duracion:', duracion, ' cantidad de ', palabra_tiempo, ' fuera de rango')
  }
  
  if (durac_unid == 'pnl_day') {
    fecha <- (dias %>% filter(IxDia == Ix_base + start + duracion - 1) %>% select(date))[[1]]
  } else {
    l <- list(durac_num)
    names(l) <- unidad_tiempo
    fecha <- fecha_inicial - do.call(period, l)
  }
  
}

procesar_to_date <- function(duracion, fecha_inicial) {
  
  tramo_1 <- str_sub(duracion, 1, 1)
  fecha_final <- floor_date(fecha_inicial, 
                            unit = switch(tramo_1, 
                                          m = 'month',
                                          w = 'week',
                                          y = 'year') )
  # Lubridate weeks start on sundays...
  if (tramo_1 == 'w' && wday(fecha_final) == 1) fecha_final <- fecha_final + days(1)
  
  fecha_final
}

devolver_fecha_de_duracion <- function(duracion, fecha_inicial, start) {
  tramos <- str_split(duracion, '-')
  tramos <-  str_trim(tramos[[1]])
  if (length(tramos) > 2 || length(tramos) == 0) {
    stop('Especificacion de duracion inválida: ', duracion)
  }
  
  tramo_1 <- tramos[1]
  if (str_detect(tramo_1, '^[wmy]td')) {
    fecha_final <- procesar_to_date(str_sub(tramos[1], 1, 1), fecha_inicial)
  } else if (str_detect(tramo_1, 'pnl_day')) {
    fecha_final <- obtener_fecha(tramo_1, fecha_inicial, start, Ix_base)
    
  } else if (str_detect(tramo_1, 'day|month|year')) {
    fecha_final <- obtener_fecha(tramo_1, fecha_inicial, start, Ix_base) 
    
  } else if (str_detect(tramo_1, '^[0-3]?[0-9]{1}[/-][0-1]?[0-9]{1}[/-][0-9]{2,4}')) {
    try(fecha_final <- dmy(tramo_1))
    if (inherits(fecha_final, 'try-error') || inherits(fecha_final, 'error')) {
      stop('duracion:', tramo_1, ' fecha inválida.')
    } else if (fecha_final < dmy('1/1/2012')) {
      stop('duracion:', tramo_1, ' fecha demasiado temprana')
    } else if (fecha_final > fecha_base) {
      stop('duracion:', tramo_1, ' > fecha_base')
    }
    
  } else {
    stop(tramo_1, ': duracion desconocida o inválida')
  }
  
  dias_pnl <- TRUE
  
  if (length(tramos) > 1) {
    atras_num <- as.numeric(str_replace_all(tramos[2], '[^0-9]', ''))
    if (is.na(atras_num)) atras_num <- 1
    
    atras_unid <- str_replace_all(tramos[2], '[^a-zA-Z_]', '') %>% str_replace('s', '')
    if (atras_unid == '') atras_unid <- 'd'
    
    
    if (atras_unid != str_to_lower(atras_unid)) {
      dias_pnl <- FALSE
      atras_unid <- str_to_lower(atras_unid)
    }
    
    try(fecha_final <- fecha_final - 
          do.call(switch(atras_unid, d = 'days', m = 'months', w = 'weeks', y = 'years'),
                  list(atras_num))
    )
    
    if (is.na(fecha_final)) 
      stop('"', duracion, '": especificación de duración inválida', call. = FALSE)
    
  }
  
  if (dias_pnl) {
    if(wday(fecha_final) %in% c(1, 7))
      fecha_final <- fecha_final - days(1)
    if(wday(fecha_final) %in% c(1, 7))
      fecha_final <- fecha_final - days(1)
  }
  
  fecha_final
}

detectar_duracion_string <- function(param) {
  ### detectar_duracion_string
  
  es_durac_str <-  str_detect(param, '^[wmy]td') || 
    str_detect(param, 'pnl_day') || 
    str_detect(param, 'day|month|year') || 
    str_detect(param, '^[0-3]?[0-9]{1}[/-][0-1]?[0-9]{1}[/-][0-9]{2,4}')
}

cb <- function(df, sep="\t", dec=",", max.size=(200*1000)){
  # Copy a data.frame to clipboard
  write.table(df, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
}

# improved list of objects
.ls.objects <- function (env = .GlobalEnv, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, env = env)))
  names <- ls(env = env, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(print(object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

# shorthand
lsos <- function(..., n=20) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

