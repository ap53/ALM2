### ALM_3_correr_alarma.R

probar_expresion <- function(...) {
  lista_res <- c_a(prueba_expr = 1, ...)
  if (dbg()) print(lista_res)
  return(lista_res)
}

c_a <- function(alarma, 
                importancia = 5, tipo = '', mensaje_corto = '', mensaje = '', 
                # parametros usados cuando se llama desde la funcion 'crossover'
                paso_crossover = 0, permanente = 0,
                # parametros usados solo para testear parseo de expresiones
                prueba_expr = 0){
  ### c_a
  # ToDo: SACAR ESTO
  origen_termino <- 'pnl cableado'
  
  # Inicializar el proceso
  Ix_base <- calc_Ix_base()
  alarm_env$res <- list(tipo = tipo, fecha_base = fecha_base, Ix_base = Ix_base, 
                        importancia_potencial = importancia, importancia = NA,
                        mensaje_corto = mensaje_corto, 
                        mensaje = str_replace_all(mensaje, '\n +', '\n'), 
                        flias = list(), variables = list(), paso_crossover = paso_crossover )
  
  # flias y variables guardan datos que se van descubriendo a lo largo de la 
  # transformación de la expresión de la alarma y que se usan después al armar 
  # los informes a JIRA.
  # procesar_alarma() (y test_expresion_simple()) las modifican como
  # side-effect.
  flias = list()
  variables = list()
  
  
  # res se modifica como side-effect del llamado a procesar_alarma() o test_expresion_simple()
  if (!prueba_expr) {
    # res <- 
    procesar_alarma(alarma)
  } else {
    # res <- 
    test_expresion_simple(alarma)
  }
  
  alarm_env$res$flias <- unname(flias)
  alarm_env$res$variables <- variables
  
  if (prueba_expr) return(alarm_env$res)
  
  informar_resultados() #, importancia, paso_crossover, permanente
  return(invisible(alarm_env$res))
}


procesar_alarma <- function(alarma) {     # , importancia, 
                                               # mensaje_corto, mensaje, paso_crossover
  ### procesar_alarma
  if (dbg()) { # Cuando queremos debugear, es conveniente evitar el tryCatch
    res <- procesar_alarma_body(alarma)
  } else {
    res <- proceso <- tryCatch({
      procesar_alarma_body(alarma)
    },
    error=function(cond) {
      alarm_env$res$eval <<- "ERROR AL CORRER LA ALARMA"
      alarm_env$res$alarma <- NA
      alarm_env$res$mensaje <- cond
      return(alarm_env$res)
    })
    
  }
  return(alarm_env$res)
}

procesar_alarma_body <- function(alarma) {    # , importancia, 
                                                   # mensaje_corto, mensaje, paso_crossover
  ### procesar_alarma_body
  expr <- expr_find(alarma)
  alarm_env$res$expr <- expr_text(alarma)
  alarm_env$res$expr_txt <- para_imprimir(alarma, nivel = 0)
  
  # OJO: si corre una alarma con el formato de la versión 0.1 original, 
  # el parámetro flia1 queda mapeado a paso_crossover...
  if (!is.numeric(alarm_env$res$paso_crossover) || !alarm_env$res$paso_crossover %in% c(0, 1, 2)) {
    # assign('paso_crossover', 0, parent.frame())
    stop('Llamada inválida a c_a: ', alarm_env$res$paso_crossover, 
         ' aparece en una posición inesperada.', call. = FALSE)
  }
  
  if (exists('expr') && (class(expr) == 'call')) {
    # Aprovechando que las alarmas deben ser definidas como expresiones de
    # comparación, separo las ramas izq y der a fin de poder informarlas por separado...
    alarm_env$res$operador = expr[[1]] 
    alarm_env$res$izq = expr[[2]]
    alarm_env$res$der = expr[[3]]
    
    # Para cada rama voy guardando: el texto (en el que luego voy a reemplazar
    # el valor de las variables si es que las hay), la expresión transformada y
    # el valor resultante de su evaluación.
    alarm_env$res$izq_text <- para_imprimir(alarma, nivel = 2)
    alarm_env$res$izq_modif <- transformar_expresion(expr[[2]])
    # A veces el valor viene con un name, que me complica la vida: mejor lo elimino de entrada
    alarm_env$res$izq_valor <- eval(alarm_env$res$izq_modif) %>% unname
    alarm_env$res$valor_ult <- alarm_env$res$izq_valor
    
    alarm_env$res$der_text <- para_imprimir(alarma, nivel = 3)
    alarm_env$res$der_modif <- transformar_expresion(expr[[3]])
    alarm_env$res$der_valor <- eval(alarm_env$res$der_modif) %>% unname
    alarm_env$res$valor_ult <- alarm_env$res$izq_valor
    
    # Ya evaluadas ambas ramas, puedo evaluar la alarma completa
    alarm_env$res$alarma <- do.call(as.character(alarm_env$res$operador), list(alarm_env$res$izq_valor, alarm_env$res$der_valor))
    alarm_env$res$valor_ult <- alarm_env$res$alarma
    alarm_env$res$importancia <- sign(alarm_env$res$alarma) * alarm_env$res$importancia_potencial
    if (alarm_env$res$alarma) {
      alarm_env$res$mensaje <- str_replace_all(alarm_env$res$mensaje, '\n +', '\n')
    } else {
      alarm_env$res$mensaje_corto <- iconv('No se disparó' , from = "", to = "UTF-8")
      alarm_env$res$mensaje <- ''
    }
    
    alarm_env$res <- redondear_dif(alarm_env$res)
    alarm_env$res$eval <- paste(alarm_env$res$izq_valor, alarm_env$res$operador, alarm_env$res$der_valor)
  } else {
    stop('Alarma inválida', call. = FALSE)
  }
  
  return(alarm_env$res)
}


calc_termino <- function(serie, duracion = 0, start = NULL, familia, post_proceso = c('percentil'), 
                         p = NULL, ND = -1, ver_serie = FALSE,
                         es_llave = FALSE) {
  ### calc_termino
  
  ticker_ <- familia
  pos_frame_ppal <- llamado_desde('c_a')

  if (!llamado_desde('llave')) {
    flias <- get('flias', envir = sys.frame(pos_frame_ppal))
    flias <- c(flias, familia) %>% unique
    # NOTA: Side-effect: estoy modificando la list 'flias'
    assign('flias', flias, envir = sys.frame(pos_frame_ppal))
  } 

  if (llamado_desde('llave') || es_llave) {
    datos_uno <- dtb[.(ticker_, serie), nomatch = 0] %>% 
      arrange(desc(date)) %>% select(value)
  } else {
    
    if (duracion > 1 && post_proceso == 'percentil' && is.null(p)) {
      stop('Falta especificar el post_proceso (o tal vez el percentil)', call. = FALSE)
    }
    
    # Si estoy en el segundo paso de un crossover(), tengo que mirar el día anterior
    xovr_slide <- ifelse(alarm_env$res$paso_crossover == 2, 1, 0)
    
    duracion_anclada <- FALSE
    if (is.character(duracion)) {
      duracion_anclada <- TRUE
      
      # 'Chicken and egg' situation: the value to use as default for
      # 'start' depends on 'duracion', but if 'duracion' is specified as character
      # I need 'start' to calculate it.
      #   ==> for character 'duracion's, I use a default of 0 for 'start'
      start <- ifelse(is.null(start), 0, start) + xovr_slide
      
      fecha_inicial <- (dias %>% filter(IxDia == alarm_env$res$Ix_base + start) %>% select(date))[[1]]
      fecha_final <- devolver_fecha_de_duracion(str_to_lower(duracion), fecha_inicial, start)
      un_solo_dia <- (fecha_final == fecha_inicial)
    } else {
      # "duracion = 0" doesn't make much sense. If I want a series of just two days,
      # starting today, I would need "duracion = 2, start = 1". So a series of one 
      # day, starting today should be "duracion = 2, start = 1". 
      
      # Because of the mistaken initial decision to make "duracion = 0" the default,
      # and because even though it is wrong, it still seems intuitive, I allow it
      # and I silently change it to 1 when I receive it as 0. 
      if (duracion == 0) duracion = 1
      
      
      un_solo_dia <- (duracion == 1)
      if (is.null(start)) {
        # Set default value if needed
        start <- ifelse(un_solo_dia, 0, 1) + xovr_slide
      } else {
        start <- start + xovr_slide
      }
    }
    fecha_start <- (dias %>% filter(IxDia == alarm_env$res$Ix_base + start) %>% select(date))[[1]]
    
    
    datos_uno <- dtb[.(ticker_, serie), nomatch = 0] %>% filter(date <= fecha_start)
    if (nrow(datos_uno) == 0) {
      stop('Faltan datos para ', serie, '(', ticker_, ')  (no hay nada)', call. = FALSE)
    }
    
    # if (exists('usarDT') && usarDT) {
    if (!duracion_anclada) {
      datos_uno <- datos_uno %>% 
        filter(complete.cases(.)) %>% 
        arrange(desc(date)) %>% 
        slice(1:duracion) %>%                #    <<<-- 'duracion' selection of records
        tbl_df
      
    } else {
      datos_uno <- datos_uno %>% 
        spread(variable, value, fill = NA) %>% 
        filter(complete.cases(.)) %>% 
        arrange(desc(date)) %>% 
        filter(date >= fecha_final) %>%    #    <<<-- fixed date selection of records
        tbl_df
    }
    
    if(ver_serie > alarm_env$res$paso_crossover) {
      # This is just for debugging
      print(datos_uno, n = nrow(datos_uno))
    }
    
    # if (post_proceso %in% c('ultimo', 'primero')) {
    #   fecha_end <- (dias %>% filter(IxDia == start + duracion - 1) %>% select(date))[[1]]
    #   datos_uno <- datos_uno %>% filter(date >= fecha_end)
    # }
    
    if (nrow(datos_uno) == 0) {
      if (post_proceso %in% c('ultimo', 'primero')) {
        return(ND)
      } else {
        stop('Faltan datos para ', ticker_, '(todos)', call. = FALSE)
      }
    } 
    
    if ( (datos_uno %>% select(date) %>% slice(1L))[[1]] < fecha_start) {
      if (un_solo_dia) {
        if (post_proceso %in% c('ultimo', 'primero')) {
          return(ND)
        } else {
          stop('Faltan datos para ', ticker_, ' (start)', call. = FALSE)
        }
      } else {
        # There are no more NAs, can safely return values
        if (post_proceso =='ultimo') {
          return( (datos_uno %>% select(4) %>% slice(1L))[[1]] )
        } else if (post_proceso == 'primero') {
          return( (datos_uno %>% select(4) %>% slice(n()))[[1]])
        } else {
          stop('Faltan datos para ', ticker_, ' (start)', call. = FALSE)
        }
      }
      
    }
    
    # 'start' records exists
    if (un_solo_dia) { # Si es un valor puntual, ya lo puedo devolver...
      resultado <- (datos_uno %>% select(4) %>% slice(1L))[[1]]
      return( ifelse(is.na(resultado), ND, resultado) )
      
    } else if (nrow(datos_uno) < duracion) { # Si es más de un día, me fijo si tengo suficientes
      if (!post_proceso %in% c('ultimo', 'primero')) {                   # registros...
        stop('Faltan datos para ', ticker_, '(cantidad)', call. = FALSE)
      }
    }
    
    # If I got here, it means that:
    #    1) duracion > 1
    #    2) Data is OK
    # Just post-process and return
    v_serie <- (datos_uno %>% select(4))[[1]]
    
    if (post_proceso == 'percentil') {
      return(quantile(v_serie, p))
    } else if (post_proceso == 'media') {
      return(mean(v_serie))
    } else if (post_proceso == 'suma'){ 
      return(sum(v_serie))
    } else if (post_proceso == 'max'){ 
      return(max(v_serie))
    } else if (post_proceso == 'min'){ 
      return(min(v_serie))
    } else if (post_proceso == 'signo_suma'){ 
      return(sign(sum(v_serie)))
    } else if (post_proceso == '%pos') {
      return(sum(v_serie > 0) / length(v_serie))
    } else if (post_proceso == '%neg') {
      return(sum(v_serie < 0) / length(v_serie))
    } else if (post_proceso == 'ultimo') {
      return(v_serie[1])
    } else if (post_proceso == 'primero') {
      return(v_serie[length(v_serie)])
    } else
      stop('post_proceso: ', ticker_, ' desconocido.', call. = FALSE)
  }
}

informar_resultados <- function() { #, importancia, paso_crossover, permanente
  ### informar_resultados
  
  # Retoque de campos para JIRA (emprolijar)
  if (is.na(alarm_env$res$alarma)) {
    alarm_env$res$alarma <- TRUE
    if (is.null(alarm_env$res$es_pnl) || is.na(alarm_env$res$es_pnl)) {
      alarm_env$res$importancia <- alarm_env$res$importancia_potencial
    } else {
      # On 'lack of data' errors, pnl is serious, other cases not so serious...
      alarm_env$res$importancia_potencial <- ifelse(alarm_env$res$es_pnl, -5, -1)
      # Remove alarm_env$res$es_pnl so it doesn't break preparar_salida later on
      alarm_env$res$es_pnl <- NULL
    }
    
    alarm_env$res$mensaje_corto <- 'ERROR AL CORRER LA ALARMA'
    if (is.null(alarm_env$res$mensaje)) alarm_env$res$mensaje <- 'ERROR AL CORRER LA ALARMA'
  } else {
  }
  
  if (alarm_env$res$paso_crossover == 0) { # correr_alarma fue llamada directamente
    salida_pantalla <- preparar_salida(alarm_env$res, archivo_salida, silencioso = FALSE)
    cat(salida_pantalla, '\n\n')
  } else { # correr_alarma fue llamada a través de crossover
    return(alarm_env$res)
  }
}


preparar_salida <- function(lista_res, archivo = NULL, silencioso = FALSE) {
  ### preparar_salida
  
  # Recibe una lista de resultados (OJO: puede no ser directamente
  # alarm_env$res, por ej. en el caso de x_over) y arma las diferentes salidas
  # de Alarmas (JIRA, pantalla, listado 'long' de resultados)
  
  dic_alarmas <- 
    data_frame(importancia = 0L:5L, 
               Prioridad = c('Trivial', 'Trivial', 'Menor', 'Mayor', 'Crítica', 'Bloqueadora'))
  
  alarm_env$contar_alarma(lista_res$importancia)
  
  lista_res$id_alarma <- alarm_env$get_next_alarm_id()
  lista_res$mensaje <- str_replace(lista_res$mensaje, 'Error: Falta', 'Falta')
  
  # In corrida_alarmas we can save last id used at the end, in all other cases save it here.
  if(any(str_detect(sapply(sys.calls(), function(x) as.character(x)[1]), 'corrida_alarmas'))) {
    cat(lista_res$id_alarma, file="id_alarmas_file.txt", sep = "\n")
  }
  
  if (is.null(lista_res$X)) lista_res$X <- -99
  
  lista_res$flias <- paste(lista_res$flias, collapse = '|')
  nombres_variables <- as_data_frame(lista_res$variables)
  nombres_variables <- gather(nombres_variables, x_name, x_value)
  
  if (nrow(nombres_variables) > 0) {
    for (i in seq.int(1, nrow(nombres_variables))) {
      lista_res$expr_txt <- str_replace_all(lista_res$expr_txt, 
                                      nombres_variables$x_name[i],
                                      nombres_variables$x_value[i])
      lista_res$izq_text <- str_replace_all(lista_res$izq_text, 
                                      nombres_variables$x_name[i],
                                      nombres_variables$x_value[i])
      lista_res$der_text <- str_replace_all(lista_res$der_text, 
                                      nombres_variables$x_name[i],
                                      nombres_variables$x_value[i])
    }
  }
  # lista_res$variables no debe ser una lista, para el as.data.frame que sigue
  lista_res$variables <- paste(lista_res$variables, collapse = '|')
  
  lista_res <- eliminar_expresiones(lista_res)
  s0 <- as.data.frame(lista_res, stringsAsFactors = FALSE) %>% 
    mutate(fecha_corrida = Sys.Date())
  
  s0 <- s0 %>% left_join(dic_alarmas, by = 'importancia') %>% 
    mutate(Prioridad = ifelse(!is.na(Prioridad), Prioridad, as.character(importancia)))
  
  s0 <- s0 %>% rename(fecha_alarma = fecha_base, cartera_precio = flias,
                      nombre = tipo, resumen = mensaje_corto)
  
  # names(s0) <- c('expr', 'fecha_alarma', 'cartera_precio', 'eval', 'result_parciales',
  #                'misma_formula', 'alarma', 'importancia', 'nombre', 'resumen',
  #                'mensaje', 'id_alarma', 'fecha_corrida', 'Prioridad')
  
  # ToDo: por ahora simulo salida de la primer versión, generar mejores salidas
  s0 <- s0 %>% select(expr = expr_txt, fecha_alarma, cartera_precio, eval, # result_parciales, misma_formula, 
                      alarma, importancia, nombre, resumen, 
                      mensaje, id_alarma, X, fecha_corrida, Prioridad)
  
  s_jira <- s0 %>% mutate(proyecto = 'ALM', 
                          fecha_corrida = format(fecha_corrida, '%d/%m/%Y'),
                          fecha_alarma  = format(fecha_alarma,  '%d/%m/%Y')) %>% 
    select(id_alarma, fecha_corrida, fecha_alarma, nombre, cartera_precio, 
           resumen, importancia, Prioridad, 'Project Key' = proyecto, free_text = mensaje, 
           'Software indicador de Señal' = eval, X)
  
  s_long <- s0 %>% gather(dato, valor, -c(id_alarma, fecha_alarma, fecha_corrida)) %>% 
    select(id_alarma, fecha_alarma, fecha_corrida, dato, valor) %>% 
    filter(dato != 'misma_formula')
  
  if (archivo != '') {
    if(!str_detect(archivo, '\\.csv')) sep = '\t' else sep = ', '
    
    if (lista_res$alarma) {
      archivo_jira <- str_replace(archivo, '\\.', '_jira.')
      suppressWarnings(
        write.table(s_jira, file = archivo_jira, 
                    append = TRUE, quote = FALSE, sep = sep,
                    row.names = FALSE, col.names = (!file.exists(archivo_jira)),
                    fileEncoding = 'UTF-8')
      )
      
      if(as.character(sys.calls()[[1]])[1] == 'corrida_alarmas') {
        nombre_backup = sys.frames()[[1]]$nombre_backup
        
        try({
          suppressWarnings(
            write.table(s_jira, file = nombre_backup, 
                        append = TRUE, quote = FALSE, sep = sep,
                        row.names = FALSE, col.names = (!file.exists(nombre_backup)),
                        fileEncoding = 'UTF-8')
          )
          
        })
      }
    }
    
    suppressWarnings(
      write.table(s_long, file = archivo, 
                  append = TRUE, quote = FALSE,  sep = sep,
                  row.names = FALSE, col.names = (!file.exists(archivo)))
    )
  }
  
  if (silencioso){
    return('')
  } else {
    # Don't show fecha_base if it is the same as 'last_day'
    if (lista_res$fecha_base == alarm_env$max_fecha_datos)
      lista_res$fecha_base <- NULL
    else
      lista_res$fecha_base <- paste('fecha_base =', format(fecha_base, "%d/%m/%Y"))
    
    lista_res$mensaje <- str_replace_all(lista_res$mensaje, '\n*$', '')
    
    # ToDo: por ahora simulo salida de la primer versión, generar mejores salidas
    res1 <- list(expr = lista_res$expr_txt, lista_res$fecha_base, lista_res$flias, 
                 lista_res$eval, # res$result_parciales,
                 lista_res$alarma, lista_res$importancia, lista_res$tipo, lista_res$mensaje_corto, lista_res$mensaje, 
                 lista_res$id_alarma, lista_res$X)
    if (is.null(res1[[2]])) res1[2] <- NULL
    
    salida <- paste(res1, collapse = "\n")
    
    return(salida)
  }
}

redondear_dif <- function(lista_res) {
  ### redondear_dif
  
  fact_redondeo <- get_factor_redondeo(lista_res$izq_valor, lista_res$der_valor)
  
  lista_res$izq_valor <- round(lista_res$izq_valor, fact_redondeo)
  lista_res$der_valor <- round(lista_res$der_valor, fact_redondeo)
  
  lista_res
}

get_factor_redondeo <- function(v1, v2) {
  ### get_factor_redondeo
  stopifnot(is.numeric(v1), is.numeric(v2))
  
  dif_entre_resultados <- abs(v1 - v2)
  
  if (dif_entre_resultados >= 1) {
    if (pmin(v1, v2) >= 100) {
      redondeo = 1
    } else redondeo = 2
  } else redondeo = pmin(ceiling(-log10(dif_entre_resultados)), 7)
  
  redondeo
}

calc_Ix_base <- function() {
  ### calc_Ix_base
  
  # If called from corrida_alarmas, Ix_base already exists, but the actual calls
  # to this function will be made via source(). As the calculation of Ix_base is
  # slow, I get() it from corrida_alarmas() in a convoluted way because of the
  # source() calls...
  if(any(str_detect(sapply(sys.calls(), function(x) as.character(x)[1]), 'corrida_alarmas'))) {
    Ix_base <- get('Ix_base', 
                   sys.frame(which(str_detect(
                     sapply(sys.calls(), 
                            function(x) as.character(x)[1]), 'corrida_alarmas'))))
    
  } else {
    # If not invoked via corrida_alarmas(), calculate it here...
    Ix_base <- (dias %>% filter(date == fecha_base) %>% select(IxDia))[[1]]
    if (length(Ix_base) == 0){
      print(paste('Sin datos para el dìa ', fecha_base))
    }
    Ix_base
  }
  
}

para_imprimir <- function(x, nivel = 0) {
  ### para_imprimir
  
  if (nivel == 0) {
    b <- expr_text(x)
  } else {
    b <- expr_find(x)[nivel]
  }
  x <- str_replace_all(as.character(b), '\"', "'")
  x <- paste0(x, collapse = '')
  x <- str_replace_all(x, '\n', '')
  x <- str_replace_all(x, ' +', ' ')
  x <- str_replace_all(x, '\\{ ', '{')
  x
}

#### test_expresion_simple ------------------------------------------------------------------
# Sólo se usa para testear transformar_expresion()
test_expresion_simple <- function(alarma) {
  ### test_expresion_simple  Sólo para debugging de expresiones
  expr <- expr_find(alarma)
  
  expr_procesada <- transformar_expresion(expr)
  
  if (dbg()) print(as.character(expr_procesada))

  valor_ult <- eval(expr_procesada) %>% unname

  #res <- get('res', parent.frame())
  # NOTA: Side-effect: estoy modificando la list 'res'
  
  alarm_env$res$valor_ult <- valor_ult
  #assign('res', res, parent.frame())

  if (dbg()) print(paste('valor = ', alarm_env$res$valor_ult))
  return(invisible(NULL))
}

eliminar_expresiones <- function(lista_res) {
  ### eliminar_expresiones
  
  # Después de calcular la alarma, varios componentes de res contienen
  # expresiones (o calls). Las paso a texto para que no se dispare una
  # evaluación no deseada al manipular la lista.
  lista_res <- lapply(lista_res, function(x) if (is.language(x)) {
    x <- paste0(str_trim(as.character(x)), sep = ' ', collapse = '')
  } else x)
}
