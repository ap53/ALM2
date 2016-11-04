### ALM_4_procesos.R

x_over <- function(..., permanente = 0) {
  chk_hoy <- c_a(..., paso_crossover = 1)
  
  chk_ayer <- c_a(..., paso_crossover = 2)
  
  hay_error <- 0
  if (str_detect(chk_hoy$mensaje_corto, 'ERROR') || str_detect(chk_ayer$mensaje_corto, 'ERROR')) {
    hay_error <- 1
    chk_hoy$X <- 1 # Flag que significa: se prendió la alarma
  } 
  
  if (  !chk_hoy$alarma & !chk_ayer$alarma){
    # No se disparó ni ayer ni hoy, mando resultados de hoy
    if (!hay_error) {
      chk_hoy$mensaje_corto <- 'No hay crossover (alarma apagada)'
      chk_hoy$X <- -99 # No se va a mandar a ningún lado, pero le pongo algo por las dudas
    }
    
  } else if (chk_hoy$alarma & !chk_ayer$alarma){
    # No se disparó ayer pero hoy sí: se prendió la alarma, hay crossover (+1)
    if (!hay_error) chk_hoy$mensaje_corto <- paste0('Se prendió la alarma: "', chk_hoy$mensaje_corto, '"')
    chk_hoy$eval <- paste(chk_ayer$eval, '|', chk_hoy$eval)
    chk_hoy$X <- 1 # Flag que significa: se prendió la alarma  (mando el +1 aunque haya error)
    
  } else if (!chk_hoy$alarma & chk_ayer$alarma){
    # Se disparó ayer pero hoy no: se apagó la alarma, hay crossover (-1)
    # Apago la alarma y aviso que no hay crossover
    if (!hay_error) {
      chk_hoy$alarma <- TRUE
      chk_hoy$importancia <- chk_ayer$importancia
      chk_hoy$mensaje_corto <- paste0('Se apagó la alarma: "', chk_ayer$mensaje_corto, '"')
      chk_hoy$mensaje <- paste0('Se apagó la alarma: "', chk_ayer$mensaje, '"')
      chk_hoy$X <- -1 # Flag que significa: se apagó la alarma
    }
    chk_hoy$eval <- paste(chk_ayer$eval, '|', chk_hoy$eval)
    
  } else if ( chk_hoy$alarma &  chk_ayer$alarma){
    # Se disparó ambas veces: no hay crossover, pero si piden 'permanente' lo informo igual
    if (!hay_error) {
      chk_hoy$mensaje_corto <- paste0(chk_hoy$mensaje_corto, ': no es crossover')
      if (!permanente) {
        # Apago la alarma
        chk_hoy$alarma <- FALSE
        chk_hoy$importancia <- 0
        chk_hoy$mensaje_corto <- paste0(chk_hoy$mensaje_corto, ': no se disparó.')
        chk_hoy$X <- -99 # No se va a mandar a ningún lado, pero le pongo algo por las dudas
      } else {
        chk_hoy$X <- 0 # Flag que significa: sigue prendida la alarma
      }
    }
  }
  
  salida_pantalla <- preparar_salida(chk_hoy, archivo_salida)
  cat(salida_pantalla, '\n\n')
}

corrida_alarmas <- function(filename, fecha_inicial = NULL, fecha_final = fecha_inicial) {
  ptm <- proc.time()
  
  # # Calls to cambiar_fecha_base are ignored during a corrida_alarmas,
  # # fecha_base is controlled by a for loop
  # #     ==> reset fecha_base to "pnl_last_day" on exit.
  on.exit(fecha_base <<- alarm_env$max_fecha_datos)
  
  if (is.null(fecha_inicial)) {
    fecha_inicial <- fecha_base
  } else {
    # Not really setting fecha_base, just taking advantage of date validation
    fecha_inicial <- cambiar_fecha_base(fecha_inicial)
  }
  
  
  
  nombre_backup <- paste0(path, 'Backups_Alarmas//Alarmas ', str_replace_all(Sys.time(), ':', ''), '.csv')
  
  # Al no haber sido evaluada fecha_final hasta ahora, el default del argumento 
  # hace que, si no había sido especificada en el llamado, fecha_final YA TIENE el
  # mismo valor que fecha_inicial.
  if (fecha_final != fecha_inicial) {
    # Not really setting fecha_base, just taking advantage of date validation
    fecha_final <- cambiar_fecha_base(fecha_final)
  }
  
  
  
  by <- ifelse(fecha_inicial > fecha_final, -1, 1)
  
  alarm_env$init_cant_alarmas()
  
  if (str_detect(filename, '\\.R$')) {
    fechas <- seq(fecha_inicial, fecha_final, by = by)
    for (ix in seq_along(fechas)) {
      fecha_base <<- fechas[ix]
      
      
      
      if(!wday(fecha_base) %in% c(1, 7)) {
        Ix_base <- (dias %>% filter(date == fecha_base) %>% select(IxDia))[[1]]
        if (length(Ix_base) == 0){
          print(paste('Sin datos para el día ', fecha_base))
          next()
        }
        ######################################################
        source(filename, print.eval = TRUE, encoding = 'utf-8')
        ######################################################
      }
    }
  } else if (str_detect(filename, '\\.yaml$')) {
    print('Todavía no está...')
  } else stop("Sólo se pueden procesar .R o .yaml")
  
  print(alarm_env$ver_cuantas_corrieron())
  
  # Save id_alarma to file
  id_alarma <- alarm_env$id_alarma
  cat(id_alarma,file = "id_alarmas_file.txt",sep = "\n")
  
  if (sum(alarm_env$cant_alarmas) <= 1) {verbo = 'Tardó: '} else {verbo = 'Tardaron: '}
  cat(verbo, (proc.time() - ptm)[3], ' segundos...\n')
}

#### Comparar archivos ----------------------------------------
comparar_familias <- function(fech_fija, path_bkp, fecha_tope = as.Date('2012-01-01')) {
  tipo_arch = 'familia'
  
  # Extraer lista de nombres de archivos del tipo deseado (tipo_arch, fecha_tope)
  #   Ordenarlos por fecha *descendiente*
  lista_arch <- list.files(path_bkp, pattern = tipo_arch)
  
  # Verificar que exista el de la fecha base solicitada
  archivo_base <- paste0(format(fech_fija, "%Y-%m-%d"), '.', tipo_arch, '.RDS')
  if (!(archivo_base %in% lista_arch))
    stop('No existe', archivo_base, 'en la carpeta', path_bkp, sep = ' ')
  
  
  # Eliminar de la lista las más recientes que esta fecha base
  lista_arch <- lista_arch[which(ymd(str_sub(lista_arch, 1, 10)) < fech_fija & ymd(str_sub(lista_arch, 1, 10)) >= fecha_tope)] %>% 
    sort(decreasing = TRUE)
  
  # Leer datos base
  dt_base <- readRDS(paste0(path_bkp, archivo_base))
  
  # Son archivos grandes, data.table conviene
  if (!inherits(dt_base, 'data.table')) dt_base <- as.data.table(dt_base )
  
  # Eliminar columnas innecesarias
  dt_base <- dt_base[, list(Descripcion, CarteraNom)]
  
  # Comienzo acumulación
  dta <- dt_base %>% mutate(Fecha = fech_fija, Existe = 'X')
  
  # Loop sobre los anteriores a la fecha base solicitada
  for (arch in lista_arch) {
    # Extraer fecha loop
    fecha_loop <- str_sub(arch, 1, 10) %>% ymd()
    print(fecha_loop)
    
    # Leer el target
    dt_loop <- readRDS(paste0(path_bkp, arch))
    if (dim(dt_loop)[2] == 0) {
      print(paste0('Problemas con el archivo: ', path_bkp, arch))
      next()
    }
    
    if (!inherits(dt_loop, 'data.table')) dt_loop <- as.data.table(dt_loop )
    
    # Eliminar columnas innecesarias
    dt_loop <- dt_loop[, list(Descripcion, CarteraNom)] %>% mutate(Fecha = fecha_loop, Existe = 'X')
    
    # Acumulo
    dta <- rbindlist(list(dta, dt_loop), use.names=TRUE, fill=TRUE)
    
  }
  
  ult_fecha <- fecha_loop
  
  dta <- dta %>% rename(Familia = Descripcion, Cartera = CarteraNom)
  # dta <- dta %>% filter(Fecha < '2016-09-15'| Fecha > '2016-09-20' | Familia != 'VIX' | Cartera != 'T-VIX')
  
  combinaciones <- dta %>% expand(nesting(Familia, Cartera), Fecha)
  dta <- dta %>% right_join(combinaciones, by = c("Familia", "Cartera", "Fecha")) %>% 
    arrange(Familia, Cartera, desc(Fecha)) %>% 
    group_by(Familia, Cartera) %>% mutate(Previo = lead(Existe)) %>% ungroup
  
  
  dta <- dta %>% filter(coalesce(Existe, '0') != coalesce(Previo, '0')) %>% 
    arrange(Familia, Cartera, desc(Fecha)) %>%  group_by(Familia, Cartera)
  
  dta <- dta %>% mutate(FchPrevia = lead(Fecha), DespuesDe_Dias = Fecha - FchPrevia) %>% 
    select(Familia, Cartera, Fecha, Existe, DespuesDe_Dias) %>% 
    mutate(Existe = coalesce(Existe, ''), DespuesDe_Dias = coalesce(fech_fija - Fecha))
  
  # Informar diferencias
  cant_sin_cambios <- dta %>% filter(Fecha == ult_fecha) %>% nrow()
  print(paste('TOTAL: ', cant_sin_cambios, 'sin cambios y', nrow(dta) - cant_sin_cambios, 'diferencias encontradas.'))
  write.csv(dta, file = paste(str_replace_all(today(), '-', ''), 'Cambios Familias.csv'), row.names = FALSE)
  
  return(dta)
}

comparar_archivos <- function(tipo_arch, path_bkp, fecha_inic = fecha_base, 
                              fecha_tope = as.Date('2012-01-01'),
                              lista_fechas = NULL,   # c(ymd(20161005), ymd(20160915))
                              excluir_primer_aparicion = TRUE, 
                              tolerancia_dias = 0, # >0 se queda con DespuesDe_Dias > tol, < 0 se queda con los cercanos
                              tolerancia_dif = 'baja', # cualquier otro string es 'alta'
                              eliminar_arch_intermedios = TRUE,
                              dbg = 0) {
  fh_inicio <- Sys.time()
  fh_interm <- fh_inicio
  primera_mostrada_tiempos <- TRUE
  
  mostrar_durac <- function(dur) {
    unidades <- attr(dur, 'units')
    if (unidades == 'secs'){
      unidades <- 'segundos'
    } else if (unidades == 'mins'){
      unidades <- 'minutos'
    } else if (unidades == 'hours'){
      unidades <- 'horas'
    } # else queda como está...
    paste(round(dur, 1), unidades)
  }
  mostrar_tiempos <- function(txt) {
    ahora <- Sys.time()
    if (primera_mostrada_tiempos) {
      cat('Inicio comparación de', arch, 'a las', format(Sys.time(), '%H:%M\n'))
      primera_mostrada_tiempos <<- FALSE
    } else {
      cat(txt, 'a las', format(ahora, '%H:%M'), 'tardó:', mostrar_durac(ahora - fh_interm), 
          ', acumulado: ', mostrar_durac(ahora - fh_inicio), '\n')
    }
    fh_interm <<- ahora
  }
  
  mostrar_tiempos()
  
  if (dbg >= 1) browser()
  # Para cada tipo_arch especifico:
  # v_fecha: si hay alguna columna que sea "variable" (en un sentido "tidyr")
  # y represente una "fecha de contrato". Un ejemplo es Fecha en pnl. 
  # El PatFamilia de una "fecha de contrato" a la siguente puede variar, 
  # pero para una "fecha de contrato" dada, debería ser constante de un 
  # archivo al siguiente. Hay que aclarar cual es esta "fecha de contrato"
  # para estar seguros de que R la vea como fecha y poder hacer cálculos
  # de diferencias de fecha.
  # v_variables: son las columnas "variables" (en el sentido "tidyr"), o sea
  # las que forman la clave sobre la que vamos a buscar diferencias. Si hay 
  # una 'v_fecha', debe formar parte de este vector de variables.
  # v_observaciones: son aquellas columnas que vamos a pasar de wide a long.
  # Por ejemplo, en pnl vamos a buscar por separado las diferencias de 
  # 'PatFamilia' y las de 'RentDiaria' por lo las bajamos a una columna '.variable'.
  
  # Por ahora especifico todo en la función: creo que van a ser estos tres archivos
  # nada más: si aparecen varios más, pasar esto a un archivo de configuración.
  if (tipo_arch == 'familia') {
    v_fecha <- ''
    # OJO: más abajo las renombro "c('Familia', 'Cartera')"
    v_variables <- c('Descripcion', 'CarteraNom')
    v_observaciones <- c()
  } else if (tipo_arch == 'pnl') {
    v_fecha <- 'Fecha'
    v_variables <- c('Fecha', 'NombreFamilia')
    v_observaciones <- c('PatFamilia', 'RentDiaria', 'Suscripciones', 'Rescates')
  } else if (tipo_arch == 'bulk') {
    v_fecha <- 'Periodo' # OJO: más abajo lo cambio a 'Fecha'
    v_variables <- c('Fecha', 'CarteraNom', 'Especie')
    v_observaciones <- c('Cantidad', 'Valuacion', 'Resultado')
  }
  v_todas <- c(v_variables, v_observaciones)
  
      # Extraer lista de nombres de archivos del tipo deseado (tipo_arch, fecha_tope)
    #   Ordenarlos por fecha *descendiente*
    lista_arch <- list.files(path_bkp, pattern = tipo_arch) %>% sort(decreasing = TRUE)
    
    if (!is.null(lista_fechas)) {
      if (class(lista_fechas) != 'Date') {
        stop('Las fechas que se pasan en lista_fechas deben ser un vector de "Dates".\n Ej: "c(ymd(20161005), ymd(20160915))"')
      }
      listados <- paste0(format(lista_fechas, "%Y-%m-%d"), '.', tipo_arch, '.RDS')
      
      lista_arch <- intersect(lista_arch, listados) %>% sort(decreasing = TRUE)
      if (length(lista_arch) != length(listados)) {
        stop('No se encontraron todos los archivos especificados en lista_fechas.')
      }
      archivo_base <- lista_arch[1]
      fecha_inic <- ymd(str_sub(archivo_base, 1, 10))
    } else {
      
      # Verificar que exista el de la fecha base solicitada
      archivo_base <- paste0(format(fecha_inic, "%Y-%m-%d"), '.', tipo_arch, '.RDS')
      if (!(archivo_base %in% lista_arch)){
        fecha_inic <- str_sub(lista_arch[1], 1, 10) %>% ymd()
        print(paste('OJO: No existe', archivo_base, 'en la carpeta', path_bkp, ' ==> fecha_inic = ', format(fecha_inic, '%Y-%m-%d')))
        
        archivo_base <- paste0(format(fecha_inic, "%Y-%m-%d"), '.', tipo_arch, '.RDS')
      }
    }
  
  # La cantidad de días que han pasado entre la "fecha de contrato" y el día en que
  # se detecta la diferencia tiene más sentido si se expresa en días de semana: es
  # lo mismo una diferencia entre martes y jueves que una entre jueves y lunes.
  # Preparo un Calendario para contar dias de semana(o sea los laborables)
  calendario_dias_semana <- data.table(fecha = fecha_inic - seq(0, as.numeric(fecha_inic - fecha_tope + 1)) )
  calendario_dias_semana <- calendario_dias_semana[, dia_semana := wday(fecha)][dia_semana %in% 2:6]
  setkey(calendario_dias_semana, fecha)
  calendario_dias_semana <- calendario_dias_semana[, ix := seq_len(.N)]
  
  # Eliminar de la lista las más recientes (o igual) que la fecha fija  
  lista_arch <- lista_arch[which(ymd(str_sub(lista_arch, 1, 10)) < fecha_inic & 
                                   ymd(str_sub(lista_arch, 1, 10)) >= fecha_tope)] %>% 
    sort(decreasing = TRUE)
  
  dt_base <- readRDS(paste0(path_bkp, archivo_base))
  # Son archivos grandes, data.table es más conveniente que dplyr
  if (!inherits(dt_base, 'data.table')) dt_base <- as.data.table(dt_base )
  
  # Si hay una v_fecha, me aseguro de que se llame 'Fecha' para no
  # complicarme la vida cuando haga las comparaciones
  if (v_fecha != '' && v_fecha != 'Fecha') {
    dt_base <- dt_base %>% rename_('Fecha' = v_fecha)
  }
  
  # Me aseguro de que tenga tipo Date
  if (v_fecha != '' && 'Fecha' %in% names(dt_base)) {
    if (class(dt_base[['Fecha']]) == 'character') {
      dt_base[['Fecha']] <- 
        ymd(str_sub(dt_base[['Fecha']], 1, str_locate(dt_base[['Fecha']], ' ')[1] - 1))
    }
    
    dt_base <- dt_base[Fecha >= fecha_tope]
  }
  
  if (dbg >= 2) browser()
  # Eliminar columnas innecesarias (me quedo con las v_variables y con las v_observaciones)
  # El resto vuela.
  dt_base <- data.table(as.data.frame(dt_base)[, get('v_todas')]) #[order(-Fecha)]
  
  
  # Comienzo acumulación
  dta <- dt_base %>% mutate(Fecha_arch = fecha_inic, Existe = 'X')
  
  # Loop sobre los archivos anteriores a la fecha base fija
  for (arch in lista_arch) {
    # Extraer fecha loop
    fecha_loop <- str_sub(arch, 1, 10) %>% ymd()
    print(fecha_loop)
    
    # Leer el target
    dt_loop <- readRDS(paste0(path_bkp, arch))
    if (dim(dt_loop)[2] == 0) {
      print(paste0('Problemas con el archivo: ', path_bkp, arch))
      next()
    }
    
    if (v_fecha != '' && v_fecha != 'Fecha') {
      dt_loop <- dt_loop %>% rename_('Fecha' = v_fecha)
    }
    
    if (v_fecha != '' && 'Fecha' %in% names(dt_loop)) {
      if (class(dt_loop[['Fecha']]) == 'character') {
        dt_loop[['Fecha']] <- 
          ymd(str_sub(dt_loop[['Fecha']], 1, str_locate(dt_loop[['Fecha']], ' ')[1] - 1))
      }
      dt_loop <- dt_loop[Fecha >= fecha_tope]
    }
    
    if (!inherits(dt_loop, 'data.table')) dt_loop <- as.data.table(dt_loop )
    
    # Eliminar columnas innecesarias
    dt_loop <- data.table(as.data.frame(dt_loop)[, get('v_todas')]) %>% 
      mutate(Fecha_arch = fecha_loop, Existe = 'X')
    
    # Acumulo
    if (dbg >= 3) browser()
    dta <- rbindlist(list(dta, dt_loop), use.names=TRUE, fill=TRUE)
    
  }
  
  ult_fecha <- fecha_loop
  mostrar_tiempos('Archivos leidos')
  
  if (dbg >= 2)  browser()
  
  if (tipo_arch == 'familia') { # Por ahora es el caso de 'familia'
    dtb <- dta %>% rename(.Variable = Existe) %>% mutate(.Valor = 1) %>% 
      rename(Familia = Descripcion, Cartera = CarteraNom) 
    
    v_variables <- c('Familia', 'Cartera')
    v_todas <- c(v_variables, v_observaciones)
    
    combinaciones <- dtb %>% expand(nesting(Familia, Cartera, .Variable), Fecha_arch)
  } else if (tipo_arch == 'pnl') {
    dta <- dta[!(is.na(Fecha) | is.na(NombreFamilia))] %>% filter(Fecha >= fecha_tope)
    
    dtb <- dta %>% select(-Existe) %>% 
      gather_('.Variable', '.Valor', v_observaciones, convert = TRUE)
    
    combinaciones <- dtb %>% expand(nesting(Fecha, NombreFamilia, .Variable ), Fecha_arch)
  } else if (tipo_arch == 'bulk') {
    dta <- dta[!(is.na(Fecha) | is.na(CarteraNom))] %>% filter(Fecha >= fecha_tope)
    
    dtb <- dta %>% select(-Existe) %>% 
      gather_('.Variable', '.Valor', v_observaciones, convert = TRUE)
    
    combinaciones <- dtb %>% expand(nesting(Fecha, CarteraNom, .Variable ), Fecha_arch)
  }
  
  if (eliminar_arch_intermedios) rm(dta)
  
  vector_of_vars <- c(v_variables, '.Variable')
  # Convert character vector to list of symbols
  dots <- lapply(vector_of_vars, as.symbol)
  
  vector_of_vars_arrange <- c(vector_of_vars, 'desc(Fecha_arch)')

  
  # if (dbg >= 1) browser()
  # combi_2 <- dtb %>% expand_(nesting_(dots), 'Fecha_arch')

  dtc <- dtb %>% right_join(combinaciones) %>% 
    group_by_(.dots = dots) %>%  
    arrange_(.dots = vector_of_vars_arrange)
  
  dtc <- dtc %>% mutate(Previo = lead(.Valor)) %>% ungroup
  if (v_fecha != '') {
    dtc <- dtc %>% filter(Fecha_arch > Fecha)
  }
  
  mostrar_tiempos('Archivos expandidos')
  
  dtd <- dtc %>% 
    filter(dif_significativa(coalesce(.Valor, -987987654.321), 
                             coalesce(Previo, -987987654.321),
                             tolerancia_dif))
  
  
  if (eliminar_arch_intermedios) rm(dtc, combinaciones)
  mostrar_tiempos('Comparaciones')
  
  # Convert character vector to list of symbols
  dots <- lapply(vector_of_vars, as.symbol)
  dtd <- dtd %>% arrange_(.dots = vector_of_vars_arrange) %>% 
    ungroup %>% group_by_(.dots = dots)  # , .Variable
  
  mostrar_tiempos('Dataframe ordenado')
  
  if (v_fecha == '') {
    dth <- dtd %>% mutate('FchPrevia' = lead(Fecha_arch), DespuesDe_Dias = Fecha_arch - FchPrevia)
    
    mostrar_tiempos('FchPrevia generada')
    
  } else { 
    if (dbg >= 1) browser()
    fechas <- dtd %>% ungroup %>% select(Fecha_arch) %>% distinct(Fecha_arch)
    fechas <- fechas %>% 
      left_join(calendario_dias_semana, by = c('Fecha_arch' = 'fecha')) %>% 
      mutate(ix = ix)
             
    dtd <- dtd %>% ungroup %>% left_join(fechas, by = 'Fecha_arch') %>% rename(DespuesDe_Dias = ix)

    
    fechas <- dtd %>% select(Fecha) %>% distinct(Fecha)
    fechas <- fechas %>% 
      left_join(calendario_dias_semana, by = c('Fecha' = 'fecha')) %>% 
      mutate(ix = ix)
    dtg <- dtd %>% left_join(fechas, by = 'Fecha') %>% mutate(DespuesDe_Dias = DespuesDe_Dias - ix)
    
    # dtg <- dtd %>% mutate(DespuesDe_Dias =
    #                         calendario_dias_semana[.(Fecha_arch), ix] -
    #                         calendario_dias_semana[.(Fecha), ix])
    
    if (eliminar_arch_intermedios) rm(dtd, fechas)
    
    # Algunos archivos se generan un sábado y dan NA porque no existen en
    # calendario: para este cálculo asumo que se generaron el viernes
    dth <- dtg %>% 
      mutate(DespuesDe_Dias = ifelse(is.na(DespuesDe_Dias),
                                     calendario_dias_semana[.(Fecha_arch - 1), ix] -
                                       calendario_dias_semana[.(Fecha), ix],
                                     DespuesDe_Dias)
      )
    
    mostrar_tiempos('DespuesDe_Dias generado')
    
  }
  
  if (excluir_primer_aparicion) {
    if (dbg >= 2) browser()
    dth <- dth %>% filter(Fecha_arch != ult_fecha)
    
    mostrar_tiempos('excluir_primer_aparicion generado')

  }
  
  if (tolerancia_dias != 0) {
    if (tolerancia_dias > 0) {
      dth <- dth %>% filter(DespuesDe_Dias > tolerancia_dias)
    } else {
      dth <- dth %>% filter(DespuesDe_Dias <= -tolerancia_dias)
    }
    
    mostrar_tiempos('Filtro tolerancias')
    
  }
  
  v1 <- c(v_variables, '.Variable', '.Valor', 'Previo', 'Fecha_arch', 'DespuesDe_Dias')
  dti <- dth %>% 
    select_(.dots = v1) %>% 
    arrange_(vector_of_vars_arrange)
  
  if (eliminar_arch_intermedios) rm(dth)
  
  mostrar_tiempos('Arreglos antes de generar archivos')
  
  if (dbg >= 2) browser()
  # pisp(dti, 'ALFA', 'RentDiaria')
  
  # Informar diferencias
  if (excluir_primer_aparicion) {
    print(paste(nrow(dti), "diferencias encontradas (excluyendo casos de 'aparición' en la primer fecha)."))
  } else {
    cant_sin_cambios <- dti %>% filter(Fecha_arch == ult_fecha) %>% nrow()
    print(paste('TOTAL: ', cant_sin_cambios, 'sin cambios y', nrow(dti) - cant_sin_cambios, 'diferencias encontradas.'))
  }
  if (dbg >= 1) browser()
  write.csv(dti, file = paste(format(Sys.time(), '%Y-%m-%d_%H.%M'), paste0('Cambios ', tipo_arch, '.csv')), row.names = FALSE)
  
  mostrar_tiempos('Archivo cambios generado')
  
  # Si pido comparar dos fechas, saco además una comparación de las v_variables excluyendo v_fecha
  if (!is.null(lista_fechas) && length(lista_fechas) == 2) {
    if (dbg >= 2) browser()
    if (v_fecha != '') v_variables <- setdiff(v_variables, 'Fecha')
    
    vars_fch1 <- dtb %>% filter(Fecha_arch == lista_fechas[1]) %>% 
      select_(.dots = c(v_variables)) %>% distinct_(.dots = c(v_variables))
    vars_fch2 <- dtb %>% filter(Fecha_arch == lista_fechas[2]) %>% 
      select_(.dots = c(v_variables)) %>% distinct_(.dots = c(v_variables))
    
      dif_key <- bind_rows(
        vars_fch1 %>% anti_join(vars_fch2),
        vars_fch2 %>% anti_join(vars_fch1),
        .id = 'Archivo'
      )
    if (nrow(dif_key) > 0) {
      write.csv(dif_key, file = paste(format(Sys.time(), '%Y-%m-%d_%H.%M'), paste0('Diferencias claves ', tipo_arch, '.csv')), row.names = FALSE)
    } else {
      print('No se encontraron diferencias en la composición de las variables clave entre ambos archivos.')
    }
    mostrar_tiempos('Archivo comparacion de claves generado')
  }
  return(invisible(NULL))
}

dif_significativa <- function(a, b, tolerancia_dif) {
  mayor <- pmax(abs(a), abs(b))
  
  if (tolerancia_dif == 'baja') {
    limite <- case_when(
      mayor > 10000 ~ 1,
      mayor >   100 ~ 0.1,
      mayor >    10 ~ 0.01,
      TRUE          ~ 0.0001
    )
  } else {
    limite <- case_when(
      mayor > 1000000 ~ 10000,
      mayor >  100000 ~  1000,
      mayor >   10000 ~   100,
      mayor >     100 ~    10,
      mayor >      10 ~     1,
      TRUE            ~     0.1
    )
  }
  
  abs(a - b) > limite
}

ultimos_archivos_diarios <- function(tipo_arch, url, file_type = 'RDS') {
  filenames = RCurl::getURL(url, ftp.use.epsv = FALSE, ftplistonly = TRUE)
  filenames = paste(url, strsplit(filenames, "\r\n")[[1]], sep = "")
  files <- str_replace(filenames, url, '') %>% tbl_df %>% 
    filter(str_detect(value, tipo_arch), str_detect(value, paste0('.', file_type, '$')), str_detect(value, '^20')) %>% 
    mutate(fh_arch = ymd_hm(str_sub(value, 1, 16)),
           f_arch = as_date(fh_arch),
           h_arch = as.duration(interval(f_arch, fh_arch))) %>% 
    filter(!is.na(h_arch))
  
  files %>% group_by(f_arch) %>% 
    filter(as.duration(h_arch) == as.duration(max(h_arch))) %>% 
    ungroup %>% select(archivo = value)
}

# tipo_arch <- 'familia'
# url <- 'ftp://intranet:intranet.2013@192.168.2.95/'
# 
# ultimos_archivos_del_dia(tipo_arch, url)
