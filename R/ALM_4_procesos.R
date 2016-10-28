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
                              excluir_primer_aparicion = TRUE, 
                              tolerancia_dias = 0, # >0 se queda con DespuesDe_Dias > tol, < 0 se queda con los cercanos
                              tolerancia_dif = 'baja', # cualquier otro string es 'alta'
                              dbg = 0) {
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
    v_variables <- c('Fecha', 'CarteraNom')
    v_observaciones <- c('Cantidad', 'Valuacion', 'Resultado')
  }
  v_todas <- c(v_variables, v_observaciones)
  
  # Extraer lista de nombres de archivos del tipo deseado (tipo_arch, fecha_tope)
  #   Ordenarlos por fecha *descendiente*
  lista_arch <- list.files(path_bkp, pattern = tipo_arch) %>% sort(decreasing = TRUE)
  
  # Verificar que exista el de la fecha base solicitada
  archivo_base <- paste0(format(fecha_inic, "%Y-%m-%d"), '.', tipo_arch, '.RDS')
  if (!(archivo_base %in% lista_arch)){
    fecha_inic <- str_sub(lista_arch[1], 1, 10) %>% ymd()
    archivo_base <- paste0(format(fecha_inic, "%Y-%m-%d"), '.', tipo_arch, '.RDS')
    
    print(paste('OJO: No existe', archivo_base, 'en la carpeta', path_bkp, ' ==> fecha_inic = ', format(fecha_inic, '%Y-%m-%d')))
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
  
  if (dbg >= 2) browser()
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
  
  vector_of_vars <- c(v_variables, '.Variable')
  vector_of_vars_arrange <- c(vector_of_vars, 'desc(Fecha_arch)')
  dtc <- dtb %>% right_join(combinaciones) %>%  
    arrange_(.dots = vector_of_vars_arrange) %>% 
    group_by_(vector_of_vars)
  
  dtc <- dtc %>% mutate(Previo = lead(.Valor)) %>% ungroup
  if (v_fecha != '') {
    dtc <- dtc %>% filter(Fecha_arch > Fecha)
  }
  
  dtd <- dtc %>% 
    filter(dif_significativa(coalesce(.Valor, -987987654.321), 
                             coalesce(Previo, -987987654.321),
                             tolerancia_dif))
  
  dtd <- dtd %>% arrange_(.dots = vector_of_vars_arrange) %>% 
    ungroup %>% group_by_(v_variables)  # , .Variable
  
  if (excluir_primer_aparicion) {
    dtd <- dtd %>% filter(Fecha_arch != ult_fecha)
    
    if (v_fecha != '') {
      primer_fecha <- (dtd %>% summarise(min(Fecha)))[[1]][1]
      dtd <- dtd %>% filter(Fecha != primer_fecha)
    }
  }
  
  if (v_fecha == '') {
    dth <- dtd %>% mutate('FchPrevia' = lead(Fecha_arch), DespuesDe_Dias = Fecha_arch - FchPrevia)
  } else { 
    dtg <- dtd %>% mutate(DespuesDe_Dias =
                            calendario_dias_semana[.(Fecha_arch), ix] -
                            calendario_dias_semana[.(Fecha), ix])
    
    # Algunos archivos se generan un sábado y dan NA porque no existen en
    # calendario: para este cálculo asumo que se generaron el viernes
    dth <- dtg %>% 
      mutate(DespuesDe_Dias = ifelse(is.na(DespuesDe_Dias),
                                     calendario_dias_semana[.(Fecha_arch - 1), ix] -
                                       calendario_dias_semana[.(Fecha), ix],
                                     DespuesDe_Dias)
      )
  }
  
  if (tolerancia_dias != 0) {
    if (tolerancia_dias > 0) {
      dth <- dth %>% filter(DespuesDe_Dias > tolerancia_dias)
    } else {
      dth <- dth %>% filter(DespuesDe_Dias <= -tolerancia_dias)
    }
  }
  
  v1 <- c(v_variables, '.Variable', '.Valor', 'Previo', 'Fecha_arch', 'DespuesDe_Dias')
  dti <- dth %>% 
    select_(.dots = v1) %>% 
    arrange_(vector_of_vars_arrange)
  
  if (dbg >= 2) browser()
  # pisp(dti, 'ALFA', 'RentDiaria')
  
  # Informar diferencias
  cant_sin_cambios <- dti %>% filter(Fecha_arch == ult_fecha) %>% nrow()
  print(paste('TOTAL: ', cant_sin_cambios, 'sin cambios y', nrow(dti) - cant_sin_cambios, 'diferencias encontradas.'))
  write.csv(dti, file = paste(str_replace_all(today(), '-', ''), paste0('Cambios ', tipo_arch, '.csv')), row.names = FALSE)
  return(dti)
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

