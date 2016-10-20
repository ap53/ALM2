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