librerias_base()

comparar_archivos <- function(fech_fija, tipo_arch, path_bkp, fecha_tope = as.Date('2012-01-01')) {
  # Calendario para contar dias de semana(o sea los laborables)
  calendario_dias_semana <- data.table(fecha = fech_fija - seq(as.numeric(fech_fija - fecha_tope) + 1) )
  calendario_dias_semana <- calendario_dias_semana[, dia_semana := wday(fecha)][dia_semana %in% 2:6]
  setkey(calendario_dias_semana, fecha)
  calendario_dias_semana <- calendario_dias_semana[, ix := seq_len(.N)]
  
  # Extraer lista de nombres de archivos del tipo deseado (tipo_arch, fecha_tope)
  #   Ordenarlos por fecha *descendiente*
  lista_arch <- list.files(path_bkp, pattern = tipo_arch) %>% sort(decreasing = TRUE)
  
  # Verificar que exista el de la fecha base solicitada
  archivo_base <- paste0(format(fech_fija, "%Y-%m-%d"), '.', tipo_arch, '.RDS')
  if (!(archivo_base %in% lista_arch))
    stop('No existe', archivo_base, 'en la carpeta', path_bkp)
  
  # Leer estructura del tipo de archivo buscado
  ##### tipo_vars <- readxl::read_excel('params_archivos.xlsx') %>% 
  #####   filter(id_archivo == tipo_arch)
  ##### lst_fecha <- (tipo_vars %>% 
  #####               filter(tipo == 'fecha') %>% 
  #####               select(nombre))[[1]]
  ##### lst_variables <- (tipo_vars %>% 
  #####                   filter(tipo == 'variable') %>% 
  #####                   select(nombre))[[1]]
  ##### lst_observaciones <- (tipo_vars %>% 
  #####                       filter(tipo == 'observacion') %>% 
  #####                       select(nombre))[[1]]
  ##### 
  ##### # v_fecha <- 'Fecha'
  ##### # v_variables <- c('Fecha', 'NombreFamilia')
  ##### # v_observaciones <- c('PatFamilia', 'RentDiaria', 'Suscripciones', 'Rescates')
  ##### 
  ##### lst_guardar <- c(lst_variables, lst_observaciones)
  ####
  v_todas <- c(v_variables, v_observaciones)
  
  # Eliminar de la lista las más recientes que esta fecha base
  lista_arch <- lista_arch[which(ymd(str_sub(lista_arch, 1, 10)) < fech_fija & ymd(str_sub(lista_arch, 1, 10)) >= fecha_tope)] %>% 
    sort(decreasing = TRUE)
  
  # Leer datos base
  dt_base <- readRDS(paste0(path_bkp, archivo_base))
  
  # Son archivos grandes, data.table es más conveniente que dplyr
  if (!inherits(dt_base, 'data.table')) dt_base <- as.data.table(dt_base )
  
  if (v_fecha != '' && v_fecha %in% names(dt_base)) {
    columna_fecha <- match(v_fecha, names(dt_base))
    if (class(dt_base[[columna_fecha]]) == 'character') {
      # browser()
      # v_fch_ <- parse(text = v_fecha)
      # dt_base[, v_fch_ := str_sub(v_fch_, 1, str_locate(v_fch_, ' ')[1] - 1), with = FALSE]
      # 
      dt_base[[columna_fecha]] <- 
        ymd(str_sub(dt_base[[columna_fecha]], 1, str_locate(dt_base[[columna_fecha]], ' ')[1] - 1))
    }
    dt_base <- dt_base[get(v_fecha) >= fecha_tope]
  }
  
  # Eliminar columnas innecesarias
  dt_base <- data.table(as.data.frame(dt_base)[, get('v_todas')]) #[order(-Fecha)]
  
  # Comienzo acumulación
  dta <- dt_base %>% mutate(Fecha_arch = fech_fija, Existe = 'X')
  
  
  # Loop sobre los anteriores a la fecha base solicitada
  for (arch in lista_arch) {
    # Extraer fecha loop
    fecha_loop <- str_sub(arch, 1, 10) %>% ymd()
    print(fecha_loop)
    
    # En el base, truncar las fechas más recientes que la del loop
    if (v_fecha != '' && v_fecha %in% names(dt_base)) {
      dt_base <- dt_base[get(v_fecha) <= fecha_loop - 1, ]
    }
    
    # Leer el target
    dt_loop <- readRDS(paste0(path_bkp, arch))
    if (dim(dt_loop)[2] == 0) {
      print(paste0('Problemas con el archivo: ', path_bkp, arch))
      next()
    }
    
    if (v_fecha != '' && v_fecha %in% names(dt_loop)) {
      columna_fecha <- match(v_fecha, names(dt_loop))
      if (class(dt_loop[[columna_fecha]]) == 'character') {
        dt_loop[[columna_fecha]] <- 
          ymd(str_sub(dt_loop[[columna_fecha]], 1, str_locate(dt_loop[[columna_fecha]], ' ')[1] - 1))
      }
      dt_loop <- dt_loop[get(v_fecha) >= fecha_tope]
    }
    
    if (!inherits(dt_loop, 'data.table')) dt_loop <- as.data.table(dt_loop )
    
    # Eliminar columnas innecesarias
    dt_loop <- data.table(as.data.frame(dt_loop)[, get('v_todas')]) %>% 
      mutate(Fecha_arch = fecha_loop, Existe = 'X')
    
    # Acumulo
    dta <- rbindlist(list(dta, dt_loop), use.names=TRUE, fill=TRUE)
    
  }
  
  ult_fecha <- fecha_loop
  no_hay_var_observaciones <- FALSE
  
  if (no_hay_var_observaciones) {
    dta <- dta %>% rename(.Variable = Existe) %>% mutate(.Valor = 1)
  } else {
    dta <- dta %>% select(-Existe)
    dtb <- dta %>% gather_('.Variable', '.Valor', v_observaciones, convert = TRUE)
  }
  
  ##### v_variables_fch_arch <- c(v_variables, '.Variable')

  ##### dd2 <- list( ~nesting_(list('Fecha' = Fecha, 'NombreFamilia' = NombreFamilia, '.Variable' = .Variable)), ~Fecha_arch)
  ##### ptos <- list(~nesting_(as.list(setNames(nm = v_variables_fch_arch))), ~Fecha_arch)
  ##### 
  ##### selected_var1 <- "Fecha"
  ##### selected_var2 <- "NombreFamilia"
  ##### selected_var3 <- ".Variable"
  ##### 
  ##### # Assemble expressions from those choices
  ##### L1 <- list(
  #####   interp(~x, x = as.name(selected_var1)),
  #####   interp(~x, x = as.name(selected_var2)),
  #####   interp(~x, x = as.name(selected_var3))
  ##### )
  ##### L1 <- setNames(L1, c(selected_var1, selected_var2, selected_var3))
  ##### 
  ##### L2 <- list(
  #####   interp(~nesting_(L), 
  #####          L = L1),
  #####   interp(~x, x = as.name('Fecha_arch'))
  ##### )
  ##### 
  ##### # Create variable names
  ##### # exp_name1 <- paste0(selected_func1, "_", selected_var1)
  ##### # exp_name2 <- paste0(selected_func2, "_", selected_var2)
  ##### # 
  ##### # bundled2 <- setNames(bundled2, c(exp_name1, exp_name2))
  
  combinaciones <- dtb %>% expand(nesting(Fecha, NombreFamilia, .Variable ), Fecha_arch)
  
  dtc <- dtb %>% right_join(combinaciones) %>%  
    arrange(Fecha, NombreFamilia, .Variable , desc(Fecha_arch)) %>% 
    group_by(Fecha, NombreFamilia, .Variable )
  
  dtc <- dtc %>% mutate(Previo = lead(.Valor)) %>% ungroup
  
  dtd <- dtc %>% filter(coalesce(.Valor, -987654.321) != coalesce(Previo, -987654.321)) %>% 
    arrange(Fecha, NombreFamilia, .Variable, desc(Fecha_arch)) %>% 
    group_by(Fecha, NombreFamilia, .Variable)
  
  # dtd <- dtd %>% filter(Fecha_arch != ult_fecha)

  dte <- dtd %>% mutate('FchPrevia' = lead(Fecha_arch), DespuesDe_Dias = Fecha_arch - FchPrevia)
  
  dtf <- dte %>% 
    select(Fecha, NombreFamilia, .Variable, .Valor, Previo, Fecha_arch, DespuesDe_Dias) %>% 
    mutate(.Valor = coalesce(.Valor, 0), DespuesDe_Dias = fech_fija - Fecha_arch) %>% 
    arrange(NombreFamilia, .Variable, Fecha, desc(Fecha_arch))
  
  # Informar diferencias
  cant_sin_cambios <- dtf %>% filter(Fecha_arch == ult_fecha) %>% nrow()
  print(paste('TOTAL: ', cant_sin_cambios, 'sin cambios y', nrow(dtf) - cant_sin_cambios, 'diferencias encontradas.'))
  write.csv(dtf, file = paste(str_replace_all(today(), '-', ''), paste0('Cambios ', tipo_arch, '.csv')), row.names = FALSE)
  return(dtf)
}

#### Ejecucion --------------------------------------------------------------------------------------
fech_fija <- ymd('2016-01-12')
tipo_arch <- 'pnl'
path_bkp <- '../data/Backup Data Contable/'
fecha_tope <- ymd('2016-01-6')

v_fecha <- 'Fecha'
v_variables <- c('Fecha', 'NombreFamilia')
v_observaciones <- c('PatFamilia', 'RentDiaria', 'Suscripciones', 'Rescates')

debugonce(comparar_archivos)
dif <- comparar_archivos(fech_fija, tipo_arch, path_bkp, fecha_tope) 

f_i <- c('Alfa Total', 'BETA', 'BREAKOUTS', 'Cash', 'COBERTURA', 'DONCHIAN', 
         'FUTUROS ADMINISTRADOS', 'GASTOS', 'Gastos Operativos', 'MACROMODEL', 
         'PRIMA DE MERCADO', 'TESORERIA', 'TOTAL T+ L', 'TSM', 'VIX + STRANGLES')

generar_csv_diferencias(dif, f_i)

# rm(comparar_archivos, buscar_diferencias)
