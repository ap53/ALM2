#### ALM_2_parseo_expr.R

# La parte central de ALM consiste en el parseo y evaluación de expresiones
# 'cuasi-R' que tienen dos particularidades que hacen que no se puedan evaluar
# directamente:

# 1) Tienen funciones que no existen en R: por ejemplo se puede pedir nav('VIX')
#    o Margen('TOTAL T+ L'): la característica es que el nombre de estas 'funciones'
#    corresponde a valores que figuran en la columna 'variable' del dataframe 'dtb'
#    Otros parámetros de estas 'funciones', permiten especificar (además de la
#    familia o especie) qué datos se buscan de la serie.

# 2) Aparecen 'llaves', o sea expresiones encerradas entre '{}' que permiten 
#    definir nuevas series como expresiones algebraícas de las series originales.
#    Estas llaves definen nuevas 'funciones' que a su vez pueden ser evaluadas
#    como las del punto anterior.

# Este archivo contiene las rutinas que analizan la expresión de la alarma y la 
# transforman en una expresión evaluable por R. Para esto transforman las
# funciones no definidas en llamados a dos nuevas funciones: calc_termino y
# llave(), ambas definidas en ALM_3_correr_alarma().

transformar_expresion <- function(x, res, nivel_arbol = 1, nivel_llave = 0) {
  ### transformar_expresion 
  # Recibe una expresión x cualquiera (puede ser la alarma
  # entera, o una rama) y la devuelve después de haber transformado los llamados
  # a 'variables dtb' en llamados a calc_termino() y los llamados a '{}' en
  # llamados a llave. En el mismo proceso, reemplaza cualquier variable R por su
  # valor, a fin de permitir mandar información acerca de qué se está calculando
  # a JIRA.
  
  # La rutina está adaptada de tree() del package 'pryr' de Hadley.
  
  #  NOTA: la función devuelve un call transformado. En si misma no tiene 
  #  side-effects, pero la expresion transformada llama a calc_termino() que sí
  #  los tiene y modifica 2 items de la lista 'res': alarm_env$res$flias y alarm_env$res$variables
  
  tmp_class_x <- class(x)
  empieza_llave <- FALSE
  
  # x_bq <- bquote(.(x))    No borrar, tb funciona pero es menos clara. La guardo para aprender a usar bquote.
  
  pos_frame_ppal <- llamado_desde('c_a')
  if (is_atomic(x)) {
    x
  } else if (is.name(x)) {
    # Guardar nombre y valor de la variable para reemplazo en los textos para JIRA
    x_name <- as.character(x)
    var <- get(x_name, envir = sys.frame(pos_frame_ppal))
    
    if (is_atomic(var)) {
      if (!x_name %in% names(alarm_env$res$variables)) {
        variables <- get('variables', envir = sys.frame(pos_frame_ppal))
        variables <- c(variables, var)
        names(variables)[length(variables)] <- x_name
        # NOTA: Side-effect: estoy modificando la list 'variables' en c_a
        assign('variables', variables, envir = sys.frame(pos_frame_ppal))
      }
    }
    
    # Y seguir la recorrida...
    x
  } else if (is.call(x)) {
    term <- as.character(x[[1]])
    
    if (dbg()) {
      print(paste0('in ', nivel_arbol, ':', tmp_class_x))
      print(paste0('in ', nivel_arbol, ':', '  length(x):', length(x)))
      print(paste0('in ', nivel_arbol, ':', '  x:', paste0(x, collapse='@')))
      print(paste0('in ', nivel_arbol, ':', '  length(term):', length(term)))
      print(paste0('in ', nivel_arbol, ':', '  term:', term))
    }
    
    if (length(term) > 1 && term[1] =='{') {
      # Setear los flags que me van a permitir manejar 'llave()' en forma diferente
      empieza_llave <- TRUE
      nivel_llave <- nivel_llave + 1
      
      # La única forma que encontré de recuperar una expresión en forma 'infix'
      # es con print(). Con "as.character" (y varias otras cosas que probé) se
      # transforma en 'prefix', por ej. "2 + 3" se transforma en "`+` 2 3"
      ticker_serie_nueva <- capture.output(print(x[[1]][[2]]))
      
      ticker_serie_nueva <- paste0(str_replace_all(as.character(ticker_serie_nueva), '[\"\']', ''), 
                                   collapse = '')
      ticker_serie_nueva <- str_replace_all(ticker_serie_nueva, '\n', '')
      ticker_serie_nueva <- str_replace_all(ticker_serie_nueva, ' +', ' ')
      
      ### Reemplazar '{()' por' llave()'. 
      x[[1]] <- as.call(c(substitute(llave), ticker_serie_nueva, as.list(x[[1]])[2:length(x[[1]])]))
      
      
      # Si no es llave, puede ser un término de Alarma, o sea una 'variable' de dtb
    } else if (!exists(term)) {
      # NOTA: Side-effect: en este llamado también se modifica la list 'res' 
      x <- reemplazar_expr_termino(x, term, sign(nivel_llave))   
    } 
    
    hijos <- lapply(as.list(x), transformar_expresion, nivel_arbol = nivel_arbol + 1, nivel_llave)
    
    #  Si estoy saliendo de una llave, acomodo los flags
    if (empieza_llave) {
      empieza_llave <- FALSE
      nivel_llave <- nivel_llave - 1
    }
    
    # Y sigue el recorrido
    x <- as.call(as.list(hijos))
    
    if (dbg()) {
      tmp_class_x <- class(x)
      
      print(paste0('out ', nivel_arbol))
      print(paste0('out ', nivel_arbol, ':', tmp_class_x))
      print(paste0('out ', nivel_arbol, ':', '  length(x):', length(x)))
      print(paste0('out ', nivel_arbol, ':', '  x:', paste0(x, collapse='@')))
    }
    
    x
  } else {
    stop('Clase: ', class(x), ' no soportada en transformar_expresion')
  }
}

reemplazar_expr_termino <- function(x, term, es_llave) {
  ### reemplazar_expr_termino
  
  if (length(verificar_diccionario(term)[[1]]) > 0) {
    # pos_frame_ppal <- llamado_desde('c_a')
    
    # Lo normal va a ser que el param 'familia' (q puede ser una 'especie') venga sin nombre.
    # En este caso debe venir como el primer param de tipo 'character'.
    # Hay que nombrarlo, por las dudas de que 'duracion' o 'start' vengan por 'default'.
    
    # Pero primero me aseguro de que no esté nombrado el alguna otra posicion de la lista.
    if (is.null(as.list(x)$familia)){
      if (length(as.list(x)) < 2) 
        stop('Como mínimo debe estar explicitado el parámetro familia.', call. = FALSE)
      
      
      for (parm in 2:length(as.list(x))) {
        if (class(as.list(x)[[parm]]) == 'character') {
          # Podría ser la duración expresada como string
          if (!detectar_duracion_string(as.list(x)[[parm]])) {
            # Ojo: no renombrar otro parametro ya nombrado...
            if (is.null(names(as.list(x))) || (names(as.list(x))[[parm]] == '')) {
              names(x)[[parm]] <- 'familia'
              break()
            }
          }
        } else if (class(as.list(x)[[parm]]) == 'name') {
          x_name <- as.character(as.list(x)[[parm]])
          var <- get(x_name, envir = sys.frame(1))
          
          if (is_atomic(var)) {
            # Ojo: no renombrar otro parametro ya nombrado...
            if (is.null(names(as.list(x))) || (names(as.list(x))[[parm]] == '')) {
              names(x)[[parm]] <- 'familia'
              break()
            }
          }
        }
      }
      
      # Al forzar un nombre específico, los nombres 'vacíos' quedan como NA
      names(x) <- vapply(names(x), function(n) if (is.na(n)) n <- '' else n <- n, '')
      
      if (is.null(as.list(x)$familia))
        stop('Como mínimo debe estar explicitado el parámetro familia.', call. = FALSE)
      
      
    }
    
    x <- as.call(c(substitute(calc_termino), term, 
                   as.list(x)[2:length(x)], es_llave = es_llave))
    
    x
  } else {
    stop(paste(term, 'no figura en el diccionario de términos válidos'), call. = FALSE)
  }
}

llave <- function(ticker_serie_nueva, llv_expr, es_llave = 0) {
  ### llave
  
  pos_frame_ppal <- llamado_desde('c_a')
  variables <- get('variables', envir = sys.frame(pos_frame_ppal))
  if (!is.null(variables) && length(variables) > 0)
    ticker_serie_nueva <- str_replace_all(ticker_serie_nueva, variables)
  
  ya_existe <- nrow(dtb[.(ticker_serie_nueva, 'llave'), nomatch = 0]) > 0
  
  if(!ya_existe) {
    expr <- expr_find(llv_expr)
    texto_alarma <- expr_text(llv_expr)
    values <- eval(expr) # Recordatorio: llama a calc_termino varias veces
    
    ## Insertar en dtb
    
    # OJO: ASUMO QUE TODAS LAS SERIES INVOLUCRADAS TIENEN EL MISMO CALENDARIO
    # Puedo usar una serie cualquiera como template para la nueva...
    serie_template <- str_extract(ticker_serie_nueva, '^[^\\)]*\\)')
    variable_template <- str_extract(serie_template, '[^\\(]*')
    ticker_template <- str_replace(serie_template, variable_template, '')
    # Tengo algo tipo   (VIX)  sin comillas, pero con paréntesis: los elimino
    ticker_template <- str_sub(ticker_template, 2, -2)
    
    template <- dtb[.(ticker_template, variable_template)]
    template <- template %>% mutate(variable = 'llave', ticker = ticker_serie_nueva) %>% 
      arrange(desc(date))
    template['value'] <- values[[1]]
    
    # La inserto en dtb
    l <- list(dtb, template)
    dtb <<- rbindlist(l, use.names=TRUE)
    keycols <- c('ticker', 'variable')
    setDT(dtb, keycols, keep.rownames=FALSE)
  }
  a <- 1
  function(...) prep_calc_llave(familia = ticker_serie_nueva, ...)
}

prep_calc_llave <- function(...) {
  calc_termino('llave', ...)
}

