# ALM_0_Funciones.R
# Carga los fuentes de la sub-carpeta R, por orden alfabético


librerias_base <- function(){
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(lubridate))
  suppressPackageStartupMessages(library(stringr))
  suppressPackageStartupMessages(library(tidyr))
  suppressPackageStartupMessages(library(data.table))
  suppressPackageStartupMessages(library(pryr))
  suppressPackageStartupMessages(library(stringi))
  # suppressPackageStartupMessages(library(readr))
  suppressPackageStartupMessages(library(lazyeval))
  suppressPackageStartupMessages(library(RCurl))
}
librerias_base()


# Carga de los fuentes
fuentes <- dir("./R", pattern = "\\.R", ignore.case = TRUE) %>% sort

# Sourceo todo lo que encuentro en ./R/, pero primero elimino este archivo del loop.
fuentes <- fuentes[!fuentes == 'ALM_0_Funciones.R']
for (arch in fuentes) {
  source(paste0('./R/', arch))
}

rm(fuentes)
