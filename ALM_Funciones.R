# ALM_Funciones.R
# Carga los fuentes de la sub-carpeta R, por orden alfabético

fuentes <- dir("./R", pattern = "\\.R", ignore.case = TRUE) %>% sort

for (arch in fuentes) {
  source(arch)
}
