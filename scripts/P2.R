###############################################################################-
# Práctica 2: importación y manejo de datos
# Autora: Ana Escoto
# Fecha: 2024-06-11
###############################################################################-

# Paquetes ----

if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse, # la paquetería tidyverse
               readr, # leer archivos de texto
               readxl, # leer archivos de excel
               writexl, # escribe archivos de excel
               haven, # lee y escribe archivos dta, sav, sas
               magrittr, # pipes o tuberías
               skimr, # revisa dataframes
               sjlabelled, # etiquetes
               foreign, # archivos dbf
               janitor) # limpiar nombres de nuestros dataframes y tabulados


# Importar datos desde Excel ----

ejemplo_xlsx <- readxl::read_excel("datos/ejemplo_xlsx.xlsx", 
                                   sheet = "para_importar")


writexl::write_xlsx(ejemplo_xlsx, path = "datos/mi_exportación.xlsx") # extensión


# Importar datos desde Stata y SPSS ----

concentradohogar <- haven::read_dta("datos/concentradohogar.dta")

concentradohogar <- haven::read_sav("datos/concentradohogar.sav")


haven::write_dta(concentradohogar, path = "miexportacion.dta", version = 12)

write_rds(concentradohogar, "datos/concentradohogar.rds")

# Podemos guardar ambientes --> .Rdata
# Podemos guardar objetos específicos --> .rds
# scripts --> .R
# Proyectos --> .Rprojects

# Importando .csv y con url ----

mig_inter_quinquen_proyecciones <- readr::read_csv("https://conapo.segob.gob.mx/work/models/CONAPO/Datos_Abiertos/pry23/02_mig_inter_quinquen_proyecciones.csv")

mig_inter_quinquen_proyecciones <- read.csv("https://conapo.segob.gob.mx/work/models/CONAPO/Datos_Abiertos/pry23/02_mig_inter_quinquen_proyecciones.csv")


rm(ejemplo_xlsx, mig_inter_quinquen_proyecciones)


# Revisión dataframes ----

names(concentradohogar)

class(concentradohogar)

head(concentradohogar, n = 10)

tail(concentradohogar)

str(concentradohogar)

# Revisión skimr ----

skimr::skim(concentradohogar)


# Los pipes ----

concentradohogar  |> # parto de mi objeto con información
  head(n=20) |> # me da las primeras 20 líneas
  tail() # meda las últimas 6 líneas [de las primeras 20 líneas]

tail(head(concentradohogar, n=20))

concentradohogar$becas

# Limpiar nombres ----

names(ejemplo_xlsx)

ejemplo_xlsx$`Protección de derechos humanos`


ejemplo_xlsx<-ejemplo_xlsx |> 
  janitor::clean_names()

names(ejemplo_xlsx)

writexl::write_xlsx(ejemplo_xlsx, path = "nombres_limpios.xlsx")

#snakecase

ejemplo_xlsx |> 
  janitor::clean_names(case = "title")


ejemplo_xlsx |> 
  janitor::clean_names(prefix= "ici_")


ejemplo_xlsx |> 
  janitor::clean_names(postfix= "17")


# Mi primer tabulado ----


table(concentradohogar$clase_hog) # tabulado que no es compatible con pipes


concentradohogar |> 
  janitor::tabyl(clase_hog)

# dplyr para revisar


concentradohogar |> 
  dplyr::glimpse() # ojear "str"


# Como seleccionar casos y variables ----

concentradohogar$ing_cor
concentradohogar[,23]
concentradohogar[ ,"ing_cor"] # comillas
concentradohogar[["ing_cor"]] # comillas

concentradohogar |> 
  dplyr::select(ing_cor) # ojo sin comillas

concentradohogar |> 
  dplyr::select(-ing_cor) # Me da todo MENOS esa variable

concentradohogar[ , -c(23:30)]


# crear una variable 

concentradohogar$ing_cor2 <- concentradohogar$ing_cor
concentradohogar$ing_cor2 <- NULL

concentradohogar<-concentradohogar |> 
  dplyr::mutate(ing_cor2 = ing_cor)

concentradohogar<-concentradohogar |> 
  dplyr::mutate(ing_cor2 = NULL)

# Subconjuntos ----

subset1 <- concentradohogar |> 
  dplyr::filter(ing_cor>2000) # quedarse con los casos que cumplan esa condición

subset1 <- concentradohogar |> 
  dplyr::filter(ing_cor > 2000 & sexo_jefe == "1") # quedarse con los casos que cumplan esa condición

subset1 <- concentradohogar |> 
  dplyr::filter(ing_cor > 2000 | sexo_jefe == "1") # quedarse con los casos que cumplan alguna de esas condiciones

subset1 <- concentradohogar |> 
  dplyr::filter(!ing_cor>2000) # quedarse con los casos que NO cumplan esa condición

subset1 <- concentradohogar |> 
  dplyr::filter(!is.na(ing_cor)) # quedarse con los casos que NO cumplan esa condición


subset2<-concentradohogar |> 
  dplyr::select(ing_cor, sexo_jefe, edad_jefe)

subset2<- concentradohogar |> 
  dplyr::select(starts_with("ing"))


subset2<- concentradohogar |> 
  dplyr::select(ends_with("cor"))


subset2<- concentradohogar |> 
  dplyr::select(matches("ing"))


subset2<- concentradohogar |> 
  dplyr::select(folioviv:transf_hog)


subset2<- concentradohogar |> 
  dplyr::select(folioviv:upm, starts_with("ing"), sexo_jefe, edad_jefe)

names(subset2)

# Para negar selección o botar
# -c()

subset2<- concentradohogar |> 
  dplyr::select(-c(folioviv:upm, starts_with("ing"), sexo_jefe, edad_jefe))

names(subset2)

subset3 <- concentradohogar |> 
  dplyr::filter(sexo_jefe=="2") |> 
  dplyr::select(folioviv:upm, starts_with("ing"), edad_jefe)


# Etiquetas -----

class(concentradohogar$sexo_jefe) # reviso el tipo de varible

etiqueta_sex <- c("Hombre", "Mujer") # creo el vector de etiquetas

concentradohogar<-concentradohogar |> 
  dplyr::mutate(sexo_jefe = as_numeric(sexo_jefe)) |>  # cambio a valores numéricos
  dplyr::mutate(sexo_jefe = sjlabelled::set_labels(sexo_jefe, labels = etiqueta_sex)) # set labels, establezco las etiquetas


dplyr::glimpse(concentradohogar$sexo_jefe)

table(sjlabelled::as_label(concentradohogar$sexo_jefe))
table(concentradohogar$sexo_jefe)

class(concentradohogar$sexo_jefe)
class(as_label(concentradohogar$sexo_jefe))

concentradohogar |> 
  mutate(sexo_jefe=as_label(sexo_jefe)) |> 
  tabyl(sexo_jefe)
