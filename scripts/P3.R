###############################################################################-
# Práctica 3: Revisión de elementos estadísticos básicos desde {tidyverse}
# Autora: Ana Escoto
# Fecha: 2024-06-12
###############################################################################-

# Paquetes ----


if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse,
               writexl, 
               haven,
               sjlabelled, 
               janitor,
               magrittr,
               GGally,
               wesanderson,
               gt,
               pollster,
               dineq
)


# Datos ----

concentradohogar <- haven::read_sav("datos/concentradohogar.sav") 


## Variables nominales ----

concentradohogar %>% 
  dplyr::count(sexo_jefe=="2") # cuentan los casos que cumplen con la condición "sexo_jefe==2"


concentradohogar %>%
  with(
    table(sexo_jefe)
  )

# with() me permite usar algunos comandos de base en formato de pipe

### Recordemos nuestro etiquetado


etiqueta_sex<-c("Hombre", "Mujer")

concentradohogar<-concentradohogar %>% 
  mutate(sexo_jefe=as_numeric(sexo_jefe)) %>% # para quitar el "string"
  mutate(sexo_jefe=sjlabelled::set_labels(sexo_jefe, labels=etiqueta_sex)) 


concentradohogar<-concentradohogar %>% 
  mutate(clase_hog=as_numeric(clase_hog)) %>% # para quitar el "string"
  sjlabelled::set_labels(clase_hog, labels=c("unipersonal",
                                             "nuclear", 
                                             "ampliado",
                                             "compuesto",
                                             "corresidente")) 


# Con "tabyl()"" de "janitor""


tab <- concentradohogar %>%
  dplyr::mutate(sexo_jefe=as_label(sexo_jefe))  %>%
  janitor::tabyl(sexo_jefe)

  
concentradohogar %>% 
  #dplyr::mutate(sexo_jefe=as_label(sexo_jefe))  %>%
  janitor::tabyl(sexo_jefe)%>% 
  janitor::adorn_totals()


class(concentradohogar$sexo_jefe) # variable sin etiqueta
class(as_label(concentradohogar$sexo_jefe)) # variable con etiqueta

class(as_label(concentradohogar$educa_jefe)) # variable ordinal
class(concentradohogar$ing_cor) # variable de intervalo/razón

  
dplyr::glimpse(concentradohogar$sexo_jefe)


concentradohogar %>% 
  dplyr::mutate(sexo_jefe=as_label(sexo_jefe)) %>% # cambia los valores de la variable a sus etiquetas
  janitor::tabyl(sexo_jefe) %>% # para hacer la tabla
  janitor::adorn_totals() %>% # añade totales
  janitor::adorn_pct_formatting(digits = 2)  # nos da porcentaje en lugar de proporción

### Variables ordinales ----

glimpse(concentradohogar$educa_jefe)


concentradohogar <-concentradohogar %>% 
  mutate(educa_jefe=as_numeric(educa_jefe)) %>% 
  set_labels(educa_jefe,
             labels=c("Sin instrucción", 
                      "Preescolar",
                      "Primaria incompleta",
                      "Primaria completa",
                      "Secundaria incompleta",
                      "Secundaria completa",
                      "Preparatoria incompleta",
                      "Preparatoria completa",
                      "Profesional incompleta",
                      "Profesional completa",
                      "Posgrado"))
  
concentradohogar %>%
  mutate(educa_jefe=as_label(educa_jefe)) %>% 
  tabyl(educa_jefe) |> 
  mutate(freq_acumulado = cumsum(n)) |> 
  mutate(pct_acumulado = cumsum(percent)) 


# Para que no nos salgan las categorías sin datos podemos *apagar* la opción `show_missing_levels=F` dentro del comando "tabyl()"


concentradohogar %>% 
  filter(educa_jefe>4) |> 
  mutate(educa_jefe=as_label(educa_jefe)) %>% 
  tabyl(educa_jefe, show_missing_levels=F ) %>% # esta opción elimina los valores con 0
  adorn_totals()  


# Bivariado cualitativo ----

#### Cálculo de frecuencias

# Las tablas de doble entrada tiene su nombre porque en las columnas entran los valores de una variable categórica, y en las filas de una segunda. Básicamente es como hacer un conteo de todas las combinaciones posibles entre los valores de una variable con la otra.
# 
# Por ejemplo, si quisiéramos combinar las dos variables que ya estudiamos lo podemos hacer, con una tabla de doble entrada:
  
  
concentradohogar %>% 
  mutate(clase_hog=as_label(clase_hog)) %>% 
  mutate(sexo_jefe=as_label(sexo_jefe)) %>% # para que las lea como factor
  janitor::tabyl(clase_hog, sexo_jefe, show_missing_levels=F ) %>% # incluimos aquí 
  janitor::adorn_totals(where = "col")  


# Observamos que en cada celda confluyen los casos que comparten las mismas características:
  
  
concentradohogar %>%   
  count(clase_hog==1 & sexo_jefe==1) # nos da la segunda celda de la izquierda



### Totales y porcentajes ----


concentradohogar %>% 
  mutate(clase_hog=as_label(clase_hog)) %>% 
  mutate(sexo_jefe=as_label(sexo_jefe)) %>% # para que las lea como factor
  tabyl(clase_hog, sexo_jefe, show_missing_levels=F ) %>% # incluimos aquí dos variables
  adorn_totals("col")  

# 
# O bien agregar los dos, introduciendo en el argumento `c("col", "row")` un vector de caracteres de las dos opciones requeridas:
  
  
concentradohogar %>% 
  mutate(clase_hog = as_label(clase_hog)) %>% 
  mutate(sexo_jefe = as_label(sexo_jefe)) %>% # para que las lea como factor
  tabyl(clase_hog, sexo_jefe, show_missing_levels = F ) %>% # incluimos aquí dos variable
  adorn_totals(where = c("col", "row")) 
  
  
concentradohogar %>% 
  dplyr::mutate(clase_hog = as_label(clase_hog)) %>% 
  dplyr::mutate(sexo_jefe = as_label(sexo_jefe)) %>% # para que las lea como factor
  janitor::tabyl(clase_hog, sexo_jefe, show_missing_levels = F ) %>% # incluimos aquí dos variable
  janitor::adorn_totals(where = c("col", "row")) %>% 
  janitor::adorn_percentages(denominator="col") %>% # Divide los valores entre el total de la columna
  adorn_pct_formatting() # lo vuelve porcentaje

  
concentradohogar %>% 
  dplyr::mutate(clase_hog = as_label(clase_hog)) %>% 
  dplyr::mutate(sexo_jefe = as_label(sexo_jefe)) %>% # para que las lea como factor
  janitor::tabyl(clase_hog, sexo_jefe, show_missing_levels = F ) %>% # incluimos aquí dos variable
  janitor::adorn_totals(where = c("col", "row")) %>% 
  janitor::adorn_percentages(denominator="row") %>% # Divide los valores entre el total de la fila
  janitor::adorn_pct_formatting() # lo vuelve porcentaje


concentradohogar %>% 
  mutate(clase_hog = as_label(clase_hog)) %>% 
  mutate(sexo_jefe = as_label(sexo_jefe)) %>% # para que las lea como factor
  tabyl(clase_hog, sexo_jefe, show_missing_levels = F ) %>% # incluimos aquí dos variable
  adorn_totals(c("col", "row")) %>% 
  adorn_percentages("all") %>% # Divide los valores entre el total de la población
  adorn_pct_formatting() # lo vuelve porcentaje


# Factores de expansión y algunas otras medidas ----

concentradohogar |> 
  select(factor)

## La función `tally()` ----


concentradohogar %>% 
  dplyr::group_by(as_label(sexo_jefe)) %>% 
  dplyr::tally(factor) %>% #nombre del factor
  janitor::adorn_totals()  # Agrega total


# Podemos usar funciones de `adorns...` de \`{janitor}


concentradohogar %>% 
  group_by(as_label(sexo_jefe)) %>% 
  tally(factor) %>% #nombre del factor
  janitor::adorn_totals() %>% # Agrega total
  janitor::adorn_percentages("all")  %>% 
  janitor::adorn_pct_formatting()


## Con `dplyr::count()` ----

# La función `count()` también permite dar pesos a la operaciones de frecuencias, con el argumento `wt =`


concentradohogar %>% 
  dplyr::count(sexo_jefe, clase_hog,  wt = factor) 



# Es compatible con etiquetas


concentradohogar %>% 
  count(as_label(sexo_jefe), 
        as_label(clase_hog),
        wt = factor) 


## con `{pollster}` ----

# Para una variable


# tabulado simple con factor de expansión

concentradohogar %>% 
  dplyr::mutate(sexo_jefe = sjlabelled::as_label(sexo_jefe)) %>% 
  pollster::topline(sexo_jefe , # variable a tabular
                    weight = factor, # factor de expansión
                    cum_pct = F, # apagando el porcentaje acumulado
                    valid_pct = F) # apagando el porcentaje válido. No hay missings


concentradohogar %>% 
  dplyr::mutate(sexo_jefe = sjlabelled::as_label(sexo_jefe)) %>% 
  pollster::topline(sexo_jefe , # variable a tabular
                    weight = factor, # factor de expansión
                    cum_pct = F, # apagando el porcentaje acumulado
                    valid_pct = F) |> # apagando el porcentaje válido. No hay missings
  janitor::adorn_totals()


# Para dos variables

concentradohogar %>% 
  dplyr::mutate(sexo_jefe = sjlabelled::as_label(sexo_jefe)) %>% 
  dplyr::mutate(clase_hog = sjlabelled::as_label(clase_hog)) %>% 
  pollster::crosstab(sexo_jefe, clase_hog, weight = factor)



concentradohogar %>% 
  dplyr::mutate(sexo_jefe = sjlabelled::as_label(sexo_jefe)) %>% 
  dplyr::mutate(clase_hog = sjlabelled::as_label(clase_hog)) %>% 
  pollster::crosstab(sexo_jefe, # la variable en las filas
                     clase_hog, # la variable que va en las columnas
                     weight = factor, # pongo variable facto
                     pct_type = "col" # default -row
                     )

concentradohogar %>% 
  dplyr::mutate(sexo_jefe = sjlabelled::as_label(sexo_jefe)) %>% 
  dplyr::mutate(clase_hog = sjlabelled::as_label(clase_hog)) %>% 
  pollster::crosstab(sexo_jefe, # la variable en las filas
                     clase_hog, # la variable que va en las columnas
                     weight = factor, # pongo variable facto
                     pct_type = "cell" # default -row
  )

concentradohogar %>% 
  dplyr::mutate(sexo_jefe = sjlabelled::as_label(sexo_jefe)) %>% 
  dplyr::mutate(clase_hog = sjlabelled::as_label(clase_hog)) %>% 
  pollster::crosstab(sexo_jefe, # la variable en las filas
                     clase_hog, # la variable que va en las columnas
                     weight = factor, # pongo variable facto
                     pct_type = "col" # default -row
  ) |> 
  adorn_totals(where = "col")


# Descriptivos para variables cuantitativas ----

## Medidas numéricas básicas ----

# 5 números

summary(concentradohogar$ing_cor) ## ingresos

concentradohogar |> 
  dplyr::select(starts_with("in")) |> 
  summary()

# Con pipes se pueden crear "indicadores" de nuestras variables es un tibble

# Los ingresos de la ENIGH son trimestrales
concentradohogar %>% 
  dplyr::filter(tam_loc=="1") |>  # tamaño de localidad de más 100,000 hb
  dplyr::group_by(sjlabelled::as_label(clase_hog), as_label(sexo_jefe)) |> # generando
  dplyr::summarise(media_ingreso = mean(ing_cor/3, na.rm = T),
                   media_ponderada_ing= weighted.mean(ing_cor/3, na.rm = T, w = factor),
                   mediana_ingresos = median(ing_cor/3, na.rm = T),
                   sd_ingresos = sd(ing_cor/3, na.rm = T), 
                   media_integrantes = mean(tot_integ, na.rm = T))


tab_estadisticos <- concentradohogar %>% 
  dplyr::filter(tam_loc=="1") |>  # tamaño de localidad de más 100,000 hb
  dplyr::group_by(sjlabelled::as_label(clase_hog), as_label(sexo_jefe)) |> # generando
  dplyr::summarise(media_ingreso = mean(ing_cor/3, na.rm = T),
                   media_ponderada_ing= weighted.mean(ing_cor/3, na.rm = T, w = factor),
                   mediana_ingresos = median(ing_cor/3, na.rm = T),
                   sd_ingresos = sd(ing_cor/3, na.rm = T), 
                   media_integrantes = mean(tot_integ, na.rm = T))


list_resultados<- list("tab_sexo" = tab, 
                       "ingresos" = tab_estadisticos)

writexl::write_xlsx(list_resultados, path = "resultados.xlsx")

## Histograma básico ----


hist(log(concentradohogar$ing_cor))


# Le podemos modificar el título del eje de las x y de las y


hist(concentradohogar$ing_cor, 
     main="Histograma de escolaridad", 
     xlab="Ingresos trimestrales", ylab="Frecuencia") 


# ¡A ponerle colorcitos! Aquí hay una lista <https://r-charts.com/es/colores/>
  
  
hist(concentradohogar$ing_cor, 
     main="Histograma de escolaridad",
     xlab="Años aprobados", 
     ylab="Frecuencia", col = "deeppink4" ) 


# Con pipes:
  
  
concentradohogar %>% 
  with(hist(ing_cor)) # con with, para que entienda


concentradohogar %>% 
  filter(!is.na(ing_cor)) %>% # la ventaja de esta forma es que podemos hacer más operaciones
  with(hist(ing_cor, main= "histograma"))


box <-boxplot(concentradohogar$ing_cor)

box

## Recodificación de variables ----

### `dplyr::if_else()` ----


#concentradohogar<- concentradohogar %>% 

concentradohogar %<>%
  dplyr::mutate(joven=dplyr::if_else(edad_jefe<30, # la condición a cumplir
                                     "joven", # el valor que toma si es verdadero
                                     "no joven")) # el valor que toma si es falso

concentradohogar %>% 
  janitor::tabyl(joven)



### `dplyr::case_when()` ----


concentradohogar %<>% 
  mutate(grupo_edad2=dplyr::case_when(edad_jefe<30                ~ 1,
                                      edad_jefe>29 & edad_jefe<45 ~ 2,
                                      edad_jefe>44 & edad_jefe<65 ~ 3,
                                      edad_jefe>64                ~ 4))

#TRUE~ 4

concentradohogar %>% tabyl(edad_jefe,grupo_edad2)



### `dplyr::rename()` -----


concentradohogar %<>%
  dplyr::rename(nuevo_nombre = grupo_edad2) # nuevo nombre va primero

names(concentradohogar)
# Esto en base sería similar a

names(concentradohogar)[128]<-"grupo_edad2"
names(concentradohogar)

nombres_originales<-names(concentradohogar)


## at ----
### summarise -----

concentradohogar |> 
  dplyr::summarise_at(vars(ing_cor:remu_espec), ~ mean(.x))

concentradohogar |> 
  dplyr::summarise_at(vars(ing_cor:remu_espec), ~ weighted.mean(.x, w=factor))

### mutate -----

concentradohogar |> 
  dplyr::mutate_at(vars(clase_hog, sexo_jefe), ~ as_label(.x)) |> 
  tabyl(clase_hog, sexo_jefe)


### rename ----


paste("hola","como estás")
paste0("hola","como estás")

concentradohogar |> 
  dplyr::rename_at(vars(clase_hog, sexo_jefe), ~ paste0(.x,"_2022" ) ) |> 
  names()

concentradohogar |> 
  dplyr::rename_at(vars(clase_hog, sexo_jefe), ~ paste0("p_", .x , "_2022") ) |> 
  names()


stringr::str_remove_all("yo quiero ir a la playa", "a")


concentradohogar |> 
  dplyr::rename_at(vars(clase_hog, sexo_jefe), ~ stringr::str_remove_all(.x, "o") ) |> 
  names()


stringr::str_replace_all("yo quiero ir a la playa", "a", "o")


concentradohogar |> 
  dplyr::rename_at(vars(clase_hog, sexo_jefe), ~ stringr::str_replace_all(.x, "o", "a") ) |> 
  names()


## Creación de variables por intervalos  ----


### Con cut() ----

concentradohogar |> 
  dplyr::mutate(grupo=cut(edad_jefe, 
                          breaks = c(0, 25, 50, 75, 100), 
                          include.lowest = T,
                          right = F)) |> 
  tabyl(grupo)





concentradohogar |> 
  dplyr::mutate(grupo=cut(edad_jefe, 
                          breaks = seq(0, 110, 5), 
                          include.lowest = T,
                          right = F)) |> 
  tabyl(grupo)

## quintiles, deciles, etc ----

concentradohogar |> 
  dplyr::mutate(quintil = ntile(ing_cor, n = 5)) |>
  tabyl(quintil)
                  


concentradohogar |> 
  dplyr::mutate(quintil = dineq::ntiles.wtd(ing_cor, n = 5, weights=factor )) |>
  pollster::topline(quintil, weight = factor)


## gt ----

tab |> 
  gt::gt() |> 
  tab_header(title = "Jefes y jefas de hogar en la muestra") |> 
  tab_footnote(footnote = "Datos no ponderados")


concentradohogar |> 
  dplyr::mutate_at(vars(clase_hog, sexo_jefe), ~ as_label(.x)) |> 
  janitor::tabyl(clase_hog, sexo_jefe) |> 
  janitor::adorn_totals(where = c("row", "col")) |> 
  janitor::adorn_percentages(denominator="col") |> 
  janitor::adorn_pct_formatting() |> 
  gt::gt() |> # grammar of tables es de formato
  tab_header(title = "Jefes y jefas de hogar en la muestra") |> 
  tab_footnote(footnote = "Datos no ponderados")


#concentradohogar <- read_sav("datos/concentradohogar.sav", encoding = "latin1")
