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


### Recordemos nuestro etiquetado


etiqueta_sex<-c("Hombre", "Mujer")

concentradohogar<-concentradohogar %>% 
  mutate(sexo_jefe=as_numeric(sexo_jefe)) %>% # para quitar el "string"
  sjlabelled::set_labels(sexo_jefe, labels=etiqueta_sex) 


concentradohogar<-concentradohogar %>% 
  mutate(clase_hog=as_numeric(clase_hog)) %>% # para quitar el "string"
  sjlabelled::set_labels(clase_hog, labels=c("unipersonal",
                                             "nuclear", 
                                             "ampliado",
                                             "compuesto",
                                             "corresidente")) 



# Con "tabyl()"" de "janitor""


concentradohogar %>%
  dplyr::mutate(sexo_jefe=as_label(sexo_jefe))  %>%
  janitor::tabyl(sexo_jefe)

  
concentradohogar %>% 
  dplyr::mutate(sexo_jefe=as_label(sexo_jefe))  %>%
  janitor::tabyl(sexo_jefe) %>% 
  janitor::adorn_totals()



class(concentradohogar$sexo_jefe) # variable sin etiqueta
class(as_label(concentradohogar$sexo_jefe)) # variable con etiqueta

class(as_label(concentradohogar$educa_jefe)) # variable ordinal
class(concentradohogar$ing_cor) # variable de intervalo/razón

  
dplyr::glimpse(concentradohogar$sexo_jefe)



concentradohogar %>% mutate(sexo_jefe=as_label(sexo_jefe)) %>% # cambia los valores de la variable a sus etiquetas
  tabyl(sexo_jefe) %>% # para hacer la tabla
  adorn_totals() %>% # añade totales
  adorn_pct_formatting()  # nos da porcentaje en lugar de proporción

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
  tabyl(educa_jefe)


# Para que no nos salgan las categorías sin datos podemos *apagar* la opción `show_missing_levels=F` dentro del comando "tabyl()"


concentradohogar %>% 
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
  tabyl(clase_hog, sexo_jefe, show_missing_levels=F ) %>% # incluimos aquí 
  adorn_totals()  


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
  adorn_totals(c("col", "row")) 
  
  
concentradohogar %>% 
  mutate(clase_hog = as_label(clase_hog)) %>% 
  mutate(sexo_jefe = as_label(sexo_jefe)) %>% # para que las lea como factor
  tabyl(clase_hog, sexo_jefe, show_missing_levels = F ) %>% # incluimos aquí dos variable
  adorn_totals(c("col", "row")) %>% 
  adorn_percentages("col") %>% # Divide los valores entre el total de la columna
  adorn_pct_formatting() # lo vuelve porcentaje

  
concentradohogar %>% 
  mutate(clase_hog = as_label(clase_hog)) %>% 
  mutate(sexo_jefe = as_label(sexo_jefe)) %>% # para que las lea como factor
  tabyl(clase_hog, sexo_jefe, show_missing_levels = F ) %>% # incluimos aquí dos variable
  adorn_totals(c("col", "row")) %>% 
  adorn_percentages("row") %>% # Divide los valores entre el total de la fila
  adorn_pct_formatting() # lo vuelve porcentaje


concentradohogar %>% 
  mutate(clase_hog = as_label(clase_hog)) %>% 
  mutate(sexo_jefe = as_label(sexo_jefe)) %>% # para que las lea como factor
  tabyl(clase_hog, sexo_jefe, show_missing_levels = F ) %>% # incluimos aquí dos variable
  adorn_totals(c("col", "row")) %>% 
  adorn_percentages("all") %>% # Divide los valores entre el total de la población
  adorn_pct_formatting() # lo vuelve porcentaje


# Factores de expansión y algunas otras medidas ----

## La función `tally()` ----


concentradohogar %>% 
  group_by(as_label(sexo_jefe)) %>% 
  tally(factor) %>% #nombre del factor
  adorn_totals()  # Agrega total


# Podemos usar funciones de `adorns...` de \`{janitor}


concentradohogar %>% 
  group_by(as_label(sexo_jefe)) %>% 
  tally(factor) %>% #nombre del factor
  adorn_totals() %>% # Agrega total
  adorn_percentages("all")  %>% 
  adorn_pct_formatting()


## Con `dplyr::count()` ----

# La función `count()` también permite dar pesos a la operaciones de frecuencias, con el argumento `wt =`


concentradohogar %>% 
  count(sexo_jefe, clase_hog,  wt = factor) 



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
  pollster::topline(sexo_jefe , weight = factor)


# Para dos variables

concentradohogar %>% 
  dplyr::mutate(sexo_jefe = sjlabelled::as_label(sexo_jefe)) %>% 
  dplyr::mutate(clase_hog = sjlabelled::as_label(clase_hog)) %>% 
  pollster::crosstab(sexo_jefe, clase_hog, weight = factor)




# Descriptivos para variables cuantitativas ----

## Medidas numéricas básicas ----

# 5 números

summary(concentradohogar$ing_cor) ## educación


# Con pipes se pueden crear "indicadores" de nuestras variables es un tibble


concentradohogar %>% 
  summarise(nombre_indicador=mean(ing_cor, na.rm=T))


## Histograma básico ----


hist(concentradohogar$ing_cor)


# Le podemos modificar el título del eje de las x y de las y


hist(concentradohogar$ing_cor, 
     main="Histograma de escolaridad", 
     xlab="Años aprobados", ylab="Frecuencia") 


# ¡A ponerle colorcitos! Aquí hay una lista <https://r-charts.com/es/colores/>
  
  
hist(concentradohogar$ing_cor, 
     main="Histograma de escolaridad",
     xlab="Años aprobados", 
     ylab="Frecuencia", col="deeppink1") 


# Con pipes:
  
  
concentradohogar %>% 
  with(hist(ing_cor)) # con with, para que entienda


concentradohogar %>% 
  filter(!is.na(ing_cor)) %>% # la ventaja de esta forma es que podemos hacer más operaciones
  with(hist(ing_cor, main= "histograma"))


## Recodificación de variables

### `dplyr::if_else()`


concentradohogar %<>% 
  mutate(joven=dplyr::if_else(edad_jefe<30, 1, 0))

concentradohogar %>% tabyl(edad_jefe,joven)



### `dplyr::case_when()`


concentradohogar %<>% 
  mutate(grupo_edad2=dplyr::case_when(edad_jefe<30 ~ 1,
                                      edad_jefe>29 & edad_jefe<45 ~ 2,
                                      edad_jefe>44 & edad_jefe<65 ~ 3,
                                      edad_jefe>64 ~ 4))

#TRUE~ 4

concentradohogar %>% tabyl(edad_jefe,grupo_edad2)



### `dplyr::rename()`


concentradohogar %<>%
  dplyr::rename(nuevo_nombre=grupo_edad2)


# Esto en base sería similar a

names(concentradohogar)[128]<-"grupo_edad2"
names(concentradohogar)




