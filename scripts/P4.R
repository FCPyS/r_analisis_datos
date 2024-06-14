###############################################################################-
# Práctica 4: Revisión de elementos estadísticos básicos desde {tidyverse}
# Autora: Ana Escoto
# Fecha: 2024-06-13
###############################################################################-

## Paquetes ----

if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse,
               writexl, 
               haven,
               sjlabelled, 
               janitor,
               magrittr,
               GGally,
               wesanderson,
               RColorBrewer,
               viridis,
               ggthemes,
               esquisse
)


## Fusionado de conjuntos de datos ----

viviendas <- haven::read_sav("datos/viviendas.sav")
concentradohogar <- haven::read_sav("datos/concentradohogar.sav")
poblacion<- haven::read_sav("datos/poblacion.sav")

### Juntando bases ----


dim(viviendas)
names(viviendas[,1:15])

dim(concentradohogar)
names(concentradohogar[,1:15])

dim(poblacion)
names(poblacion[,1:15])

# Identificador único es "folioviv"

merge_data<- merge(viviendas, # dataframe x
                   concentradohogar, # daframe y
                   by="folioviv")



#Esto es equivalente a `left_join()` de `{dplyr}`


merge_data <- dplyr::inner_join(viviendas, #dataframe left
                               concentradohogar, #dataframe right
                               by="folioviv") 

merge_data <- viviendas %>% #dataframe left
  dplyr::left_join(concentradohogar, by="folioviv") 


names(merge_data)
dim(merge_data)

### Merge con id compuesto ----

viviendas %>% 
  select(folioviv) |> 
  janitor::get_dupes()

concentradohogar %>% 
  select(folioviv, foliohog) |> 
  janitor::get_dupes()

poblacion %>% 
  select(folioviv, foliohog, numren) |> 
  janitor::get_dupes()


merge_data2<- merge(concentradohogar, 
                    poblacion, 
                    by = c("folioviv", "foliohog"), 
                    all = FALSE)
dim(merge_data2)


merge_data2 %>% 
  tail()

names(concentradohogar)
names(poblacion)

names(merge_data2)

intersect(names(concentradohogar),
          names(poblacion))


merge_data2 <- concentradohogar |> 
  dplyr::left_join(poblacion) # si no pongo el by, R toma la intersección de nombres


### Bases de distinto tamaño ----

rm(merge_data, merge_data2) # botamos otros ejemplos

ingresos<- haven::read_sav("datos/ingresos.sav")

ingresos %>% 
  select(folioviv, foliohog, numren, clave) |> 
  janitor::get_dupes()

ingresos %>% 
  tabyl(clave)

rm(ingresos)

ingresos_sueldos<-ingresos %>% 
  filter(clave=="P001") # sueldos

dim(ingresos_sueldos)


merge_data3<-merge(poblacion, 
                   ingresos_sueldos, 
                   by=c("folioviv", "foliohog", "numren"))

dim(merge_data3)

### Cuatro formas de hacer un fusionado ----
  

#### Casos en ambas bases ----


merge_data3<-merge(poblacion, 
                   ingresos_sueldos,
                   by=c("folioviv", "foliohog", "numren"), 
                   all = F)
dim(merge_data3)


#### Todos los casos ----


merge_data3<-merge(poblacion,
                   ingresos_sueldos, 
                   by=c("folioviv", "foliohog", "numren"), 
                   all = T)

dim(merge_data3)


#### Casos en la base 1 ----


merge_data3<-merge(poblacion, 
                   ingresos_sueldos,
                   by=c("folioviv", "foliohog", "numren"), 
                   all.x  = TRUE)

dim(merge_data3)


#### Casos de la base 2 ----



merge_data3<-merge(poblacion, 
                   ingresos_sueldos,
                   by=c("folioviv", "foliohog", "numren"),
                   all.y  = TRUE)
dim(merge_data3)


### Las cuatro formas en `dplyr` ----

merge_data3<-dplyr::inner_join(poblacion,
                               ingresos_sueldos,
                               by=c("folioviv", "foliohog", "numren"))
dim(merge_data3)

  
merge_data3<-dplyr::full_join(poblacion, 
                              ingresos_sueldos, 
                              by=c("folioviv", "foliohog", "numren"))
dim(merge_data3)


merge_data3<-dplyr::left_join(poblacion,
                              ingresos_sueldos, 
                              by=c("folioviv", "foliohog", "numren"))
dim(merge_data3)


  
merge_data3<-dplyr::right_join(poblacion, 
                               ingresos_sueldos,
                               by=c("folioviv", "foliohog", "numren"))
dim(merge_data3)



merge_data3<-poblacion %>% # pongo el conjunto que será la "izquierda
  dplyr::right_join(ingresos_sueldos, 
                    by=c("folioviv", "foliohog", "numren"))
dim(merge_data3)


# EJEMPLO  ENCUCI 2020  ----
# https://www.dropbox.com/scl/fo/3pdbpjgi0cctnf7y2lcin/ANBL55zYyHHqAAcpHsuON9A?rlkey=m3zvod05sijx9ufzfp0meelmg&dl=0

encuci_viv <- foreign::read.dbf("BD_ENCUCI2020_dbf/ENCUCI_2020_VIV.dbf")
encuci_sd <- foreign::read.dbf("BD_ENCUCI2020_dbf/ENCUCI_2020_SD.dbf")
encuci_sec4_5 <- foreign::read.dbf("BD_ENCUCI2020_dbf/ENCUCI_2020_SEC_4_5.dbf")

encuci_sec6_7_8 <- foreign::read.dbf("BD_ENCUCI2020_dbf/ENCUCI_2020_SEC_6_7_8.dbf")

encuci<-encuci_viv |> 
  dplyr::left_join(encuci_s) |> 
  dplyr::left_join(encuci_sec4_5)

names(encuci)

encuci |> 
  pollster::topline(SEXO, weight = FAC_VIV)


encuci |> 
  pollster::topline(SEXO, weight = FAC_SEL)

encuci |> 
  filter(!is.na(FAC_SEL)) |> 
  select(FAC_VIV, FAC_SEL)

rm(list=ls()[-1]) # depende del objeto donde esté en la lista

## Visualización de datos ----


# Incluimos los cambios anteriores al conjunto de datos


etiqueta_sex<-c("Hombre", "Mujer")

concentradohogar <- concentradohogar %>% 
  mutate(sexo_jefe=as_numeric(sexo_jefe)) %>% # para quitar el "string"
  sjlabelled::set_labels(sexo_jefe, labels=etiqueta_sex) %>% 
  mutate(clase_hog=as_numeric(clase_hog)) %>% # para quitar el "string"
  sjlabelled::set_labels(clase_hog, labels=c("unipersonal",
                                             "nuclear", 
                                             "ampliado",
                                             "compuesto",
                                             "corresidente")) %>% 
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
                      "Posgrado")) %>% 
  mutate(ent=stringr::str_sub(folioviv, start=1, end=2 ))





### ¿Cuál es el mejor gráfico?


#Me gusta mucho este recurso: <https://www.data-to-viz.com/>

### Gráficos de `{graphics}` ----
  
# "plot()" Es la función más simple. Y es otro comando "listo"


plot(as_label(concentradohogar$sexo_jefe))


#Esto es igual que:
  
  
barplot(table(as_label(concentradohogar$sexo_jefe)))


### Gráficos con `{ggplot2}`  ----

#### Un lienzo para dibujar ----

# Para hacer un gráfico, `{ggplot2}` tiene el comando `ggplot()`.  Hacer gráficos con esta función tiene una lógica  aditiva. Lo ideal es que iniciemos estableciendo el mapeo estético de nuestro gráfico, con el comando aes()


concentradohogar %>%
  select(sexo_jefe, clase_hog, ing_cor, edad_jefe, educa_jefe) |> 
  ggplot2::ggplot() + # ojo es un +
  aes(x= as_label(sexo_jefe))

g1<-concentradohogar %>%
  select(sexo_jefe, clase_hog, ing_cor, edad_jefe, educa_jefe) |> 
  ggplot2::ggplot() + # ojo es un +
  aes(x= as_label(sexo_jefe))

g1 ## imprime el lienzo


g1$data


### Gráficos univariados

#### Para cualitativas


g1 +
  geom_bar()

g1 +  geom_bar(aes(fill = as_label(sexo_jefe))) ## colorea la geometría

## Esto es equivalente

concentradohogar %>%
  ggplot2::ggplot() +
  aes(x = as_label(sexo_jefe),
      fill = as_label(sexo_jefe)) +
    geom_bar()



#### Para variables cuantitativas ----
# Podemos hacer histogramas y gráficos de densidad, de manera fácil. La idea es agregar en nuestro "lienzo" una geometría, un valor para dibujar en él. Esto se agrega con un "+" y con la figura que se añadirá a nuestro gráfico.


concentradohogar %>%
 ggplot2::ggplot() +
  aes(x = ing_cor)
 ## imprime el lienzo


#### Histograma


concentradohogar %>%
  ggplot2::ggplot() +
  aes(x = ing_cor) +
  geom_histogram(bins = 10) 



#### Densidad


concentradohogar %>%
  ggplot2::ggplot() +
  aes(x = log(ing_cor/3)) +
  geom_density()


#### Gráficos bivariados ----

##### Cuali-cuali

concentradohogar |> 
  ggplot2::ggplot() +
  aes(x = as_label(sexo_jefe)) +
  geom_bar()

  

concentradohogar |> 
  ggplot2::ggplot() +
  aes(x = as_label(sexo_jefe)) +
  geom_bar(aes(fill = as_label(clase_hog)))


concentradohogar |> 
  ggplot2::ggplot() +
  aes(x = as_label(sexo_jefe)) +
  geom_bar(aes(fill = as_label(clase_hog)),
                 position="dodge") ## cada categoría "llena" a una unidad



g_bivariado <-concentradohogar |> 
  ggplot2::ggplot() +
  aes(y = as_label(sexo_jefe)) +
  geom_bar(aes(fill = as_label(clase_hog)),
           position="dodge") ## cada categoría "llena" a una unidad

g_bivariado

### Escalas de color ----

#### `{RcolorBrewer}` ----


RColorBrewer::display.brewer.all()


g_bivariado + scale_fill_brewer(palette = "Accent")


#### `{viridis}` ----

g_bivariado + scale_fill_viridis_d()

#### `{wesanderson}` ----

wesanderson::wes_palettes

wesanderson::wes_palette("IsleofDogs1", n=5)
mi_paleta <- wesanderson::wes_palette("IsleofDogs1", n=5)


g_bivariado + scale_fill_manual(values=mi_paleta)


### Temas ----

  
g_bivariado + 
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal()



g_bivariado + 
  scale_fill_brewer(palette = "Dark2") + 
  ggthemes::theme_stata()


#Si queremos que sean horizontales (como debe ser) 

g_bivariado + 
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  coord_flip()


subset <- concentradohogar |> 
  dplyr::mutate(sexo_jefe=as_label(sexo_jefe)) |> 
  select(sexo_jefe, educa_jefe, ing_cor)

### Un atajo `{esquisse}` ----

# Si la lógica de las capas y lienzos  parece un poco complicada para graficar con `{ggplot2}`, el paquete "esquisse" tiene un complemento. Cuando lo instalamos con pacman más arriba, se puede observar el complemento:

ggplot(subset) +
  aes(x = sexo_jefe) +
  geom_bar(fill = "#FF69B4") +
  labs(y = "Frecuencia", title = "Título de mi gráfico") +
  coord_flip() +
  ggthemes::theme_economist()

  
## Ejercicio
  
#¡Junta tu base!
  