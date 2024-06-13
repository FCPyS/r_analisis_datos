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


merge_data<- merge(viviendas,
                   concentradohogar,
                   by="folioviv")



#Esto es equivalente a `left_join()` de `{dplyr}`


merge_data <- dplyr::left_join(viviendas,
                               concentradohogar, 
                               by="folioviv") 

merge_data <- viviendas %>% 
  dplyr::left_join(concentradohogar, by="folioviv") 


names(merge_data)
dim(merge_data)

### Merge con id compuesto ----

viviendas %>% 
  janitor::get_dupes(folioviv)

concentradohogar %>% 
  janitor::get_dupes(c(folioviv, foliohog))

poblacion %>% 
  janitor::get_dupes(c(folioviv, foliohog, numren))



merge_data2<- merge(concentradohogar, 
                    poblacion, 
                    by = c("folioviv", "foliohog"))
dim(merge_data2)


merge_data2 %>% 
  tail()


### Bases de distinto tamaño ----

rm(merge_data, merge_data2) # botamos otros ejemplos

ingresos<- haven::read_sav("datos/ingresos.sav")

ingresos %>% 
  janitor::get_dupes(c(folioviv, foliohog, numren, clave))

ingresos %>% 
  tabyl(clave)


ingresos_sueldos<-ingresos %>% 
  filter(clave=="P001") 

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
  ggplot2::ggplot() + # ojo es un +
  aes(x= as_label(sexo_jefe))

g1<-concentradohogar %>%
  ggplot2::ggplot() + # ojo es un +
  aes(x= as_label(sexo_jefe))

g1 ## imprime el lienzo



### Gráficos univariados

#### Para cualitativas


g1 +
  geom_bar()

g1 +  geom_bar(aes(
  fill = as_label(sexo_jefe)
)) ## colorea la geometría

## Esto es equivalente

concentradohogar %>%
  ggplot(aes(as_label(sexo_jefe),
             fill = as_label(sexo_jefe)
  )
  ) + geom_bar()



#### Para variables cuantitativas ----
# Podemos hacer histogramas y gráficos de densidad, de manera fácil. La idea es agregar en nuestro "lienzo" una geometría, un valor para dibujar en él. Esto se agrega con un "+" y con la figura que se añadirá a nuestro gráfico.


g2<-concentradohogar %>%
  ggplot(aes(ing_cor))

g2 ## imprime el lienzo


#### Histograma


g2 + geom_histogram() 



#### Densidad

g2 + geom_density()



#### Gráficos bivariados ----

##### Cuali-cuali



g1 +  geom_bar(aes(fill = as_label(clase_hog)),
               position="dodge") #pone las categorías lado a lado y no apiladas

  
g_bivariado <- g1 +  
  geom_bar(aes(fill = as_label(clase_hog)),
           position="fill") ## cada categoría "llena" a una unidad

g_bivariado


### Escalas de color ----

#### `{RcolorBrewer}` ----


RColorBrewer::display.brewer.all()


g_bivariado + scale_fill_brewer(palette = "Dark2")


#### `{viridis}` ----

g_bivariado + scale_fill_viridis_d()

#### `{wesanderson}` ----

wesanderson::wes_palettes

wesanderson::wes_palette("IsleofDogs1", n=5)
mi_paleta <- wesanderson::wes_palette("IsleofDogs1", n=5)


g_bivariado + scale_fill_manual(values=mi_paleta)


### Temas ----

  
g_bivariado + scale_fill_brewer(palette = "Dark2") + theme_minimal()


#Si queremos que sean horizontales (como debe ser) 

g_bivariado + 
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  coord_flip()


### Un atajo `{esquisse}` ----

# Si la lógica de las capas y lienzos  parece un poco complicada para graficar con `{ggplot2}`, el paquete "esquisse" tiene un complemento. Cuando lo instalamos con pacman más arriba, se puede observar el complemento:
  
## Ejercicio
  
#¡Junta tu base!
  