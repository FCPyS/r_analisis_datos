###############################################################################-
# Práctica 5: Visualización (II) y diseño muestral
# Autora: Ana Escoto
# Fecha: 2024-06-14
###############################################################################-


# Paquetes ----


if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse,
               broom, # ayudarnos a los formatos
               haven,
               sjlabelled, 
               janitor,
               magrittr,
               GGally, # es para graficos
               wesanderson,
               RColorBrewer,
               viridis,
               ggthemes,
               esquisse,
               survey, # Diseño muestral complejo
               srvyr) # Diseño muestral complejo


# Datos ----


concentradohogar <- haven::read_sav("datos/concentradohogar.sav") %>% 
  mutate(sexo_jefe=as_numeric(sexo_jefe)) %>% # para quitar el "string"
  sjlabelled::set_labels(sexo_jefe, labels=c("Hombre", "Mujer")) %>% 
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



# Gráficos bivariados cuanti - cuanti ----

## correlación ----


plot(concentradohogar$ing_cor, concentradohogar$gasto_mon)

concentradohogar |> 
  with(
   plot(ing_cor, gasto_mon) 
  )

cor(concentradohogar$ing_cor,
     concentradohogar$gasto_mon)



# Para los gráficos "cuanti-cuanti", iniciamos con un gráfico de dispersión o *scatter*
  
  
  ## *scatterplot* ----
  
  
concentradohogar %>% 
  filter(ent=="01") |> 
  ggplot() +
  aes(x = ing_cor,
      y = gasto_mon) +
  geom_point() #puntito



### geometría "jitter"----


concentradohogar %>% 
  ggplot() +
  aes(x = ing_cor,
      y = gasto_mon) +
  geom_jitter() #puntito pero separados


concentradohogar %>% 
  filter(ing_cor<2000000) |> 
  ggplot() +
  aes(x=log(ing_cor),
      y=log(gasto_mon),
      alpha= I(0.2))+ # para la transparencia
  geom_jitter() # puntito pero "separado"

concentradohogar %>%
  ggplot() +
  aes(x=ing_cor,
      y=gasto_mon, alpha= I(0.2)) +
  geom_text(aes(label=ent)) # un texto en lugar de punto


concentradohogar %>%
  dplyr::mutate(puntitoAGS=if_else(ent=="01", 
                                 "AGS", 
                                 "")) |> 
  ggplot() +
  aes(x=ing_cor,
      y=gasto_mon, alpha= I(0.2)) +
  geom_point()+
  geom_text(aes(label=puntitoAGS)) # un texto en lugar de punto



### geometría "label" ----

concentradohogar %>% 
  filter(ent=="01") |> 
  ggplot() +
  aes(x=ing_cor,
      y=gasto_mon) +
  geom_label(aes(label=ent)) # etiqueta


## Ya no es tan bivariado ----

# Con *color*

concentradohogar |>   
ggplot() + 
  aes(x=ing_cor, y = gasto_mon) +
  geom_point()
  

concentradohogar %>% 
  ggplot() + # de aqui adelante no hay pipes
  aes(x=log(ing_cor), # este es el eje de las x
      y=log(gasto_mon), # este es el eje de las y
      alpha=I(0.2), # cambia transparencia
      color=tam_loc) + # puntitos o líneas +
  geom_point()


# Con *shape*
  
  

concentradohogar %>% 
  ggplot() +
  aes(x=ing_cor,
      y=gasto_mon,
      shape=tam_loc) + # variables de hasta 6 categorías
  geom_point() # ojo, nos da un "warning"


## *facets* ----

# Con *facet_wrap*
  

concentradohogar %>% 
  ggplot() +
  aes(x=ing_cor,
      y=gasto_mon) +
  geom_point() +
  facet_wrap(~tam_loc)


concentradohogar %>% 
  ggplot() +
  aes(x=ing_cor,
      y=gasto_mon) +
  geom_point() +
  facet_wrap(vars(tam_loc))

# Con *facet_grid*, columna


concentradohogar %>% 
  ggplot() +
  aes(x=log(ing_cor),
             y=log(gasto_mon)) +
  geom_point() +
  facet_grid(. ~ tam_loc)



# Con *facet_wrap*, fila


concentradohogar %>% 
  ggplot() +
  aes(x=log(ing_cor),
      y=log(gasto_mon)) +
  geom_point() +
  facet_grid(tam_loc ~ .)



concentradohogar %>% 
  ggplot() +
  aes(x=log(ing_cor),
      y=log(gasto_mon)) +
  geom_point() +
  facet_grid(tam_loc ~ as_label(sexo_jefe))


## Ajustes *smooth* ----


concentradohogar %>% 
  ggplot() +
  aes(x = log(ing_cor),
      y = log(gasto_mon)) +
  geom_point(color = "gray") +
  geom_smooth(method="lm", color = "purple") + # linear model
  facet_grid(tam_loc ~.)


# Enchulando tantito:
  
concentradohogar %>% 
  ggplot(aes(x=ing_cor,
             y=gasto_mon,
             color=tam_loc)) +
  geom_text(aes(label=ent)) +
  geom_smooth(method="lm") + 
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()


## Una cuarta variable ----


concentradohogar %>% 
  ggplot() +
  aes(x=ing_cor,
      y=gasto_mon,
      color=tam_loc, 
      size=tot_integ) +
  geom_point() +# ojo una variable cuantitavia
  geom_smooth(method = "lm") +
  theme_minimal()


# No es lo mismo, pero es igual:
  
  
concentradohogar %>% 
  ggplot(aes(x=ing_cor,
             y=gasto_mon,
             color=tam_loc,
             size=tot_integ)) +
  geom_point()+ 
  theme_minimal()


# Algunas opciones interesantes:
  
concentradohogar %>% 
  ggplot(aes(x=ing_cor,
             y=gasto_mon,
             color=tam_loc,
             size=tot_integ)) +
  geom_text(aes(label=ent),
            check_overlap = TRUE)+
  theme_minimal()



## GGally pares ----

concentradohogar %>% 
  filter(ent=="01") %>%
  select(ing_cor, gasto_mon, tot_integ, percep_ing) %>%
  GGally::ggpairs()



# Diseño complejo ----


# Muestreo aleatorio ----

ags_srvy <- concentradohogar %>%
  filter(ent=="01") %>% 
  srvyr::as_survey_design(weights = factor)


ags_srvy |> 
  summarise(media = srvyr::survey_mean(ing_cor))

# Si revisamos las encuestas tiene un diseño complejo, hay estratos y unidades primarias de muestreo


# Muestreo estratificado

ags_srvy <- concentradohogar %>%
  filter(ent=="01") %>% 
  srvyr::as_survey_design(
    upm = upm,
    strata = est_dis,
    weights = factor,
    nest = TRUE)



# Aún así, vamos a seleccionar un par de variables:
  
  
# simple random sample
ags_srvy <- concentradohogar %>%
  filter(ent=="01") %>% 
  select(upm, est_dis, factor, clase_hog,
         sexo_jefe, edad_jefe, educa_jefe,ing_cor, factor) %>% 
  as_survey_design(
    upm=upm,
    strata = est_dis,
    weights = factor,
    nest = TRUE)



# Para una media ponderada


ags_srvy %>%
  #filter(ing_cor>0) %>% # sólo con ingresos
  summarise(
    media_ponderada = survey_mean(ing_cor, na.rm=T))



# Si queremos los intervalos de confianza (*spoiler*):
  
  
ags_srvy %>%
  summarize(
    media_ponderada = survey_mean(ing_cor,
                                  vartype = "cv") )


ags_srvy %>%
  summarize(
    mediana_ponderada = survey_median(ing_cor,
                                      vartype = "ci") )

ags_srvy |> 
  group_by(as_label(clase_hog)) |> 
  summarise(media_ingresos = survey_mean(ing_cor, 
                                         vartype = c("se", "cv", "ci")) )


lentes <- c(1, 0, 1, 0, 0)
mean(lentes)

ags_srvy %>%
  mutate(sexo_jefe=as_label(sexo_jefe)) %>% 
  group_by(sexo_jefe) %>% #variables cuali
  summarize(proportion = survey_mean(), # proporción
            total = survey_total() ) # totales


ags_srvy %>%
  mutate(sexo_jefe=as_label(sexo_jefe)) %>% 
  mutate(clase_hog=as_label(clase_hog)) %>% 
  group_by(interact(sexo_jefe, clase_hog)) %>% # para que todo sume 100% "all" "cell"
  summarize(prop = survey_mean())


ags_srvy %>%
  mutate(sexo_jefe=as_label(sexo_jefe)) %>% 
  mutate(clase_hog=as_label(clase_hog)) %>% 
  group_by(sexo_jefe, clase_hog) %>% # cada categoría de sexo suma 100
  summarize(prop = survey_mean(vartype = "ci"))


# 
# Más sobre este tipo de estimaciones [acá](https://tidy-survey-r.github.io/tidy-survey-short-course/Presentation/Slides-day-1.html#1
# )


ags_srvy %>%
  mutate(sexo_jefe=as_label(sexo_jefe)) %>% 
  mutate(clase_hog=as_label(clase_hog)) %>% 
  group_by(sexo_jefe, clase_hog) %>%  
  survey_count(
    vartype = c("se", "ci", "var", "cv")
  )


## Todo es una regresión ----

# Este tema es un poquito más avanzado. Siguiendo este [artículo](https://danielroelfs.com/blog/everything-is-a-linear-model/)


ags_srvy %>%
  mutate(sexo_jefe=as_label(sexo_jefe)) %>% 
  group_by(sexo_jefe) %>% 
  summarize(ing_cor_mu = survey_mean(ing_cor, vartype="ci")) 

results0<-ags_srvy %>%
  mutate(sexo_jefe=as_label(sexo_jefe)) %>% 
  group_by(sexo_jefe) %>% 
  summarize(ing_cor_mu = survey_mean(ing_cor, vartype="ci")) 

results0$ing_cor_mu[1]-results0$ing_cor_mu[2]


ags_srvy %>%
  mutate(sexo_jefe=as_label(sexo_jefe)) %>% 
  survey::svyglm(design=.,
         formula=ing_cor ~ sexo_jefe,
         family = gaussian()) %>%
  summary()

svyglm <-ags_srvy %>%
  mutate(sexo_jefe=as_label(sexo_jefe)) %>% 
  svyglm(design=.,
         formula=ing_cor ~ sexo_jefe,
         family = gaussian()) 


# El coeficiente coincide y tenemos el error estándar y podemos tener también los intervalos
# 
# Vamos a pedirle los intervalos

confint(svyglm)


## Con más categorías


ags_srvy %>%
  mutate(clase_hog=as_label(clase_hog)) %>% 
  group_by(clase_hog) %>% 
  summarize(ing_cor_mu = survey_mean(ing_cor, vartype="ci")) 


ags_srvy %>%
  mutate(clase_hog=as_label(clase_hog)) %>% 
  svyglm(design=.,
         formula=ing_cor ~ clase_hog,
         family = gaussian()) %>%
  summary()

svyglm2 <-ags_srvy %>%
  mutate(clase_hog=as_label(clase_hog)) %>% 
  svyglm(design=.,
         formula=ing_cor ~ clase_hog,
         family = gaussian()) 


# El coeficiente coincide y tenemos el error estándar y podemos tener también los intervalos
# 
# Vamos a pedirle los intervalos

confint(svyglm2)

# En formato tabular
broom::tidy(svyglm2, conf.int = TRUE) |> 
  mutate(cv = std.error/estimate * 100)



