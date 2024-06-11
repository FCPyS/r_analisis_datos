# hola esta es mi primera práctica
2 + 2 # Esta es suma 
2 + 2

5 * 3 # esta es una multiplicación
10 / 5 

1:5 # una secuencia entera

seq(1, 10, 0.5) # secuencia no entera

c('a', "b", "c", "d") # esto es un conjunto o un vector de caracter

40 < 80 # resulta el valor lógico TRUE

2 + 2 == 5 # resulta el valor lógico FALSE

TRUE == T

FALSE == F

# Declarando objetos

x <- 24
x/2
print(x)

24 -> x # No se hace por convención
x <- TRUE

pluscuamperfecto <- "hola"
saludo <- "hola"
Saludo <- "Buenas tardes"

#remove(saludo, Saludo, pluscuamperfecto)
rm(saludo, Saludo, pluscuamperfecto)

# Vectores

y <- c(2, 4, 6)
y <- c("Primaria", "Secundaria")

y[2]

y[3] <- "Preparatoria"

y

sex <- 1:2
sex
names(sex) <- c("Hombre", "Mujer")
names(sex)
sex

sex[3]<- 3

sex
names(sex)[3] <-  "no binario"

sex


# Matrices 

m <- matrix(nrow = 2, 
            ncol = 3,
            1:6,
            byrow = T)
m

m <- matrix(nrow = 2, 
            ncol = 3,
            1:6,
            byrow = F)

m

dim(m)


attributes(m)


n <- 1:6
dim(n)
dim(n) <- c(2, 3)

xx <- 10:12
yy <- 14:16

cbind(xx, yy) # esta matriz no existe como objeto porque no la he declarado
rbind(xx, yy)
mi_matriz<-rbind(xx, yy)

m[1, 3]
m[1, ]
m[ , 3]

# Dataframes

class(m)
class(x)
class(sex)

m <- as.data.frame(m)

h <- x
assign("h", x)


m$V2 # es el símbolo de peso
m[1,]

cbind(m, c("a", "b"), c(T,F)) # dataframe
cbind(n, c("a", "b"), c(T,F)) # matriz

data <- data.frame(
  "entero" = 1:4, 
  "factor" = as.factor(c("a", "b", "c", "d")),
  "numero" = c(1/1, 1/2, 1/3, 1/4),
  "cadena" = c("a", "b", "c", "d")
)

str(data)

# Valores perdidos

vector <- c(1:5, #numérica
            T, 
            "a", 
            5/0,
            sqrt(-1))

vector


vector <- c(1:5, #numérica
            T, 
            NA,
            5/0,
            sqrt(-1))

vector

is.na(vector)
is.nan(vector) # not a number
is.infinite(vector)

Inf + 3
NA *+ 3


# Funciones 

sum(1:5)
sum(10, 20, 40)
sum(10, NA, 20)

sum(1, Inf, T)

sum(1:5, NA, na.rm = TRUE)

example(sum)


# Lista 

milista <- list(data, n, xx, x)

milista

milista <- list("datos" = data,
                "matriz" = n, 
                "vector" = xx,
                "valor" = x)


milista
milista$datos



# Ambiente

ls() # list objetc
gc() # garbage collection

rm(list = ls()) # borra todos los objetos

# Directorios de trabajo

getwd()

list.files()

list.files("Movies/") # entrar a las carpetas
list.files("../") # con .. podemos entrar a un nivel superior al que estamos


setwd("Desktop/")
getwd()

# Paquetes 

install.packages("foreign") # paquetes se hace una vez
library(foreign) # carga los paquetes, cada vez que abramos R

ejemplo_dbf <- foreign::read.dbf(file = "datos/ejemplo_dbf.dbf")

# Pacman C<

if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse,
               readxl,
               writexl,
               haven, 
               sjlabelled, 
               foreign) #carga los paquetes necesarios para esta práctica





