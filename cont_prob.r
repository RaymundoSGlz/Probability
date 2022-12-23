#instalamos las paqueterias
# install.packages("dslabs")
#cargamos la librerias necesarias
library(tidyverse)
library(dslabs)
data(heights)
#extraemos las alturas en un vector
x <- heights %>% filter(sex == "Male") %>% pull(height)
#definimos la funcion de distribucion acumulada empirica
F <- function(a) mean(x <= a)

1 - F(70) #probabilidad de que un hombre tenga altura mayor a 70 in
