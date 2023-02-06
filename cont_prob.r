#instalamos las paqueterías
# install.packages("dslabs")
#cargamos la librerías necesarias
library(tidyverse)
library(dslabs)
data(heights)
#extraemos las alturas en un vector
x <- heights %>% filter(sex == "Male") %>% pull(height)
#definimos la función de distribución acumulada empírica
F <- function(a) mean(x <= a)

1 - F(70) #probabilidad de que un hombre tenga altura mayor a 70 in
