library(tidyverse)
library(dslabs)
data(heights)
#extraemos las alturas en un vector
x <- heights %>% filter(sex == "Male") %>% pull(height)
#estimamos la probabilidad
1 - pnorm(70.5, mean(x), sd(x))
#grafica de la distribucion de probabilidad
plot(prop.table(table(x)), xlab = "a = Altura en pulgadas", ylab = "P(x = a)")
# Probabilidades de los datos actuales donde las distancioas contienen un entero
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)
# Probabilidades con la aproximacion normal
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))
# probabilidades en otros rangos
mean(x <= 70.9) - mean(x <= 70.1)
#no coinciden tan bien
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))
