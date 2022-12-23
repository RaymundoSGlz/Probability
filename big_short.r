
p <- .04 #porcentaje de incumplimiento
per_hip <- -200000 # perdidas por ejecución hipotecaria
r <- 0.05 #beneficio para el banco
x <- r * 180000
per_hip * p + x * (1 - p) # Valor esperado

#buscando n para que la probabilidad de perper dinero sea 0.01
z <- qnorm(0.01)
l <- per_hip
n <- ceiling((z^2 * (x - l)^2 * p * (1 - p)) / (l * p + x * (1 - p))^2)
n    # numero de prestamos necesarios
n * (per_hip * p + x * (1 - p))    # ganancias esperadas con este n

# Utilizamos simulaciones Monte Carlo para verificar
B <- 10000
p <- 0.04
x <- 0.05 * 180000
ganancia <- replicate(B, {
    sorteos <- sample( c(x, per_hip), n, 
                        prob = c(1 - p, p), replace = TRUE)
    sum(sorteos)
})
mean(ganancia)

#caso realista
p <- 0.04
x <- 0.05*180000
ganancia_r <- replicate(B, {
    new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
    eventos <- sample(c(x, per_hip), n, 
                        prob = c(1 - new_p, new_p), replace = TRUE)
    sum(eventos)
})
mean(ganancia_r)    # ganancia esperada
mean(ganancia_r < 0)    # probabilidad de perder dinero
mean(ganancia_r < -10000000)    # probabilidad de perder mas de $10 millones

#graficamos para entenderlo mejor
library(tidyverse)
data.frame(ganancia_en_millones = ganancia_r / 10^6) %>%
ggplot(aes(ganancia_en_millones)) +
geom_histogram(color = "black", binwidth = 5)
