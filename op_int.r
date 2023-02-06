n <- 1000 #numero de prestamos realizados
per_hip <- -20000 # perdida por ejecución hipotecaria
p <- 0.02 #porcentaje de deudores
# Utilizamos las propiedades de la distribución normal
# para calcular la tasa de interés necesaria 
# para asegurar una cierta probabilidad de perder dinero 
# para una determinada probabilidad de incumplimiento.
incumplimiento <- sample(c(0,1), n, prob = c(1-p, p), replace = TRUE)
sum(incumplimiento * per_hip)

# Utilizando una simulación de Monte Carlo
B <- 10000
perdidas <- replicate(B, {
    incumplimiento <- sample(c(0, 1), n, prob = c(1-p, p), replace = TRUE)
    sum(incumplimiento * per_hip)
})

# graficamos las perdidas esperadas
library(tidyverse)
data.frame(perdidas_en_millones = perdidas/10^6) %>%
    ggplot(aes(perdidas_en_millones)) +
    geom_histogram(binwidth = 0.6, col = "black")

# Utilizando CTL
n*(p*per_hip + (1-p)*0)    # Valor esperado
sqrt(n)*abs(per_hip)*sqrt(p*(1-p))    # error estándar

#para alcanzar el equilibrio x debería ser
x_e <- -per_hip * p / (1 - p)
#interés de
x_e/180000

# Calculando mejor tasa de interés
l <- per_hip
z <- qnorm(0.01)
x <- -l * (n * p - z * sqrt(n * p * (1 - p)))/ (n * (1 - p)
+ z * sqrt(n * p * (1 - p)))
#interés
x / 180000

#ganancia esperada por préstamo
per_hip * p + x * (1 - p)    # Ganancia esperada por préstamo
n * (per_hip * p + x * (1 - p)) # ganancia esperada total

# Utilizamos Monte Carlo para verificar
B <- 100000
ganancia <- replicate(B, {
    sorteos <- sample( c(x, per_hip), n, 
                        prob=c(1-p, p), replace = TRUE) 
    sum(sorteos)
})
mean(ganancia)    # Ganancia esperada total
mean(ganancia<0) # Probabilidad de perder dinero
