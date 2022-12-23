# verificando si dos personas tienen la misma fecha de cumpleaños
# en un grupo de 50
n <- 50
bdays <- sample(1:365, n, replace = TRUE)    # generamos n cumpleaños aleatorios
any(duplicated(bdays))    # verificamos si tenemos duplicados

# simulación de Monte Carlo con B=10000 repeticiones
B <- 10000
results <- replicate(B, {    # retorna un vector con B valores lógicos
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
})
mean(results)    # calcula la proporción de cumpleaños duplicados

#para cualquier tamaño de grupo 
#convertimos el calculo en una funcion
same_birthday <- function(n){
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
}
#creamos una tabla de consulta
compute_prob <- function(n, B = 10000){
    results <- replicate(B, same_birthday(n))
    mean(results)
}
#utilizamos la función sapply
n <- seq(1, 60)
prob <- sapply(n, compute_prob)
#realizamos un gráfico
#install.packages("tidyverse") instalamos el paquete
library(tidyverse)
prob <- sapply(n, compute_prob)
qplot(n, prob)

#probabilidad exacta
exact_prob <- function(n){
    prob_unique <- seq(365,365-n+1)/365
    1 - prod( prob_unique)
}
eprob <- sapply(n, exact_prob)
qplot(n, prob) + geom_line(aes(n, eprob), col = "red")
