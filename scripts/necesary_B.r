B <- 10^seq(1, 5, len = 100) # definimos el vector con valores de B
compute_prob <- function(B, n = 22) { # corremos la funcion para cada B
    same_day <- replicate(B, {
        bdays <- sample(1:365, n, replace = TRUE)
        any(duplicated(bdays))
    })
    mean(same_day)
}
# aplicamos compute_prob a varios valores de B
prob <- sapply(B, compute_prob)
# Graficamos la linea de las estimaciones
plot(log10(B), prob, type = "l")
