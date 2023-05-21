# Función para calcular la probabilidad de compartir cumpleaños en n personas
n <- 50
compute_prob <- function(n, B = 10000) {
    same_day <- replicate(B, {
        bdays <- sample(1:365, n, replace = TRUE)
        any(duplicated(bdays))
    })
    mean(same_day)
}
prob <- sapply(n, compute_prob)
plot(n, prob)
# función para calcular la probabilidad exacta de cumpleaños compartidos para cualquier n
n <- seq(1, 60)
exact_prob <- function(n) {
    prob_unique <- seq(365, 365 - n + 1) / 365
    1 - prod(prob_unique) # Calcular la probabilidad de no compartir cumpleaños y restarlo a 1
}

# aplicando la función n-veces
eprob <- sapply(n, exact_prob)

# Graficando los resultados de la simulacion de monte carlo y exactos en la misma grafica
plot(n, prob) # Gráfica de monte Carlo
lines(n, eprob, col = "red") # añadiendo los resultados exactos
