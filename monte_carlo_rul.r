# Asigna una variable `p_green` como la probabilidad de que la bola caiga en un bolsillo verde
p_green <- 2 / 38

# Asigna una variable `p_not_green` como la probabilidad de que la bola no caiga en un bolsillo verde
p_not_green <- 1 - p_green

# Asigna una variable `n` como el número de apuestas
n <- 100

# La variable `B` especifica el número de veces que queremos que se ejecute la simulación de Monte Carlo.
# Ejecutemos la simulación de Monte Carlo 10,000 veces.
B <- 10000

# Usamos la función `set.seed` para asegurarnos de que su respuesta coincida con el resultado esperado después de la muestreo aleatorio.
set.seed(1)

# Creamos un objeto llamado `S` que replica el código de muestra para `B` iteraciones y suma los resultados.
S <- replicate(B, {
    X <- sample(c(17, -1), n, replace = TRUE, prob = c(p_green, p_not_green))
    sum(X)
})

# Imprime la media de `S` en la consola.
mean(S)

# Imprime la desviación estándar de 'S' en la consola.
sd(S)
