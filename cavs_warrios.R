# Asignamos una variable 'n' como el número de juegos restantes.
n <- 6

# Asignamos una variable 'outcomes' como un vector de posibles resultados de juego,
# donde 0 indica una pérdida y 1 indica una victoria para los Cavs.
outcomes <- c(0, 1)

# Asignamos una variable 'l' a una lista de todos los posibles resultados en todos los juegos restantes.
# Use la función `rep` en `list(outcomes)` para crear una lista de longitud `n`.
l <- rep(list(outcomes), n)

# Creamos un data frame llamado 'posibilities' que contiene todas las combinaciones de resultados posibles para los juegos restantes.
possibilities <- expand.grid(l)

# Creamos un vector llamado 'results' que indica si cada fila en el data frame 'posibilities' contiene suficientes victorias para que los Cavs ganen la serie.
results <- rowSums(possibilities) >= 4

# Calculamos la proporción de 'results' en los que los Cavs ganan la serie. Imprimimos el resultado en la consola.
mean(results)


# La variable `B` especifica el número de veces que queremos que se ejecute la simulación de Monte Carlo.
# Ejecutemos la simulación de Monte Carlo 10,000 veces.
B <- 10000

# Usamos la función `set.seed` para asegurarnos de que su respuesta coincida con el resultado esperado después de la muestreo aleatorio.
set.seed(1)

# Creamos un objeto llamado `results` que replica para `B` iteraciones una serie simulada y determina si esa serie contiene al menos cuatro victorias para los Cavs.
results <- replicate(B, {
  cavs_wins <- sample(c(0, 1), 6, replace = TRUE)
  sum(cavs_wins) >= 4
})

# Calculamos la frecuencia de `B` iteraciones en que los Cavs ganaron al menos cuatro juegos en el resto de la serie.
# Imprimimos su respuesta en la consola.
mean(results)
