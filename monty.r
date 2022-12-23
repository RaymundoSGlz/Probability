# Estrategia de mantener
B <- 10000
mantener <- replicate(B, {
    puertas <- as.character(1:3)
    #colocamos los premios de manera aleatoria
    premio <- sample(c("carro", "cabra", "cabra"))
    premio_puerta <- puertas[premio == "carro"] #la puerta que tiene el premio
    mi_eleccion <- sample(puertas, 1) #puerta elegida
     #puerta abierta
    muestra <- sample(puertas[!puertas %in% c(mi_eleccion, premio_puerta)],1)
    mantener <- mi_eleccion #mantenemos la puerta elegida
    mantener == premio_puerta #verificamos si esta el premio
})
prob_mantener <- mean(mantener) #probabilidad si mantenemos la puerta


# Estrategia de cambiar
B <- 10000
cambiar <- replicate(B, {
    puertas <- as.character(1:3)
    #colocamos los premios de manera aleatoria
    premio <- sample(c("carro", "cabra", "cabra"))
    premio_puerta <- puertas[premio == "carro"] #la puerta que tiene el premio
    mi_eleccion <- sample(puertas, 1) #puerta elegida
     #puerta abierta
    muestra <- sample(puertas[!puertas %in% c(mi_eleccion, premio_puerta)],1)
    #cambiamos la puerta
    cambiar <- puertas[!puertas %in% c(mi_eleccion, muestra)]
    cambiar == premio_puerta #verificamos si esta el premio
})
prob_cambiar <- mean(cambiar) #probabilidad si mantenemos la puerta