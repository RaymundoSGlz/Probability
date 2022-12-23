n_preguntas <- 44
recompensa <- 1
penalizacion <- -0.25

#probabilidad de acertar en una pregunta
p <- 1/5

#valor esperado de puntos

e_v_puntos <- (recompensa * p) + (penalizacion * (1 - p))

#valor esperado total
m <- n_preguntas * e_v_puntos

#error estandar
se <- sqrt(n_preguntas) * abs(penalizacion - recompensa) * sqrt(p*(1-p))

# CLT mayor que 8
r <- 1 - pnorm(8, m, se)

#monte carlo
set.seed(21)
B <- 10000
S <- replicate(B, {
  Resultados <- sample(c(recompensa, penalizacion), 
  n_preguntas, replace = TRUE, prob = c(p, 1-p))
  sum (Resultados)
})
mean (S > 8)

#con los cambios realizdos

n_preguntas2 <- 44
recompensa2 <- 1
penalizacion2 <- 0

#probabilidad de acertar en una pregunta
p2 <- 1/4

#valor esperado de puntos

e_v_puntos2 <- (recompensa2 * p2) + (penalizacion2 * (1 - p2))

#valor esperado total
m2 <- n_preguntas2 * e_v_puntos2

#error estandar
se2 <- sqrt(n_preguntas2) * abs(penalizacion2 - recompensa2) * sqrt(p2*(1-p2))

# CLT mayor que 8
r2 <- 1 - pnorm(8, m2, se2)

#utilizando los rangos de habilidades
p <- seq(0.25, 0.95, 0.05)

score <- sapply(p, function(v){
  e_points <- (recompensa2*v) + (penalizacion2 * (1-v))
  m <- n_preguntas2 * e_points
  se <- sqrt(n_preguntas2) * abs(penalizacion2-recompensa2) * sqrt(v*(1-v))
  1-pnorm (35, m, se)
})

min(p[which(score > 0.8)])
