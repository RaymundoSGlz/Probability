p <- 5/38
a <- 6
b <- -1
n <- 500

#valor esperado
mu <- (a * p) + (b * (1 - p))

#error estándar
sigma <- abs(a - b) * sqrt(p * (1 - p))

#error estándar promedio
sigma/sqrt(n)

#valor esperado total
mu * n

#error estándar de la suma
sigma * sqrt(n)

#probabilidad de perder dinero después de 500 intentos
pnorm(0, mu * n, sigma * sqrt(n))
