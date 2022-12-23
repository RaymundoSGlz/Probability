p <- 5/38
a <- 6
b <- -1
n <- 500

#valor esperado
mu <- (a * p) + (b * (1 - p))

#error estandar
sigma <- abs(a - b) * sqrt(p * (1 - p))

#error estandar promedio
sigma/sqrt(n)

#valor esperado total
mu * n

#error estandar de la suma
sigma * sqrt(n)

#probabilidad de perder dinero despues de 500 intentos
pnorm(0, mu * n, sigma * sqrt(n))
