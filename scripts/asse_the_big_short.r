#cargamos librerias
library(tidyverse)
library(dslabs)
#cargamos datos
data(death_prob)
head(death_prob)
#probabilidad de muerte mujer de 50 años
p <- death_prob %>% filter(age==50, sex == "Female") %>% pull(prob)
#valor esperado de utilidad neta de una mujer de 50 años
(-150000 * p) + (1150 * (1 - p))

#error estandar
abs(-150000 - 1150) * sqrt(p * (1 - p))

#CLT
pnorm(0, 1000 * (-150000 * p + 1150 * (1 - p)),
sqrt(1000) * (abs(-150000 - 1150) * sqrt(p * (1 - p))))

#probabilidad de muerte hombre de 50 años
pm <- death_prob %>% filter(age==50, sex == "Male") %>% pull(prob)
#cobro de prima
# donde  E[S] = mu_S = 700000
#         n = 1000
#         pm = probabilidad de muerte hombres de 50 años
#         a = 150000 perdida
#         b = valor de prima a calcular
# E[S] = n * (apm + b(1-pm))
# --> b = ((E[S]/n) - apm)/(1-pm)
b <- ((700000 / 1000) - -150000 * pm) / (1 - pm)

#error estandar
err <- sqrt(1000) * abs(b - -150000) *sqrt(pm*(1-pm))

#probabilidad de perder dinero
pnorm(0, 1000 * (-150000 * pm + b * (1 - pm)), err)

# ganancias esperadas para la compañia con mas de 1,000 policias
mu <- 1000 * (-150000*0.015 + 1150*(1-0.015))

#error estandar
err3 <- sqrt(1000) * abs(-150000 - 1150) * sqrt(0.015*(1 - 0.015))

#probabilidad de perder dinero
p3 <- pnorm(0, mu, err3)

#probabilidad de perder mas de un millon
pnorm(-1000000, mu, err3)

#probabilidad de muerte más baja
#para la que la posibilidad de perder dinero supera el 90%
p <- seq(0.01, 0.03, 0.001)
f <- function(p){
  mu <- 1000 * (-150000 * p + 1150 * (1 - p))
  err <- sqrt(1000) * abs(-150000 - 1150) * sqrt(p * (1 - p))
  pnorm(0, mu, err)
}
sapply(p, FUN=f)
plot(p, sapply(p, FUN=f))

#probabilidad de muerte más baja 
#para la cual la probabilidad de perder más de $1 millón supera el 90%
p <- seq(0.01, 0.03, 0.0025)
f1 <- function(p){
  mu <- 1000 * (-150000 * p + 1150 * (1 - p))
  err <- sqrt(1000) * abs(-150000 - 1150) * sqrt(p * (1 - p))
  pnorm(-1000000, mu, err)
}
sapply(p, FUN=f1)
plot(p, sapply(p, FUN=f1))

#perdidas o ganancias
set.seed(25)
n <- 1000
p_perder <- 0.015

X <- sample(c(0, 1), n, replace=TRUE, prob = c((1 - p_perder),p_perder))
ganancia <- -150000 * sum(X == 1)/10^6 # en millones
perdida <- 1150 * sum(X == 0) / 10^6
ganancia + perdida

#probabilidad de perder 1 millon o mas
set.seed(27)
S <- replicate(10000, {
  X <- sample(c(0, 1), 1000, replace=TRUE, prob = c((1-0.015), 0.015))
  perder <- -150000 * sum(X == 1)/10^6 # en millones
  ganar <- 1150*sum(X == 0) / 10^6
  perder + ganar
})
sum(S <= -1) / 10000

#Calcule la prima requerida para una probabilidad del 5% de perder dinero 
#dado n = 1000 préstamos probabilidad de muerte p = 0.015,
#pérdida por siniestro l=-150000.
p <- .015
n <- 1000
l <- -150000
z <- qnorm(.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))

#ganancia esperada por poliza
l*p + x*(1-p)

# Monte Carlo Probabilidad de perder dinero
set.seed(28)
S <- replicate(10000, {
  X <- sample(c(0,1), n, replace = TRUE, prob=c((1-p), p))
  perdida <- l * sum(X == 1) / 10^6 # en millones
  ganancia <- x * sum(X == 0) / 10^6
  ganancia + perdida
})
sum(S<0)/10000

# Monte Carlo pandemia
set.seed(29)
n <- 1000
B <- 10000
l <- -150000
p <- 0.015
x <- 3268.063
X <- replicate(B, {
  Y <- sample(c(x, l), n, replace=TRUE, prob=c(1-p, p))
  sum(Y)
})

# 1000 polizas
mean(X)

#probabilidad de perder dinero
sum(X<0)/B

mean(X<-1000000)