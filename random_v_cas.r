# definimos la urna con los posibles resultados
color <- rep(c("Negro", "Rojo", "Verde"), c(18, 18, 2))
#suponemos 1000 jugadores
n <- 1000
#el casino perderá $1 si el resultado es rojo
#en otro caso el casino gana $1
#los resultados son independientes
X <- sample(ifelse(color == "Rojo", -1, 1), n, replace = TRUE)
X[1:10]
#conociendo las proporciones de 1s y -1s podemos generar el modelo de muestreo
X <- sample(c(-1, 1), n, replace = TRUE, prob = c(9 / 19, 10 / 19))
# las ganancias son la suma de los eventos
S <- sum(X)
#Estimamos la función de distribución utilizando simulaciones de Monte Carlo
B <- 10000 # Numero de repeticiones
#Simulamos 1000 personas jugando 10000 veces
S <- replicate(B, {
    X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))
    sum(X)    # Ganancias totales
})
mean(S <= 0) #Probabilidad que el casino pierda dinero

# Graficamos para poder visualizar de mejor manera el comportamiento
library(tidyverse) #importamos la librería
#creamos la secuencia dentro de donde se encuentran los valores de S
s <- seq(min(S), max(S), length = 100)
#Generamos la densidad normal de S
densidad_normal <- data.frame(s = s, f = dnorm(s, mean(S), sd(S)))
data.frame (S = S) %>%    # Creamos el marco de datos de S para el histograma
    ggplot(aes(S, ..density..)) +
    geom_histogram(color = "black", binwidth = 10) +
    ylab("Probabilidad") +
    geom_line(data = densidad_normal, mapping = aes(s, f), color = "blue")

#utilizando funciones estadísticas
n <- 1000
pbinom(n/2, size = n, prob = 10/19)

#Debido a que esta es una función de probabilidad discreta
# para obtener Pr(𝑆 < 0) en vez de Pr(𝑆 ≤ 0), escribimos:
pbinom(n/2-1, size = n, prob = 10/19)
