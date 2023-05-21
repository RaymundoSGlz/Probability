# definimos x como las alturas de hombres del dataset
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex == "Male") %>% pull(height)

# generamos datos simulados utilizando la distribucion normal
# ambos tienen n objetos
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)

# graficamos para los datos simulados
data.frame(simulated_heights = simulated_heights) %>%
    ggplot(aes(simulated_heights)) +
    geom_histogram(color="black", binwidth = 2)
#simulación de monte Carlo
B <- 10000
tallest <- replicate(B, {
    simulated_data <- rnorm(800, avg, s)
    max(simulated_data)    # determina la altura maxima
})
mean(tallest >= 7*12) #proporción de seven footer

#otras distribuciones
x <- seq(-4, 4, length.out = 100)
qplot(x, f, geom = "line", data = data.frame(x, f = dnorm(x)))

x <- seq(-4, 4, length.out = 100)
data.frame(x, f = dnorm(x)) %>%
    ggplot(aes(x,f)) +
    geom_line()
