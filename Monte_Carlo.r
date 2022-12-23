#comenzamos creando la bolsa de canicas
canicas <- rep(c("roja", "azul"), times = c(2, 3))
#utilizamos la función sample para escoger una al azar
sample(canicas, 1)
#definimos el numero de repeticiones
B <- 10000
#replicamos 
eventos <- replicate(B, sample(canicas, 1))
#mostramos la distribución en una tabla
tab <- table(eventos)

prop.table(tab)

#utilizando replace
events <- sample(canicas, B, replace = TRUE)
prop.table(table(events))
