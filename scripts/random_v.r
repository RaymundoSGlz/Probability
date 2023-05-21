#modelando una variable aleatoria
#definimos una variable aleatoria x
#sera 1 si la canica es azul y 0 en otro caso
canicas <- rep(c("roja", "azul"), times = c(2, 3))
x <- ifelse(sample(canicas, 1) == "azul", 1, 0)

#mostramos que la variable es diferente cada vez
ifelse(sample(canicas, 1) == "azul", 1, 0)
ifelse(sample(canicas, 1) == "azul", 1, 0)
ifelse(sample(canicas, 1) == "azul", 1, 0)
