runners <- c("Jamaica", "Jamaica", "Jamaica", 
            "USA", "Ecuador", "Netherlands", 
            "France", "South Africa")
winners <- sample(runners, 3)
#definimos el numero de repeticiones
B <- 10000
#replicamos 
set.seed(1)
eventos <- replicate(B, {
    winners <- sample(runners, 3)
    jam <- all(winners == "Jamaica")
    })
#mostramos la distribucion en una tabla
tab <- table(eventos)

prop.table(tab)

mean(eventos)
