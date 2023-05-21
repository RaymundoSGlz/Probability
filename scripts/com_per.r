library(gtools)
#creamos la baraja
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven",
            "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number=numbers, suit=suits)
deck <- paste(deck$number, deck$suit)
#verificamos la probabilidad de obtener rey
kings <- paste("King", suits)
mean(deck %in% kings)
#calculamos las formas de ejegir dos cartas cuando el orden importa
hands <- permutations(52, 2, v = deck)
#obtenemos la primera y segunda carta
first_card <- hands[,1]
second_card <- hands[,2]
#obtenemos los casos donde k es la primer carta
kings <- paste("King", suits)
sum(first_card %in% kings)
#obtenemos cuando ademas la segunda carta es K 
sum(first_card%in%kings & second_card%in%kings)/ sum(first_card%in%kings)

#obtener un natural 21
aces <- paste("Ace", suits)

facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52, 2, v = deck)
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

#utilizando Monte Carlo para el Natural 21
blackjack <- function(){
    hand <- sample(deck, 2)
    (hand[1] %in% aces & hand[2] %in% facecard) |
        (hand[2] %in% aces & hand[1] %in% facecard)
}
#repetimos 
B <- 10000
results <- replicate(B, blackjack())
mean(results)
