library("PlackettLuce")
library("gosset")

players <- matrix(c(2, 0, 0, 1,
                    0, 1, 2, 0,
                    2, 0, 1, 0,
                    0, 1, 0, 1), 
                  nrow = 4, ncol = 4, byrow = TRUE)

countries <- c("Costa Rica", "Germany", "Japan", "Spain")

colnames(players) <- countries

players

players <- as.rankings(players)

plot(network(players))

weight <- c(7, 1, 1, 0.5)

mod <- PlackettLuce(players)

ref <- "Costa Rica"

coef(mod, log = F)

summary(mod, ref = ref)

r <- reliability(mod, ref = ref)

r

cbind(Countries = countries, 
      WinProb = round((r$reliability / 0.5 - 1), 2))[-1,]

