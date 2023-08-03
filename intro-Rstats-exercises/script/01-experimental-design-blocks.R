library("ClimMobTools")
library("agricolae")

# define a treatment
treat = c("Control", paste('Supp -', 1:4))

# RCBD
set.seed(1633)
design = design.rcbd(treat, 
                     r = 5, 
                     kinds = "Wichmann-Hill",
                     continue = TRUE)

design$sketch

design = design$sketch

# incomplete block design
set.seed(1634)
design2 = randomise(8, treat, ncomp = 3)

design2

write.csv(design, "output/randomized-complete-blocks.csv", row.names = FALSE)

write.csv(design2, "output/randomized-incomplete-blocks.csv", row.names = FALSE)


