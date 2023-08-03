# ..................................
# ..................................
# Analysis of data from incomplete block design
# using ranking approach 

# ..................................
# ..................................
# Read packages ####
library("gosset")
library("PlackettLuce")
dir.create("output", showWarnings = FALSE)

# Read data ####
data("beans", package = "PlackettLuce")

genotype_features = read.csv("data/nicabean-blups.csv")

# ..................................
# ..................................
# Prepare data ####
# the data needs to be transformed into rankings 
# we use the function rank_tricot() for this task
R = rank_tricot(data = beans,
                items = c("variety_a", "variety_b", "variety_c"),
                input = c("best", "worst"))

R

# ....................................
# Fit PlackettLuce model ####
# select the reference genotype
reference = "ALS 0532-6"

# fit the model 
mod = PlackettLuce(R)

mod

# get coefficients
coef(mod, log = TRUE, ref = reference)

summary(mod, ref = reference)

# compute reliability, probability to outperform a check
rel = reliability(mod, ref = reference)

rel

# ....................................
# PLADMM analysis ####
# PLADMM model using the variety features
form = paste("~", paste0(names(genotype_features)[-c(1)], 
                         collapse = " + "))
form = as.formula(form)
form

mod2 = pladmm(R, 
              formula = form, 
              data = genotype_features)

summary(mod2)

capture.output(summary(mod2), 
               file = "output/pladmm-overall-blups.txt")




