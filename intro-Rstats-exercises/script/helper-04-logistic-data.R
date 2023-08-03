# ..................................
# ..................................
# Logistic regression
# here we simulate data
# we generate data and use age, gender, and income 
# as covariates
x = seq(-4, 4, length.out = 100)
p = 1/(1 + exp(-x))
plot(x, p, type = "l")

n = 100

set.seed(1149)
location = sample(c("Site 1", "Site 2"), size = n, replace = TRUE)
set.seed(1148)
gdd0 = round(runif(n/2, 0, 5))
set.seed(1149)
gdd1 = round(runif(n/2, 10, 20))
gdd = c(gdd0, gdd1)
boxplot(gdd ~ location)

flower0 = rep(0, n/2)
flower1 = rep(1, n/2)

flower = c(flower0, flower1)

table(flower)

# boxplots
boxplot(gdd ~ flower)

dat = data.frame(flower,
                 location,
                 gdd)

mod = glm(flower ~ ., data = dat, family = "binomial")

summary(mod)

# we compute null log-likelihood and log-lik from the model
llnull = mod$null.deviance/-2
llproposed = mod$deviance/-2

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(llnull - llproposed) / llnull

#write.csv(dat, "data/variety-adoption.csv", row.names = FALSE)


