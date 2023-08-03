# ..................................
# ..................................
# Logistic regression
# here we simulate data
# we generate data and use age, gender, and income 
# as covariates
x = seq(-4, 4, length.out = 100)
p = 1/(1 + exp(-x))
plot(x, p, type = "l")

n = 40

set.seed(1149)
gender = sample(c(0,1), size = n, replace = TRUE)
set.seed(1148)
age = round(runif(n, 18, 80))
set.seed(1150)
income = round(runif(n, 500, 1800))
hist(age)
boxplot(age)
boxplot(age ~ gender)

# get the probabilities based on the variables generated
xb = -3 + (3.5 * gender) + (-0.2 * age) + (0.01 * income)

p = 1/(1 + exp(-xb))

adopt = rbinom(n, size = 1, prob = p)

gender = ifelse(gender == 0, "Man", "Woman")

# boxplots
boxplot(age ~ gender)

boxplot(income ~ gender)

boxplot(income ~ adopt)

boxplot(age ~ adopt)

dat = data.frame(adopt,
                 gender,
                 age, 
                 income)

mod = glm(adopt ~ ., data = dat, family = "binomial")

summary(mod)

# we compute null log-likelihood and log-lik from the model
llnull = mod$null.deviance/-2
llproposed = mod$deviance/-2

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(llnull - llproposed) / llnull

#write.csv(dat, "data/variety-adoption.csv", row.names = FALSE)


