# ..................................
# ..................................
# Logistic regression 
# Load packages
library("ggplot2")
library("gnm")
library("gosset")
library("pROC")
library("tidyverse")
library("magrittr")

# this is how a logit data should look like 
x = seq(-4, 4, length.out = 100)
p = 1/(1 + exp(-x))
plot(x, p, type = "l")

# we read the variety adoption data 
# which indicates whether a farmer adopted 
# or not a new wheat variety. We add age,
# gender and income as covariates to explain the adoption
# this data was simulated using the script 
# helper-01-simulate-adoption-data.R
dat = read.csv("data/variety-adoption.csv")

str(dat)



# ..................................
# ..................................
# Visualization ####
# check the proportion of men and women
# that adopted or not 
xtabs(~ adopt + gender, data = dat)

# boxplots
boxplot(age ~ gender, data = dat)

boxplot(income ~ gender, data = dat)

boxplot(income ~ adopt, data = dat)

boxplot(age ~ adopt, data = dat)

str(dat)

ggplot(dat, aes(x = income, 
                fill = gender, 
                color = gender)) +
  geom_density(alpha = 0.5) +
  theme_classic() +
  labs(x = "Income (EUR)",
       y = "Density")

dat %>% 
  group_by(adopt) %>% 
  summarise(freq = length(adopt) / nrow(dat)) %>% 
  ggplot(aes(x = adopt, y = freq)) +
  geom_bar(stat = "identity") +
  theme_minimal()


# ..................................
# ..................................
# Test difference in income by gender using t-test ####

t.test(income ~ gender, data = dat)

t.test(age ~ gender, data = dat)


# ..................................
# ..................................
# Fit ####
# Now the model with all the covariates
mod = glm(adopt ~ ., data = dat, 
          family = "binomial")

summary(mod)

logLik(mod) * -2

# we compute null log-likelihood and log-lik from the model
llnull = mod$null.deviance/-2
llproposed = mod$deviance/-2

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(llnull - llproposed) / llnull

# same using the function gosset::pseudoR2()
pseudoR2(mod)

?pseudoR2

mod$fitted.values

## now we can plot the data
fit = data.frame(prob = mod$fitted.values,
                 observed = factor(dat$adopt))

fit = fit[order(fit$prob, decreasing = FALSE), ]
fit$rank = 1:nrow(fit)

# Lastly, we can plot the predicted probabilities for each sample in 
# adopting and color by whether or not they actually had adopted
ggplot(data=fit, aes(x = rank, y = prob, color = observed)) +
  geom_point(alpha = 1,
             shape = 4, 
             stroke = 1) +
  labs(x = "Index", y = "Predicted probability") +
  scale_color_manual(values = c("red", "blue")) +
  theme_classic()

# ..................................
# ..................................
# Model testing ####
# now a bit of model testing, intro to ML
# we split the model into two datasets 
# one for training and other for testing
n = nrow(dat)
prop = 0.7
set.seed(200)
s = sample(1:n, as.integer(n*prop), replace = FALSE)
s

# get test and train data
train = dat[s, ]
test = dat[-s, ]

mod = glm(adopt ~ ., data = train, family = "binomial")

summary(mod)

# and the predictions
preds = predict(mod, newdata = test, type = "response")

plot(sort(preds))

predition = as.integer(preds > 0.5)

table(predition)

confusion_m = addmargins(table(test$adopt, predition))

colnames(confusion_m) = c("Fail", "Success", "Total")
rownames(confusion_m) = c("Fail", "Success", "Total")

confusion_m

# false positive rate (Type 1 error)
confusion_m[1,2] / (confusion_m[1, 3])

# true positive rate 
confusion_m[2,2] / (confusion_m[2, 3])

# accuracy
(confusion_m[1, 1] + confusion_m[2, 2]) / confusion_m[3, 3]

# compute AUC
auc(test$adopt, preds)

# ..................................
# ..................................
# Cross validation ####
crossvalidation(adopt ~ gender + age + income, 
                data = dat,
                family = "binomial", 
                k = 10)
