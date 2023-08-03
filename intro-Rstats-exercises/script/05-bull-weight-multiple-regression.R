library("car")
library("ggplot2")
library("gosset")
library("gnm")

# ......................................
# ......................................
# Part 1 - read the data ####
list.files("data", full.names = TRUE)

dat = read.csv("data/bull-data.csv")

str(dat)

# Plot the data as boxplots to find any possible outlier
boxplot(dat$body_length)

boxplot(weight ~ progenitor, data = dat)

ggplot(data = dat, aes(x = progenitor, y = weight)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = "Progenitor", "Weight (kg)")

# ......................................
# ......................................
# Part 2 - ANOVA ####
mod = lm(weight ~ progenitor, data = dat)

summary(mod)

anova(mod)

aggregate(weight ~ progenitor, mean, data = dat)

# ......................................
# ......................................
# Part 3 - Fit a simple linear model ####
# Now we can start fitting the model 
# plot all the data in dat
head(dat)

plot(dat$body_length, 
     dat$weigh)

mod2 = lm(weight ~ body_length, data = dat)

summary(mod2)

coefs = coefficients(mod2)
coefs
eq = paste0("Weight = ", 
            round(coefs[1], 1), 
            " + (",
            round(coefs[2], 1),
            " * BodyLength)")

eq
# plot the observed data and the fitted line
plot(dat$body_length, 
     dat$weigh, 
     xlab = "Body Length (cm)",
     ylab = "Weight (kg)",
     main = eq)
abline(mod2, col = "red")

# ......................................
# ......................................
# Part 4 - fit a multiple linear model ####
mod3 = lm(weight ~ chest_girth + shoulder_height + body_length,
          data = dat)

summary(mod3)

png("output/model-fit.png",
    width = 20,
    height = 20,
    units = "cm",
    res = 400)
avPlots(mod3)
dev.off()

newdata = dat[1:4, ]
newdata

predict(mod3, newdata = newdata)

# ......................................
# ......................................
# Part 5 - Use model equation to predict new data ####
# extract equation
coefs = coefficients(mod3)

coefs

eq = paste0("Weight = ", 
            round(coefs[1], 1),
            " + (",
            round(coefs[2], 1),
            " * ChestGirth) + (",
            round(coefs[3], 1),
            (" * ShoulderHeight) + ("),
            round(coefs[4], 1), 
            " * BodyLength)")

eq
summary(lm(weight ~ shoulder_height, data = dat))
summary(lm(weight ~ chest_girth, data = dat))
summary(lm(weight ~ body_length, data = dat))

# let's use this equation to predict new data
# of a new lot of seeds 
var1 = 0 
var2 = 0
var3 = 0

eq

head(dat)

-695 + (2.5 * 200) + (0.9 * 118) + (1.8 * 161)

# now we put the equation in the plot 
avPlots(mod3, 
        ylab = "Weight (kg)",
        main = eq)


# ......................................
# ......................................
# Part 5 - Check model goodness-of-fit ####
# goodness-of-fit of our models
# using Akaike Information Criteria
AIC(mod)

AIC(mod2)

AIC(mod3)

