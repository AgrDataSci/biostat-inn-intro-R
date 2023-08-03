#install.packages(c("car", "ggplot2"))
library("car")
library("ggplot2")

# ......................................
# ......................................
# Part 1 - read the data ####
list.files("data", full.names = TRUE)

bean = read.csv("data/bean-data.csv")

str(bean)

# Plot the data as boxplots to find any possible outlier
boxplot(bean$hundred_seed_weight)

boxplot(bean$thickness)

boxplot(bean$width)

boxplot(bean$length)

# we see that we have outliers in width and thickness
boxplot(bean$width)
boxplot(bean$thickness)

# ......................................
# ......................................
# Part 2 - remove outliers ####
# here we use a loop to "simplify" the code 
# and prevent errors
# out = boxplot.stats(bean$width)$out
# rmv = !bean$width %in% out
# bean = bean[rmv, ]
# 
# out = boxplot.stats(bean$thickness)$out
# rmv = !bean$thickness %in% out
# bean = bean[rmv, ]
runover = c("width", "thickness")

for (i in seq_along(runover)) {
  # use function boxplot.stats to identify possible 
  # outliers
  out = boxplot.stats(bean[, runover[i]])$out
  
  rmv = !bean[, runover[i]] %in% out
  
  bean = bean[rmv, ]
  
}

# check the plots again
boxplot(bean$width)

boxplot(bean$thickness)

# ......................................
# ......................................
# Part 3 - Fit a simple linear model ####
# Now we can start fitting the model 
# plot all the data in bean
head(bean)

plot(bean$length, 
     bean$hundred_seed_weight)

# fit a linear model using only length
mod = lm(hundred_seed_weight ~ length,
         data = bean)

summary(mod)

plot(bean$length, 
     bean$hundred_seed_weight, 
     xlab = "Length (mm)",
     ylab = "Hundred seed weight (g)")
abline(mod, col = "red")

# ......................................
# ......................................
# Part 4 - fit a multiple linear model ####
# a second model with length, width and thickness combined
plot(bean$width, 
     bean$hundred_seed_weight)

plot(bean$thickness, 
     bean$hundred_seed_weight)

mod1 = lm(hundred_seed_weight ~ length,
   data = bean)
mod2 = lm(hundred_seed_weight ~ width,
   data = bean)
mod3 = lm(hundred_seed_weight ~ thickness,
   data = bean)
summary(mod1)
summary(mod2)
summary(mod3)

mod2 = lm(hundred_seed_weight ~ length + width + thickness,
         data = bean)

summary(mod2)

avPlots(mod2)

# ......................................
# ......................................
# Part 5 - Use model equation to predict new hundred seed weight ####
# extract equation
coefs = coefficients(mod2)

coefs

eq = paste0("HSW = ", 
            round(coefs[1], 1),
            " + ",
            round(coefs[2], 1),
            "(Length) + ",
            round(coefs[3], 1),
            ("(Width) + "),
            round(coefs[4], 1), 
            "(Thickness)")


eq
# let's use this equation to predict the hundred seed weight 
# of a new lot of seeds 
var1 = 1.37 
var2 = 0.716
var3 = 0.467

eq

-59.4 + (33.6*var1) + (34.7*var2) + (55.6*var3)

# now we put the equation in the plot 
avPlots(mod2, 
        ylab = "Hundred seed weight (g)",
        main = eq)


# ......................................
# ......................................
# Part 5 - Check model goodness-of-fit ####
# goodness-of-fit of our models
# using Akaike Information Criteria
AIC(mod)

AIC(mod2)

# compare actual values against predicted values 
# first steps in ML
actualval = bean[1:20, ]

predict(mod2, newdata = actualval)

# data frame with actual vs predicted
pdat = data.frame(predicted = predict(mod2, newdata = actualval), 
                  actual = actualval[, 2])

# plot using ggplot2

ggplot(pdat, aes(x = predicted, y = actual)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  labs(x = "Predicted values",
       y = "Actual values",
       title = "Predicted vs Actual values") +
  theme_classic()


ggsave("output/plot-predicted-vs-actual-bean.png",
       plot = last_plot(),
       width = 10,
       height = 10,
       dpi = 200, 
       units = "cm")
















