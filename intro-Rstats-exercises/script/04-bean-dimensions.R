list.files("data", full.names = TRUE)

bean = read.csv("data/bean-data.csv")

str(bean)

plot(bean$length, 
     bean$hundred_seed_weight, 
     xlab = "Length (mm)",
     ylab = "Hundred seed weight (g)",
     main = "Common bean dimensions")


mod = lm(hundred_seed_weight ~ length,
         data = bean)

summary(mod)

coefs = coefficients(mod)

coefs

eq = paste0("y = ", 
            round(coefs[2], 1),
            "*x ",
            round(coefs[1], 1))

eq

plot(bean$length, 
     bean$hundred_seed_weight, 
     xlab = "Length (mm)",
     ylab = "Hundred seed weight (g)",
     main = eq)
abline(mod, col = "red")

dir.create("output", showWarnings = FALSE)

png(filename = "output/bean-model-hsw-by-length.png",
    width = 15,
    height = 15,
    res = 500,
    units = "cm")
plot(bean$length, 
     bean$hundred_seed_weight, 
     xlab = "Length (mm)",
     ylab = "Hundred seed weight (g)",
     main = eq)
abline(mod, col = "red")
dev.off()

# add other trait (width)
plot(bean$width, 
     bean$hundred_seed_weight)

# we see that we have an outlier
boxplot(bean$width)

# use function boxplot.stats to identify possible 
# outliers
boxplot.stats(bean$width)

# keep the outlier values as a separeted vector
out = boxplot.stats(bean$width)$out

# check where in the vector bean$with the \
# outliers are located, we have at least two approaches
# the first in by targeting each single value 
# this is safe when you have outliers in both edges of 
# the data distribution
bean$width %in% out

# the second is just targeting the values that fall below 
# the minimum outlier
bean$width < min(out)

rmv = !bean$width %in% out

bean = bean[rmv, ]

# plot again without outliers
plot(bean$width, 
     bean$hundred_seed_weight)

# a second model with length and width combined
mod2 = lm(hundred_seed_weight ~ length + width,
         data = bean)

summary(mod2)



