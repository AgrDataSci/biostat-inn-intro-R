# Example of linear model with 
# one-way and two-way ANOVA
# hint!
# to read an external data
# replace "filename.csv" by the name of your file
# you may need to adjust the script to match with 
# your column names
library("ggplot2")

x = c(21, 20, 20.5, 22, 21.7, 21.8,
       24, 23.6, 24.3, 25, 25.3, 26,   
       26, 26.5, 26.9, 29, 28.6, 29.1)

y = rep(c("Control", "NPK", "DAP"),
         each = 6)

w = rep(paste0("R",1:6),
         times = 3)

dat = data.frame(treat = y,
                  rep = w, 
                  yield = x)

write.csv(dat, "data/yield.csv", row.names = FALSE)

boxplot(dat$yield)

boxplot(yield ~ treat,
        col = c("red", "blue", "yellow"),
        ylab = "Yield (ton/ha)",
        xlab = "Treatment",
        data = dat)

dir.create("output", showWarnings = FALSE)

png("output/boxplot.png",
    width = 10,
    height = 10,
    units = "cm",
    res = 300)
boxplot(yield ~ treat,
        col = c("red", "blue", "yellow"),
        ylab = "Yield (ton/ha)",
        xlab = "Treatment",
        data = dat)
dev.off()

plot(density(dat$yield))

mod = lm(yield ~ treat, data = dat)

summary(mod)

anova(mod)

summary(mod)

l = rep(c("Arusha", "Moshi"), each = 3)
l = rep(l, times = 3)

dat$location = l

mod2 = lm(yield ~ treat + location, 
           data = dat)

summary(mod2)

anova(mod2)

boxplot(yield ~ treat:location,
        col = c("red", "blue", "yellow"),
        ylab = "Yield (ton/ha)",
        xlab = "Treatment",
        data = dat)

ggplot(dat, aes(x = treat, y = yield)) + 
  geom_boxplot() +
  geom_jitter() +
  theme_bw() +
  labs(x = "Treatment",
       y = "Yield (ton/ha)")
