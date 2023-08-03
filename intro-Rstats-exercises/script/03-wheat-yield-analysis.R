library("ggplot2")

x = c(21, 20, 20.5, 22, 21.7, 21.8,
      24, 23.6, 24.3, 25, 25.3, 26,   
      26, 26.5, 26.9, 29, 28.6, 29.1)

y = rep(c("Control", "NPK", "DAP"),
        each = 6)

w = rep(paste0("R", 1:6), 
        times = 3)

dat = data.frame(treat = y,
                 rep = w,
                 yield = x)

dim(dat)

# write the file in the folder data
write.csv(dat, 
          file = "data/wheat-yield.csv",
          row.names = FALSE)

# let's make some visualization

?density

plot(density(dat$yield,
             kernel = "gaussian"), 
     main = "Density of Yield (ton/ha)")

boxplot(dat$yield)
median(dat$yield)
mean(dat$yield)
sd(dat$yield)

boxplot(yield ~ treat, 
        data = dat,
        col = c("pink", "blue", "red"),
        xlab = "Treatment",
        ylab = "Yield (ton/ha)")

dir.create("output", showWarnings = FALSE)

# write the plot as png
png(filename = "output/yield-boxplot.png",
    width = 10,
    height = 10, 
    units = "cm",
    res = 300)
boxplot(yield ~ treat, 
        data = dat,
        col = c("pink", "blue", "red"),
        xlab = "Treatment",
        ylab = "Yield (ton/ha)")
dev.off()

# plot using ggplot2
yield_plot = 
  ggplot(dat, aes(x = treat, y = yield)) +
  geom_boxplot() +
  geom_jitter() +
  theme_classic() +
  labs(x = "Treament",
       y = "Yield (ton/ha)")

ggsave(filename = "output/yield-boxplot-ggplot.png",
       plot = yield_plot,
       height = 10,
       width = 10, 
       units = "cm",
       dpi = 300)

# fit a linear model 
?lm

mod = lm(yield ~ treat, data = dat)

summary(mod)

mean(dat$yield[dat$treat == "Control"])

anova(mod)





