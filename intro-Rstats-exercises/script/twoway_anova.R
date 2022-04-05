

list.files("data")

dat <- read.csv("data/rice_yield.csv")

boxplot(Yield ~ Rice:Temperature,
        data = dat,
        xlab = "Rice x Temperature",
        ylab = "Yield")

dat

ggplot(data = dat, aes(x = Yield, group = Rice, fill = Rice, color = Rice)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_bw()

mod <- with(dat,
            lm(Yield ~ Rice + Temperature + Rice:Temperature))

anova(mod)

summary(mod)


