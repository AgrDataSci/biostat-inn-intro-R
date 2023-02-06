library("ggplot2")

list.files("data")

dat <- read.csv("data/rice_yield.csv")

boxplot(Yield ~ Rice:Temperature,
        data = dat,
        xlab = "Rice x Temperature",
        ylab = "Yield")


boxplot(Yield ~ Rice,
        data = dat,
        xlab = "Rice",
        ylab = "Yield")

dat

ggplot(data = dat, 
       aes(x = Yield, 
           group = Rice, 
           fill = Rice,
           color = Rice)) +
  geom_density(adjust = 1.5, alpha = 0.4) +
  theme_classic()

mod <- lm(Yield ~ Rice + Temperature, data = dat)

summary(mod)

anova(mod)


