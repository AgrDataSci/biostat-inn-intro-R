

list.files("data")

dt <- read.csv("data/rice_yield.csv")

boxplot(Yield ~ Rice:Temperature,
        data = dt,
        xlab = "Rice x Temperature",
        ylab = "Yield")

dt

mod <- with(dt,
            lm(Yield ~ Rice + Temperature + Rice:Temperature))

anova(mod)

summary(mod)


