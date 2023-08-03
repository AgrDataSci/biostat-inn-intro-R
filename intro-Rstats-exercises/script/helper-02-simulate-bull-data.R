

n = 43
progenitor = rep(c("Bob", "Diesel", "Kristof"), each = n)
set.seed(1515)
chest_girth = c(rnorm(n, mean = 210.72, sd = 10.5),
                rnorm(n, mean = 215.72, sd = 10.5),
                rnorm(n, mean = 217.72, sd = 10.5))
boxplot(chest_girth ~ progenitor)
set.seed(1516)
body_length = c(rnorm(n, mean = 166.14, sd = 10.5),
                rnorm(n, mean = 170.13, sd = 17.09),
                rnorm(n, mean = 180.5, sd = 10.5))
boxplot(body_length ~ progenitor)

set.seed(1517)
shoulder_height = c(rnorm(n, mean = 133.14, sd = 10.5),
                    rnorm(n, mean = 142.79, sd = 11.01),
                    rnorm(n, mean = 140.5, sd = 10.5))

boxplot(shoulder_height ~ progenitor)

weight = -888.64 +  0.1 * chest_girth + 3.5 * body_length + 2.7 * shoulder_height

set.seed(1759)
s = c(sample(1:n), (n+1):(n*3))
      
weight = weight[s]

dat = data.frame(progenitor,
                 body_length, 
                 shoulder_height,
                 chest_girth,
                 weight)

dat[2:5] = lapply(dat[2:5], function(x) round(x, 2))

head(dat)

#write.csv(dat, "data/bull-data.csv", row.names = FALSE)

mod = lm(weight ~ progenitor, data = dat)
summary(mod)
anova(mod)

plot(dat$weight, dat$body_length)

mod2 = lm(weight ~ body_length, data = dat)
summary(mod2)

mod3 = lm(weight ~ shoulder_height, data = dat)
summary(mod3)

mod4 = lm(weight ~ chest_girth, data = dat)
summary(mod4)

mod5 = lm(weight ~ + chest_girth + shoulder_height + body_length, data = dat)
summary(mod5)

# now we put the equation in the plot 
car::avPlots(mod5, 
        ylab = "Weight (kg)",
        main = "")

