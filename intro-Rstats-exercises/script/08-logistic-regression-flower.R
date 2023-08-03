

dat$age

head(dat)

dat = read.csv("data/variety-adoption.csv")

freezing_days = dat$age + 40
gdd = log(dat$income)
flower = dat$adopt

dat = data.frame(flower, 
                 freezing_days,
                 gdd)

mod = glm(flower ~ ., data = dat, 
          family = "binomial")

summary(mod)

write.csv(dat, "data/flowering.csv")
