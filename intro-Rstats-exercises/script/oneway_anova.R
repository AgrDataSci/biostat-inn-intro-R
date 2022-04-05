library("tidyverse")

list.files("data")

# read the file 
dat <- read.csv("data/drug_variants.csv")

dat

str(dat)

# boxplot using base R
boxplot(Value ~ Group, 
        data = dat,
        xlab = "Group",
        ylab = "Value")


# density plot using ggplot2
ggplot(data = dat, aes(x = Value, group = Group, fill = Group)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_bw()

# data summary using base R
datS <- split(dat, dat$Group)

datS <- lapply(datS, function(x){
  data.frame(group = x$Group[1],
             count = length(x$Value),
             sum  = sum(x$Value),
             mean = mean(x$Value),
             variance = var(x$Value),
             sd   = sd(x$Value))
})


datS <- do.call(rbind, datS)

datS

# same but using tidyverse
dat %>% 
  group_by(Group) %>% 
  summarise(mean = mean(Value),
            count = length(Value),
            sum = sum(Value),
            variance = var(Value),
            sd = sd(Value))

# lets run an anova
mod <- with(dat, 
            lm(Value ~ Group))

anova(mod)

summary(mod)

