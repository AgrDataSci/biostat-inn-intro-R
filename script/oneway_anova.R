

list.files("data")

dt <- read.csv("data/drug_variants.csv")

dt

boxplot(dt$Value ~ dt$Group, 
        xlab = "Group",
        ylab = "Value")

dts <- split(dt, dt$Group)

dts <- lapply(dts, function(x){
  data.frame(group = x$Group[1],
             count = length(x$Value),
             sum  = sum(x$Value),
             mean = mean(x$Value),
             variance = var(x$Value),
             sd   = sd(x$Value))
})


dts <- do.call(rbind, dts)

library("tidyverse")

dt %>% 
  group_by(Group) %>% 
  summarise(mean = mean(Value),
            count = length(Value),
            sum = sum(Value),
            variance = var(Value),
            sd = sd(Value))


my_aov <- with(dt, 
               aov(Value ~ Group))


summary(my_aov)
