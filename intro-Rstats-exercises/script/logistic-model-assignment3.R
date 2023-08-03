library("ggplot2")

dat = read.csv("data/flowering.csv", sep = ";")

head(dat)

ggplot(dat, aes(x = as.factor(flower), y = gdd)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = "Flowering event",
       y = "Count")

mod = glm(flower ~ freezing_days + gdd,
          data = dat, 
          family = "binomial")

summary(mod)
 
log-odds

# For every 1-day of freezing the prob of 
# flowering (log-odds) is -0.13 
# For every 1-day of growing degree days 
# the log-odds is 6.74



