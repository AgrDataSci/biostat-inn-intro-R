
size = c(1.8, 2.4, 3.5, 3.9, 4.4, 5.1, 5.6, 6.3, 2.1)
weight = c(2.6, 1.0, 3.7, 5.5, 3.2, 3.0, 4.9, 6.3, 4.6)

dat = data.frame(weight, size)

head(dat)

plot(density(dat$weight, na.rm = TRUE))

plot(y = dat$weight, x = dat$size)

mod = glm(weight ~ size, data = dat)

summary(mod)

coefs = coefficients(mod)
coefs = round(coefs, 3)

eq = paste0("Weight = ",
            coefs[1], 
            " + (",
            coefs[2], 
            " * Size)")
eq

r2 = with(summary(mod), 1 - deviance/null.deviance)
r2

r2 = bquote(italic(R)^2 == .(format(r2, digits = 3)))

r2

plot(y = dat$weight,
     x = dat$size, 
     xlab = "Size = (cm)",
     ylab = "Weight (g)",
     main = eq)
abline(mod, col = "red")     
text(x = 2.5, y = 6, labels = r2)     
     
     

