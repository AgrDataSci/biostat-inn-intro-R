library("sf")

# e = matrix(c(7, 59, 17, 59, 17, 63,
#              7, 63, 7, 59),
#            nrow = 5, ncol = 2, byrow = TRUE)

e = matrix(c(34, 0, 
             34.7, 0, 
             34.7, 0.7,
             34, 0.7, 
             34, 0),
            nrow = 5, ncol = 2, byrow = TRUE)


e = st_polygon(list(e))

plot(e)

set.seed(1032)
p1 = st_sample(e, 63, type = "random")
p1 = st_as_sf(p1, crs = 4326)

x = matrix(unlist(p1), nrow = 63, ncol = 2, byrow = TRUE)

x = apply(x[,c(2,1)], 1, function(x) paste(paste(x, collapse = " "), "0 0"))

write.csv(x, "points.csv")

set.seed(1032)
p2 = st_sample(e, 20, type = "hexagonal")
p2 = st_as_sf(p2, crs = 4326)

plot(e)
plot(p1, add = TRUE, col = "red")
plot(p2, add = TRUE, col = "blue", pch = "+")

# this is how you write the file as csv
p2 <- matrix(unlist(p2), 
             byrow = TRUE, 
             nrow = nrow(p2), 
             ncol = 2)

write.csv(p2, 
          file = "data/soil-sample-location.csv",
          row.names = FALSE)




