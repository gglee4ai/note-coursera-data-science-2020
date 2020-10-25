x <- list(a = 1:5, b = rnorm(10))
x <- list(1:5, rnorm(10))
x <- (1:4)
lapply(x, runif)

lapply(x, runif, min = 0, max = 10)

x <- list(a = matrix(1:4, 2, 2), b = matrix(1:6, 3, 2))
x

lapply(x, function(m) m[,1])

x <- list(a = 1:4, b = rnorm(10), c = rnorm(20, 1),
          d = rnorm(100, 5))
lapply(x, mean)
sapply(x, mean)

mean(x)

x <- matrix(rnorm(200), 20, 10)
apply(x, 2, mean)

apply(x, 1, sum)

a <- apply(x, 1, quantile, probs = c(0.25, 0.75))
dim(a)



a <- array(rnorm(2 * 2 * 10), c(2, 2, 10))
a
apply(a, 3, mean)
mean(a[,,1 ])
apply(a, c(1, 2), mean)
mean(a[1,1,])
rowMeans(a, dims = 2)
?rowMeans


l < - list(rep(1, 4), rep(2, 3), rep(3, 2), rep(4, 1))

mapply(rep, 1:4, 4:1)


noise <- function(n, mean, sd) {
  rnorm(n, mean, sd)
}

noise(5, 1, 2)

mapply(noise, 1:5, 1:5, 2)


x <- c(rnorm(10), runif(10), rnorm(10, 1))
x
f <- gl(3, 10)
f

tapply(x, f, mean)
tapply(x, f, mean, simplify = FALSE)
tapply(x, f, range)


x <- c(rnorm(10), runif(10), rnorm(10, 1))
x
f <- gl(3, 10)
split(x, f)


lapply(split(x, f), mean)

head(airquality)

s <- split(airquality, airquality$Month)
str(s)
lapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")], na.rm = TRUE))
b <- sapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")], na.rm = TRUE))
b

library(tidyverse)


airquality %>% 
  group_by(Month) %>% 
  summarize_at(c("Ozone", "Solar.R", "Wind"), mean, na.rm = TRUE)


x <- rnorm(10)
f1 <- gl(2, 5)
f2 <- gl(5, 2)
f1
f2
i <- interaction(f1, f2)


printmessage <- function(x) {
  if(x > 0) print("x is greater than zero")
  else print("x is less than or equal to zero")
  invisible(x)
}
