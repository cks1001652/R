# Example with ANOVA setup and factors:

dd <- data.frame(a = gl(3,4), b = gl(4,1,12)) # balanced 2-way
model.matrix(~ a + b, dd)

#  Regression example
model.matrix(log(Volume) ~ log(Height) + log(Girth), trees)

#  Efficient sampling from truncated normal

mu <- 1:3
s <- (2:4)/2

x <- mu + s*qnorm(1 - runif(3)*pnorm(mu/s))

X <- matrix(NA, 3, 1000)

for (i in 1:1000) X[, i] <- mu + s*qnorm(1 - runif(3)*pnorm(mu/s))

hist(X[1, ])
hist(X[2, ])
hist(X[3, ])
