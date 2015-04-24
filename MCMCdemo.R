#  MCMC Gibbs sampling for mult normal

N <- 10000
r <- .9
sig <- sqrt(1 - r^2)

x <- matrix(NA, 2, N)
x1 <- x2 <- 0
for (i in 1:N){
	x1 <- rnorm(1, r*x2, sig)
	x2 <- rnorm(1, r*x1, sig)
	x[, i] <- c(x1, x2)
	}

plot(x[1, ], x[2, ])

hist(x[1, ])

hist(x[2, ])
plot(x[1, 1:100])

var(t(x))


# Direct sampling

V <- matrix(c(1, r, r, 1), 2, 2)
A <- chol(V)

y <- matrix(NA, 2, N)
for (i in 1:N){
	y[, i] <- t(A)%*%rnorm(2)
	}

plot(y[1, ], y[2, ])

plot(y[1, 1:100])

var(t(y))
