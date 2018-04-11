############# Solutions to question 7.7 ############# 

y <- cars$dist

# a)
X <- model.matrix( ~ speed + I(speed^2), data = cars)

# b)
qrx <- qr(X) ## returns a QR decomposition object
Q <- qr.Q(qrx,complete=TRUE) ## extract Q
R <- qr.R(qrx) ## extract R

# check for orthogonality
assertthat::are_equal(solve(Q), t(Q))
assertthat::are_equal((Q) %*% t(Q), diag(dim(Q)[1]))

# c)
n <- dim(Q)[1]
m <- 200
x <- matrix(10*rnorm(n * m),n,m)
sum((Q%*%x)^2) - sum((x)^2)

# d)
n <- dim(X)[1]
p <- dim(X)[2]

qt_y <- t(Q) %*% y
f <- qt_y[1:p,]
r <- qt_y[(p+1):n,]

# e)
beta <- solve(R) %*% f

# f)
# should be zero
assertthat::are_equal(sum(r^2), sum((y - X %*%beta)^2))

# g)
sig_s <- (sum(r^2))/(n-p)

# h)
V_b <- solve(R) %*% t(solve(R)) * sig_s

# Now compare to lm
mod <- lm(dist ~ speed + I(speed^2), data = cars)
assertthat::are_equal(vcov(mod), V_b)
