############# Solutions to question 7.6 ############# 

n <- 100
rs0 <- 0.3009
df0 <- 98
dim0 <- n-df0
rs1 <- 0.3031
df1 <- 95
dim1 <- n-df1

# F test for one model over the other - can't reject null hypothesis here
f <- (rs0^2 * rs1^2) / (dim1 - dim0) / (rs1 / (n - dim1))
pf(f, df0 - df1, n - dim1)
