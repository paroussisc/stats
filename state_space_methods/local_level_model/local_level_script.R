library(ggplot2)
library(gridExtra)

y_t <- data.frame(read.delim("Nile.dat"))[[1]]
T <- nrow(data)

# initialise (as per section 2.2.2)
a_t <- rep(0, T + 1)
P_t <- c(1e7, rep(0, T))
sig_e <- sqrt(15099)
sig_n <- sqrt(1469.1)

# store the values
a_t <- rep(0, T)
v_t <- rep(0, T)
F_t <- rep(0, T)
K_t <- rep(0, T)

for (i in 1:T)
{
  # residual
  v_t[i] = y_t[i] - a_t[i]
  
  # state variance plus observation noise
  F_t[i] = P_t[i] + sig_e ^ 2
  
  # Kalman gain - lower obs variance means more learnt about state
  K_t[i] = P_t[i] / F_t[i]
  
  # t+1 state mean and variance
  a_t[i + 1] = a_t[i] + K_t[i] * v_t[i]
  P_t[i + 1] = P_t[i] * (1 - K_t[i]) + sig_n ^ 2
}

ggplot(data = data.frame(x = 1:T, y = a_t[2:length(a_t)], P = sqrt(P_t[2:length(P_t)])), aes(x = x, y = y)) + 
  geom_line() + 
  geom_line(data = data.frame(y = y_t, x=1:T), colour = "red", linetype = "dotted") + 
  ylim(450, 1500) + 
  geom_line(aes(y=qnorm(0.95, y, P)), linetype = "dashed") + 
  geom_line(aes(y=qnorm(0.025, y, P)), linetype = "dashed") 

ggplot(data = data.frame(x = 1:T, y = v_t[2:length(a_t)])) +
  geom_line(aes(x = x, y = y)) +
  geom_hline(yintercept = 0)

p <- ggplot(data = data.frame(x = 1:T, y = P_t[2:length(a_t)])) +
  geom_line(aes(x = x, y = y)) + xlab("P_t")

f <- ggplot(data = data.frame(x = 1:T, y = F_t[2:length(a_t)])) +
  geom_line(aes(x = x, y = y)) + xlab("F_t")

grid.arrange(p, f, ncol = 2)
