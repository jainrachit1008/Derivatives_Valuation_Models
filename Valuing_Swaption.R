##setwd('/Users/rachnish/Documents/Stevens/FE 680/Valuing Swaptions & Collar')##

## Valuing a European Swaption ##

#LIBOR Flat Curve
m1 <- 1
Rm1 <- 0.045                 #annual compounded
Rc1 <- m1*log(1 + Rm1/m1)    #continuosly compounded

#Parameters for Swaption
T1 <- 4              
L1 <- 10000000
Sigma_Forward1 <- 0.30     #Volatility of Swap rate
Sk <- 0.05
So <- 0.045
t1 <- c(5,6,7)

#Calculation of Annuity Factor
sum = 0
for(s in 1:(T1-1)){
  x <- exp(-Rc1*t1[s])
  sum = sum + x
}
A <- m1*sum

#Value of Swaption
D1 = (log(So/Sk) + (Sigma_Forward1^2)*(T1/2)) / (Sigma_Forward1*sqrt(T1))
D2 = D1 - Sigma_Forward1*sqrt(T1)
Value_Swaption = L1*A*(So*pnorm(D1) - Sk*pnorm(D2))

print(Value_Swaption)
