setwd('/Users/rachnish/Documents/Stevens/FE 680/Valuing_Swaps_HWModel_IntRateTrees')

## Vasicek Model Implementation for valuing European Call & Put Option on a ZCB with 5 yrs maturity. Strike Price - $87, Principal - $100 ##

Vasicek_Model_Implementation_A <- function(a = 0.1,
                                         b = 0.08,
                                         sigma = 0.015,
                                         rt = 0.05,
                                         L = 100,
                                         s = 3,
                                         Ti = 1,
                                         k = 87,
                                         t = 0){

  #Calc of A
  A <- exp((((B - Ti + t)*((a^2)*b - (sigma^2)/2))/(a^2)) - ((sigma^2)*(B^2)) / (4*a))
  print(A)
}

Vasicek_Model_Implementation_B <- function(a = 0.1,
                                           b = 0.08,
                                           sigma = 0.015,
                                           rt = 5,
                                           L = 100,
                                           s = 3,
                                           Ti = 1,
                                           k = 87,
                                           t = 0){
  
  
  #Calc of B
  B <- (1-exp(-a*(Ti-t))) / a
  print(B)
}

Vasicek_Model_Implementation_sigma_p <- function(a = 0.1,
                                           b = 0.08,
                                           sigma = 0.015,
                                           rt = 5,
                                           L = 100,
                                           s = 3,
                                           Ti = 1,
                                           k = 87,
                                           t = 0){
  
  #Calc of sigma_p
  sigma_p <- (sigma/a)*(1 - exp(-a*(s-Ti)))*sqrt((1-exp(-2*a*Ti))/(2*a))
  print(sigma_p)

}
  
#Cal of P(0,1)
B<- Vasicek_Model_Implementation_B()
A<- Vasicek_Model_Implementation_A()
sigma_p <- Vasicek_Model_Implementation_sigma_p()
rt <- 0.05
P1 <- A*exp(-1*B*rt)

#Cal of P(0,3)
B<- Vasicek_Model_Implementation_B(a = 0.1,b = 0.08,sigma = 0.015,rt = 5,L = 100,s = 3,Ti = 3,k = 87,t = 0)
A<- Vasicek_Model_Implementation_A(a = 0.1,b = 0.08,sigma = 0.015,rt = 5,L = 100,s = 3,Ti = 3,k = 87,t = 0)
sigma_p <- Vasicek_Model_Implementation_sigma_p(a = 0.1,b = 0.08,sigma = 0.015,rt = 5,L = 100,s = 3,Ti = 3,k = 87,t = 0)
rt <- 0.05
P3 <- A*exp(-1*B*rt)

#Further Implementation
Vasicek_Model_Implementation_h <- function(sigma_p = 0.02588585,
                                           L = 100,
                                           k = 87,
                                           t = 0,
                                           P1 = 0.949883, 
                                           P3 = 0.850924){
  #Cal of h
  h <- ((1/sigma_p)*log((L*P3)/(P1*k))) + (sigma_p/2)
  print(h)
}

h <- Vasicek_Model_Implementation_h()

#Price of a European Call Option
sigma_p <- 0.02588585
N_h <- pnorm(h)
N_h_sigma_p <- pnorm(h-sigma_p)

Vasicek_Model_Implementation_C <- function(L = 100,
                                           s = 3,
                                           Ti = 1,
                                           k = 87,
                                           t = 0,
                                           P1 = 0.949883, 
                                           P3 = 0.850924 ){
print(L*P3*N_h - k*P1*N_h_sigma_p)
}

Vasicek_Model_Implementation_P <- function(L = 100,
                                           s = 3,
                                           Ti = 1,
                                           k = 87,
                                           t = 0,
                                           P1 = 0.949883, 
                                           P3 = 0.850924 ){
  print(k*P1*(1-N_h_sigma_p) - L*P3*(1-N_h))
}

C <- Vasicek_Model_Implementation_C()
P <- Vasicek_Model_Implementation_P()
