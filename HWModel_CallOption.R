setwd('/Users/rachnish/Documents/Stevens/FE 680/Valuing_Swaps_HWModel_IntRateTrees')


## Hull White Model Implementation for valuing European Call Option on a ZCB with 5 yrs maturity. Strike Price - $70, Principal - $100 ##

HullWhite_Model_Implementation_B <- function(a = 0.08,
                                             Ti = 1,
                                             t = 0){
  #Calc of B
  B <- (1-exp(-a*(Ti-t))) / a
  print(B)
}


HullWhite_Model_Implementation_A <- function(a = 0.08,
                                             sigma = 0.02,
                                             rt = 0.05,
                                             Ti = 1,
                                             t = 0){
  P_x <- exp(-rt*Ti)
  P_y <- exp(-rt*t)
  
  Ft <- rt   #Since flat term-structure
   
  #Calc of A
  logA <- log(P_x/P_y) + B*Ft - (1/(4*(a^3))*(sigma^2)*((exp(-a*Ti)-exp(-a*t))^2)*(exp(2*a*t)-1))
  print(exp(logA))
}

HullWhite_Model_Implementation_sigma_p <- function(a = 0.08,
                                                   sigma = 0.02,
                                                   s = 5,
                                                   Ti = 1){
  #Calc of sigma_p
  sigma_p <- (sigma/a)*(1 - exp(-a*(s-Ti)))*sqrt((1-exp(-2*a*Ti))/(2*a))
  print(sigma_p)
}

#Cal of P(0,1)
B<- HullWhite_Model_Implementation_B()
A<- HullWhite_Model_Implementation_A()
sigma_p <- HullWhite_Model_Implementation_sigma_p() 
rt <- 0.05
P1 <- A*exp(-1*B*rt)

#Cal of P(0,5)
B<- HullWhite_Model_Implementation_B(a = 0.08,Ti = 5,t = 0)
A<- HullWhite_Model_Implementation_A(a = 0.08,sigma = 0.02,rt = 0.05,Ti = 5,t = 0)
rt <- 0.05
P5 <- A*exp(-1*B*rt)

HullWhite_Model_Implementation_h <- function(P1 = 0.9512,
                                             P5 = 0.7788,
                                             sigma_p = 0.06581336,
                                             L = 100,
                                             k = 70,
                                             t = 0){
  #Cal of h
  h <- ((1/sigma_p)*log((L*P5)/(P1*k))) + (sigma_p/2)
  print(h)
}

h <- HullWhite_Model_Implementation_h()

#Price of a European Call Option
N_h <- pnorm(h)
N_h_sigma_p <- pnorm(h-sigma_p)

HullWhite_Model_Implementation_C <- function(L = 100,
                                           s = 5,
                                           Ti = 1,
                                           k = 70,
                                           t = 0,
                                           P1 = 0.951229, 
                                           P5 = 0.778801 ){
  print(L*P5*N_h - k*P1*N_h_sigma_p)
}

C <- HullWhite_Model_Implementation_C()   
print (C) # Price of the Call Option
