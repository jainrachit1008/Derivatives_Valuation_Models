setwd('/Users/rachnish/Documents/Stevens/FE 680/Valuing_Swaps_HWModel_IntRateTrees')

## Value of a 4-Yr. Swap based on LIBOR and between USD and AUD applied to a USD Principal of $10 million ##

#Initial Specifications
AUDr <- 0.09    # annually compounded flat LIBOR rate in australia
USDr <- 0.05       # annually compounded flat LIBOR rate in America
Fixedr <- 0.08        # Fixed rate in America
ti <- c(0,1,2,3)
L <- 10000000    #in USD
sigma_Vi <- 0.30       # volatility of 1-yr. fwd rates in Australia
sigma_Wi <- 0.20       # volatility of the fwd USD-AUD FX rate
corr <- 0.3          
Taoi <- 1

#BP LIBOR after Quanto Adjustment
CF <- 0
DCF <- 0
s <- 1
for(i in ti){
  CF <- ((AUDr + AUDr*corr*sigma_Vi*sigma_Wi*i) - Fixedr)*L*Taoi
  DCF <- DCF + CF/((1+USDr)^s)
  s <- s+1
  print(CF)
}
Value_of_Swap <- DCF
print(Value_of_Swap)
