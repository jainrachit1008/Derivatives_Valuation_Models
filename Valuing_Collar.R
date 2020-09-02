##setwd('/Users/rachnish/Documents/Stevens/FE 680/Valuing Swaptions & Collar')##


## Valuing a 5-yr. collar boundaring the LIBOR interest rates between 5% and 7% ##

#LIBOR Flat Curve
m = 4
Rc <- 0.06         #Continuously compounded
Rm <- m*(exp(Rc/m) - 1)     # Quarterly Compounded

#Parameters for valuation of collar
L <- 100   # Principal
Sigma_Forward <- 0.20   # Forward Volatility
RCap <- 0.07     # Cap on Interest rates
RFloor <- 0.05     # Floor on Interest rates
Delta_k <- 0.25       # Frequency of resets
t <- seq(from = 0.25, to = 4.75, by = 0.25)    #time of resets
T <- seq(from = 0.5, to = 5, by = 0.25)     #time of payoffs

#Valuing each caplet
Caplets_sum <- 0
Value_caplet <- 0
d1 = 0
d2 = 0
PVF = 0     # PV Factor
for(i in 1:length(t)){
  d1 = (log(Rm/RCap) + (Sigma_Forward^2)*(t[i]/2)) / (Sigma_Forward*sqrt(t[i]))
  d2 = d1 - Sigma_Forward*sqrt(t[i])
  PVF = exp(-Rc*T[i])
  Value_caplet[i] = (Rm*pnorm(d1) - RCap*pnorm(d2))*L*Delta_k*PVF
  print(Value_caplet[i])
  Caplets_sum = Caplets_sum + Value_caplet[i]
}
Value_Cap <- Caplets_sum      #Value of Cap at 7%

#Valuing each floorlet
Floorlets_sum <- 0
Value_floorlet <- 0
d1_f = 0
d2_f = 0
PVF_f = 0
for(m in 1:length(t)){
  d1_f = (log(Rm/RFloor) + (Sigma_Forward^2)*(t[m]/2)) / (Sigma_Forward*sqrt(t[m]))
  d2_f = d1_f - Sigma_Forward*sqrt(t[m])
  PVF_f = exp(-Rc*T[m])
  Value_floorlet[m] = (RFloor*pnorm(-d2_f)-Rm*pnorm(-d1_f))*L*Delta_k*PVF_f
  print(Value_floorlet[m])
  Floorlets_sum = Floorlets_sum + Value_floorlet[m]
}
Value_Floor <- Floorlets_sum         # Value of Floor at 5%


#Value of Collar
Value_Collar <- Value_Cap - Value_Floor      # Long on Cap@7% & Short on Floor@5%

print(Value_Collar)
print(Value_Cap)
print(Value_Floor)
