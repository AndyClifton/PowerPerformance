IECAEP <- function(power.curve,
                   Vave = 8,
                   version = 2005){
  
  power.curve.AEP <- power.curve[which.min(power.curve$power.binmean > 0):NROW(power.curve),]
  AEP_kWf = NULL  
  ws = NULL
  f = NULL
  p = NULL
  
  # initial values
  P0 = 0
  V0 = min(power.curve.AEP$ws.binmean) - 0.5
  Freq0 = Freq(V = V0, 
               Vave = Vave)      
  ws[1] = (power.curve.AEP$ws.binmean[1] + V0)/2
  f[1] = Freq(V = power.curve.AEP$ws.binmean[1],
              Vave = Vave) - Freq0
  p[1] = (power.curve.AEP$power.binmean[1] + P0)/2    
  
  # rest of the bins
  for (i in 2:NROW(power.curve.AEP)){
    ws[i] = (power.curve.AEP$ws.binmean[i] + power.curve.AEP$ws.binmean[i-1])/2
    f[i] = Freq(V = power.curve.AEP$ws.binmean[i], Vave = Vave) - 
      Freq(V = power.curve.AEP$ws.binmean[i-1], Vave = Vave)
    p[i] = (power.curve.AEP$power.binmean[i] + power.curve.AEP$power.binmean[i-1])/2    
  }
  
  f[f<0] <- 0
  AEP_kWf = p * f
  
  # get AEP in MWhrs
  return(list("data" = data.frame(ws = ws,
                                  freq = f,
                                  power = p,
                                  AEP_kWf),
              "AEP_kW" = 8760*sum(AEP_kWf)/1000))
}

Freq <- function(V,Vave){
  1-exp(-(pi/4)*(V/Vave)^2)
}

