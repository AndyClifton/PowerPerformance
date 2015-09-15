# read data
data.in <- read.csv('PCWG_Dataset1_TimeSeries.csv')
# map data to standard naming
PCWGDataSet1 <- data.frame(ws.HH = data.in$Hub.Wind.Speed,                           
                           Ti.HH = data.in$Hub.Turbulence,
                           shear = data.in$Shear.Exponent,
                           ws.eq = data.in$Rotor.Equivalent.Wind.Speed,
                           power.mean = data.in$Hub.Power)
save(PCWGDataSet1, file= "PCWGDataSet1.Rda")