#Create the SOMs

source("./source/functions.R")
load("./data/owda_for_som.rdata") #created in import_data

#Spatial domain
som_sp_6x6 <- som(X = owda_for_som, 
                    grid = somgrid(6, 6, "hexagonal"), #dimensions
                    alpha = c(0.01, 0.001), 
                    keep.data = TRUE, 
                    rlen = 1000) #number of iterations: USE 1000, because it takes time
som_sp_6x6_map <- put.som.in.space(som_sp_6x6, owda_coords)
save(som_sp_6x6, som_sp_6x6_map, file = "results/soms_sp_6x6_1k.Rdata") 

som_sp_10x10 <- som(X = owda_for_som, 
                    grid = somgrid(10, 10, "hexagonal"), #dimensions
                    alpha = c(0.01, 0.001), 
                    keep.data = TRUE, 
                    rlen = 1000) #number of iterations: USE 100, because it takes a lot of time
som_sp_10x10_map <- put.som.in.space(som_sp_10x10, owda_coords)
save(som_sp_10x10, som_sp_10x10_map, file = "results/soms_sp_10x10_1k.Rdata") 

som_sp_14x14 <- som(X = owda_for_som, 
                    grid = somgrid(14, 14, "hexagonal"), #dimensions
                    alpha = c(0.01, 0.001), 
                    keep.data = TRUE, 
                    rlen = 1000) #number of iterations: USE 100, because it takes a lot of time
som_sp_14x14_map <- put.som.in.space(som_sp_14x14, owda_coords)
save(som_sp_14x14, som_sp_14x14_map, file = "results/soms_sp_14x14_1k.Rdata") 

#Temporal domain
owda_for_som_tm <- cbind(992:2012, t(owda_for_som))

som_tm_10x10 <- som(X = owda_for_som_tm[, -1], 
                    grid = somgrid(10, 10, "hexagonal"), #dimensions
                    alpha = c(0.01, 0.001), 
                    keep.data = TRUE, 
                    rlen = 1000) #number of iterations

som_tm_10x10_map <- put.som.in.time(som_tm_10x10, 992)

save(som_tm_10x10, som_tm_10x10_map, file = "results/soms_tm_10x10.Rdata")
