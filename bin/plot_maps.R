#Plot the SOM, some summary statistics and all the different clusters until a given number

source("./source/functions.R")
source("./source/graphics.R")
load("./results/soms_sp_6x6.Rdata") #created in build maps
load("./results/soms_sp_10x10.Rdata") #created in build maps
load("./results/soms_sp_14x14.Rdata") #created in build maps

nclusters = 17

som_sp_6x6_map_17 <- create.som.clusters(som_sp_6x6, som_sp_6x6_map, nclusters)
plot.som.summary(som_sp_6x6_map_17, "./results/figs/som_sp_6x6_100k")
plot.all.som.clusters(som_sp_6x6_map_17, "./results/figs/som_sp_6x6_100k", nclusters)
save(som_sp_6x6, som_sp_6x6_map, som_sp_6x6_map_17, file = "results/soms_sp_6x6.Rdata")

som_sp_10x10_map_17 <- create.som.clusters(som_sp_10x10, som_sp_10x10_map, nclusters)
plot.som.summary(som_sp_10x10_map_17, "./results/figs/som_sp_10x10_100k")
plot.all.som.clusters(som_sp_10x10_map_17, "./results/figs/som_sp_10x10_100k", nclusters)
save(som_sp_10x10, som_sp_10x10_map, som_sp_10x10_map_17, file = "results/soms_sp_10x10.Rdata")

som_sp_14x14_map_17 <- create.som.clusters(som_sp_14x14, som_sp_14x14_map, nclusters)
plot.som.summary(som_sp_14x14_map_17, "./results/figs/som_sp_14x14_10k")
plot.all.som.clusters(som_sp_14x14_map_17, "./results/figs/som_sp_14x14_10k", nclusters)
save(som_sp_14x14, som_sp_14x14_map, som_sp_14x14_map_17, file = "results/soms_sp_14x14.Rdata")

### Comparison of different SOM structure for same number of clusters
nclusters = 5
my.col = sample(rgb.palette.Qualitative.bright(nclusters), nclusters, replace = F)

png(file = paste0("./results/figs/compare_soms_", nclusters, ".png"), width = 7.5, height = 2.5, res = 400, units = "in", type = "cairo") 
par(mfrow = c(1, 3), mar=c(2, 2, 2, 2), ps = 12, bg="white", mgp = c(3, 0.2, 0))
plot(lat ~ lon, 
             data = som_sp_6x6_map_17, 
             pch = 15, 
             col = my.col[as.matrix(som_sp_6x6_map_17)[, 9 + nclusters]])
maps::map("world", add = TRUE)
plot(lat ~ lon, 
           data = som_sp_10x10_map_17, 
           pch = 15, 
           col = my.col[as.matrix(som_sp_10x10_map_17)[, 9 + nclusters]])
maps::map("world", add = TRUE)
plot(lat ~ lon, 
           data = som_sp_14x14_map_17, 
           pch = 15, 
           col = my.col[as.matrix(som_sp_14x14_map_17)[, 9 + nclusters]])
maps::map("world", add = TRUE)
dev.off()
