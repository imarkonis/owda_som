#Plot the SOM, some summary statistics and all the different clusters until a given number

source("./source/functions.R")
source("./source/graphics.R")
load("./results/soms_sp_6x6.Rdata")
load("./results/soms_sp_8x8.Rdata")
load("./results/soms_sp_10x10.Rdata")

nclusters = 17

som_sp_6x6_map_17 <- create.som.clusters(som_sp_6x6, som_sp_6x6_map, nclusters)
plot.som.summary(som_sp_6x6_map_17, "./results/figs/som_sp_6x6_1k")
plot.all.som.clusters(som_sp_6x6_map_17, "./results/figs/som_sp_6x6_1k", nclusters)
save(som_sp_6x6, som_sp_6x6_map, som_sp_6x6_map_17, file = "results/soms_sp_6x6.Rdata")

som_sp_8x8_map_17 <- create.som.clusters(som_sp_8x8, som_sp_8x8_map, nclusters)
plot.som.summary(som_sp_8x8_map_17, "./results/figs/som_sp_8x8_1k")
plot.all.som.clusters(som_sp_8x8_map_17, "./results/figs/som_sp_8x8_1k", nclusters)
save(som_sp_8x8, som_sp_8x8_map, som_sp_8x8_map_17, file = "results/soms_sp_8x8.Rdata")

som_sp_10x10_map_17 <- create.som.clusters(som_sp_10x10, som_sp_10x10_map, nclusters)
plot.som.summary(som_sp_10x10_map_17, "./results/figs/som_sp_10x10_10k")
plot.all.som.clusters(som_sp_10x10_map_17, "./results/figs/som_sp_10x10_10k", nclusters)
save(som_sp_10x10, som_sp_10x10_map, som_sp_10x10_map_17, file = "results/soms_sp_10x10.Rdata")