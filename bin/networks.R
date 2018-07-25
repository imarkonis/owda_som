# Canonical Network analysis
library(doParallel); library(doSNOW)
source("./source/functions.R")
load("./results/soms_sp_10x10.Rdata")
load("./data/owda_for_som.rdata")

#Plots to be improved

clusters <- 5:10
thres <- seq(0.3, 0.5, 0.025)

nclusters <- 10


for(i in 1:6){
  print(plot.som.as.network(owda_sp_6x6, clusters[i], thres[i]))
}

plot.som.clusters.network(owda_sp_6x6, nclusters, 0.1)
plot.som.clusters.map(som_sp_6x6_map_17, owda_sp_6x6, nclusters, 0.5, network = T)

for(i in 1:6){
  print(plot.som.clusters.map(som_sp_6x6_map_17, owda_sp_6x6, clusters[i], thres[i], network = T))
}

plot.som.clusters.map(som_sp_6x6_map_17, owda_sp_6x6, nclusters, 0.5, network = T)
plot.som.cluster.cor(owda_sp_6x6, nclusters)
#Networks in time


