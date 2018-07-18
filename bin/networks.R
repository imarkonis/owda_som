# Canonical Network analysis

source("./source/functions.R")
load("./results/soms_sp_10x10.Rdata")
load("./data/owda_for_som.rdata")

#Plots to be improved

clusters <- 6:17
thres <- seq(0.4, 0.7, 0.025)

for(i in 1:12){
  print(plot.som.as.network(owda_sp_10x10, clusters[i], thres[i]))
}

for(i in 1:12){
  print(plot.som.clusters.map(som_sp_10x10_map_17, owda_sp_10x10, clusters[i], thres[i], network = T))
}



