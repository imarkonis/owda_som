# Canonical Network analysis

source("./source/functions.R")
load("./results/soms_sp_10x10.Rdata")
load("./data/owda_for_som.rdata")

#Plots to be improved -> Fil

for(i in 11:17){
  print(plot.som.as.network(owda_clusters, i, 0.4))
}

for(i in 11:17){
  print(plot.som.clusters.map(som_sp_10x10_map_17, owda_clusters, i, 0.4, network = T))
}

