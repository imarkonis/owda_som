# Canonical Network analysis
library(doParallel); library(doSNOW)
source("./source/functions.R")
source("./source/graphics.R")
load("./results/soms_sp_6x6.Rdata")
load("./data/owda_for_som.rdata")

#Plots to be improved

nclust <- 10
plot.som.clusters.map(nodes_in_space = som_sp_6x6_map_17, 
                      som.classifications = owda_sp_6x6, 
                      nclusters = nclust, 
                      cor.thres = 0.5, 
                      fname = "./results/figs/som_net_10_05.png",
                      network = T)
plot.som.cluster.cor(som.classifications = owda_sp_6x6, 
                     nclusters = nclust, 
                     fname = "./results/figs/som_cor_10.png")


nclust <- 17
plot.som.clusters.map(nodes_in_space = som_sp_6x6_map_17, 
                      som.classifications = owda_sp_6x6, 
                      nclusters = nclust, 
                      cor.thres = 0.5, 
                      fname = "./results/figs/som_net_17_05.png",
                      network = T)
plot.som.cluster.cor(som.classifications = owda_sp_6x6, 
                     nclusters = nclust, 
                     fname = "./results/figs/som_cor_16.png")
#Networks in time


