#Apply SOMs in the temporal domain

source("./source/functions.R")
source("./source/graphics.R")
load("./results/soms_tm_10x10.Rdata") #created in build maps
load("./data/owda_for_som.rdata") #created in import_data

plot.other.years.in.node(som_tm_10x10_map, 1921, "scPDSI")
plot.other.years.in.node(som_tm_10x10_map, 2003, "scPDSI")
plot.other.years.in.node(som_tm_10x10_map, 2005, "scPDSI")
plot.other.years.in.node(som_tm_10x10_map, 2007, "scPDSI")
plot.other.years.in.node(som_tm_10x10_map, 2010, "scPDSI")

som_tm_10x10_map_10 <- create.som.clusters(som_tm_10x10, som_tm_10x10_map, 10)
som_tm_10x10_map_10[clusters.10 == som_tm_10x10_map_10[time == 1921, clusters.10]]

x <- hclust(dist(som_tm_10x10$codes[[1]]))
dend <- as.dendrogram(x)
plot(as.phylo(x), type="fan")
dend <- dend %>% 
  color_branches(k=4) %>% 
  color_labels
