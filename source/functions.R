library(data.table); library(reshape2); library(kohonen); library(doSNOW); library(foreach)

put.som.in.space <- function(som_map, id_cords){
  som_results <- data.table(id = id_cords$id, 
                       lat = id_cords$lat, 
                       lon = id_cords$lon,
                       node = som_map$unit.classif, 
                       distance = som_map$distances)
  nodes_in_space <- add.coords.to.som(som_results)
  return(nodes_in_space)
}

put.som.in.time <- function(som_map, start_time){
  bb <- data.table(cbind(start_time:(start_time + length(som_map$unit.classif) - 1), 
                         som_map$unit.classif, 
                         som_map$distances))
  names(bb) <- c("time", "node", "distance")
  return(bb) 
}

add.coords.to.som <- function(nodes_in_space){
  nodes_in_space[, node.lat := median(lat), by = node]
  nodes_in_space[, node.lon := median(lon), by = node]
  nodes_in_space[, node.sd.lat := sd(lat), by = node]
  nodes_in_space[, node.sd.lon := sd(lon), by = node]
  nodes_in_space[, node.counts := .N, by = node]
}

create.som.clusters <- function(som.map, nodes_in_space, nclusters){
  out <- data.table(nodes_in_space)
  for(i in 2:nclusters){
    som.hc <- unname(cutree(hclust(dist(som.map$codes[[1]]), method = "ward.D2"), i))
    out[, paste0("clusters.", i) := som.hc[out$node]]
  }
  return(cl.rename(out))
}

cl.rename <- function(dta) {
  cl_numbers_old <- dta[, grepl('clusters', colnames(dta)), with = F] 
  cl_numbers_raw <- as.numeric(gsub('clusters.', '', names(dta[, grepl('clusters', colnames(dta)), with = F])))
  id <- 1:dim(dta)[1]
  cl_id <- list()
  
  for(j in 1:dim(cl_numbers_old)[2]) {
    tmp <- list()
    for(i in 1:max(cl_numbers_old[, j, with = F])) {
      tmp[[i]] <- id[which(cl_numbers_old[, j, with = F] == i)]
    }
    cl_id[[j]] <- tmp
    names(cl_id[[j]]) <- paste0(1:max(cl_numbers_old[, j,  with = F]))
  }
  
  for(i in 1:(dim(cl_numbers_old)[2] - 1)) {
    names(cl_id[[i + 1]][cl_id[[i + 1]] %in% cl_id[[i]]])
    names(cl_id[[i + 1]]) <- as.character(names(cl_id[[i + 1]]))
    names(cl_id[[i + 1]])[which(cl_id[[i + 1]] %in% cl_id[[i]])] <- names(cl_id[[i]])[which(cl_id[[i]] %in% cl_id[[i + 1]])]
    
    if(length(cl_id[[i + 1]][!(cl_id[[i + 1]] %in% cl_id[[i]])]) != 2) next 
    if(length(cl_id[[i + 1]][!(cl_id[[i + 1]] %in% cl_id[[i]])][[1]]) >= length(cl_id[[i + 1]][!(cl_id[[i + 1]] %in% cl_id[[i]])][[2]])) {
      names(cl_id[[i + 1]])[!(cl_id[[i + 1]] %in% cl_id[[i]])][1] <- names(cl_id[[i]])[!(cl_id[[i]] %in% cl_id[[i + 1]])]
      names(cl_id[[i + 1]])[!(cl_id[[i + 1]] %in% cl_id[[i]])][2] <- cl_numbers_raw[i + 1]
    } else {
      names(cl_id[[i + 1]])[!(cl_id[[i + 1]] %in% cl_id[[i]])][1] <- cl_numbers_raw[i + 1]
      names(cl_id[[i + 1]])[!(cl_id[[i + 1]] %in% cl_id[[i]])][2] <- names(cl_id[[i]])[!(cl_id[[i]] %in% cl_id[[i + 1]])]
    }
  }
  
  cl_numbers_new <- as.data.frame(cl_numbers_old)
  
  for(i in 1:length(cl_id)) {
    for(j in 1:length(cl_id[[i]])) {
      cl_numbers_new[, i][cl_id[[i]][[j]]] <- as.numeric(names(cl_id[[i]])[j])
    }
  }
  
  out <- as.data.frame(dta)
  out[, grepl('clusters', colnames(dta))] <- cl_numbers_new
  
  return(data.table(out))
}

put.classif.to.dataset <- function(dataset, nodeset, nclusters){
  aa = parse(text = paste0("clusters.", nclusters))
  oo = merge(nodeset[, .(id, lat, lon, cluster = eval(aa))], dataset,  by = c("id","lat", "lon"))
  return(oo)
}

make.classif <- function(nodes_in_map, dataset, nclusters){
  my.som = merge(nodes_in_map, dataset,  by = c("id","lat", "lon"))
  my.som.classifications = list()
  for(i in 1:(nclusters-1)){
    cluster.name = parse(text = paste0("clusters.", i + 1))
    my.som.classifications[[i]] = my.som[,.(median(lat), median(lon), mean(scPDSI), sd(scPDSI)), list(eval(cluster.name), time)] 
    names(my.som.classifications[[i]]) = c("cluster", "time", "lat", "lon", "m.scPDSI", "sd.scPDSI")
  } 
  return(my.som.classifications)
}

get.classif <- function(som.classifications, nclusters){
  return(som.classifications[[nclusters-1]])
}  

make.som.cor.mat <- function(my.som){
  clusters.som.mat = acast(my.som, time~cluster,value.var = "m.scPDSI" )
  clusters.som.cor.mat = cor(clusters.som.mat)
  return(clusters.som.cor.mat)
}

get.cluster.coords.for.sgmt <- function(som.summary, som.clusters.cor.mat){
  som.coords = melt(som.clusters.cor.mat)
  som.coords = data.table(som.coords[som.coords$value != 1,])
  som.coords = merge(som.coords, som.summary, by.y = "cluster", by.x = "Var1")
  som.coords = merge(som.coords, som.summary, by.y = "cluster", by.x = "Var2")
  return(som.coords[,.(lat.x, lon.x, lat.y, lon.y, cor = value, cluster.1 = Var2, cluster.2 = Var1)])
}

get.clusters.above.thres <- function(som.summary, som.clusters.cor.mat, cor.thres){
  som.cors = get.cluster.coords.for.sgmt(som.summary, som.clusters.cor.mat) 
  som.cors.above.thres = som.cors[cor > cor.thres]
  return(som.cors.above.thres)
}

get.connections.between.clusters <- function(som.summary, som.clusters.cor.mat, cor.thres){
  som.cors = get.clusters.above.thres(som.summary, som.clusters.cor.mat, cor.thres)
  connec = som.cors[,.N, cluster.1]
  return(connec)
}

network_palette <- colorRampPalette(c("#BF9A77", "#D6C6B9", "#ACBD78", "#97B8C2"))
make.classification.summary <- function(selected.som){
  som.cor.mat = make.som.cor.mat(selected.som) 
  selected.som.summary = selected.som[!duplicated(cluster), .(cluster, lat, lon)]
  no.connected.clusters = get.connections.between.clusters(selected.som.summary, som.cor.mat, 0.5)  
  selected.som.summary = merge(no.connected.clusters, selected.som.summary, by.x = "cluster.1", by.y = "cluster", all.y = T)
  selected.som.summary$N[is.na(selected.som.summary$N)] = 0
  names(selected.som.summary)[1:2] = c("cluster", "connections")
  selected.som.summary[, cols := network_palette(nrow(selected.som.summary))[cluster]] 
  return(selected.som.summary)
}

get.other.years.in.node <- function(nodes.in.time, yr){
  nodes.in.time <- data.table(nodes.in.time)
  years.in.node = nodes.in.time[node == nodes.in.time[time == yr, node]]
  years.in.node = years.in.node[order(years.in.node$distance)]
  return(years.in.node)
}

som.cor <- function(owda_clusters){
  cluster_cor <- lapply(owda_clusters, make.som.cor.mat)
  cluster_cor <- lapply(cluster_cor, function(y) apply(y, 1, function(x) max(x[x < 1])))
  cluster_cor <- sapply(cluster_cor, mean)
  return(cluster_cor)
}

som.sd <- function(owda_clusters, nclusters){
  as.numeric(rapply(owda_clusters, mean)[seq(6, 6 * (nclusters - 1), 6)])
} 

soms.period <- function(data_set, 
                        periods, 
                        som_grid = somgrid(6, 6, "hexagonal"), 
                        som_iter = 1000, ...){
  
  no_cores <- as.numeric(Sys.getenv('NUMBER_OF_PROCESSORS')) - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster = makeCluster(no_cores, type = "SOCK")
  registerDoSNOW(cluster)
  
  soms_in_period <- foreach(i = 1:(length(periods) - 1)) %dopar% {
    kohonen::som(X = data_set[, periods[i]:periods[i + 1]],
                 grid = som_grid, #dimensions
                 alpha = c(0.01, 0.001), 
                 keep.data = TRUE, 
                 rlen = som_iter) 
  }
  return(soms_in_period)
}
put.soms.period.in.space <- function(soms_in_periods, id_cords, nclusters){
  soms_in_space = list()
  for(i in 1:length(soms_in_periods)){
    soms_in_space[[i]] <- put.som.in.space(soms_in_periods[[i]], id_cords)
    soms_in_space[[i]] <- create.som.clusters(soms_in_periods[[i]], soms_in_space[[i]], nclusters)
  }
  names(soms_in_space) <- names(soms_in_periods)
  return(soms_in_space)
}

