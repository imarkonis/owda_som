library(gridExtra); library(ggplot2); library(latticeExtra); library(raster); library(rasterVis)
library(rgdal); library(ggpubr); library(corrplot)

world.shp = maptools::readShapeLines("../owda/ne_50m_admin_0_countries_lakes.shp", proj4string=CRS('+proj=longlat +ellps=WGS84'))

#### Colors
rgb.palette.RdBu = colorRampPalette(rev(c('#d73027','#f46d43','#fdae61','#fee090','#fef0d9','#e0f3f8','#abd9e9','#74add1','#4575b4')), space = "rgb")

colset_mid <- c( "#4D648D", "#337BAE", "#97B8C2",  "#739F3D", "#ACBD78",  
                   "#F4CC70", "#EBB582",  "#BF9A77",
                   "#E38B75", "#CE5A57",  "#D24136", "#785A46" )
colset_mid_qual <- colset_mid[c(11, 2, 4, 6,  1, 8, 10, 5, 7, 3, 9, 12)]
palette_mid <- colorRampPalette(colset_mid)
palette_mid_qual <- colorRampPalette(colset_mid_qual)

colset_light_qual <- c("#8dd3c7",   "#fdb462", "#bebada", "#fb8072", 
                       "#80b1d3",  "#b3de69", "#ffed6f","#bc80bd",      
                       "#d9d9d9",  "#fccde5","#ccebc5", "#a1d6e2")
palette_light_qual <- colorRampPalette(colset_light_qual)

drought_palette <- colorRampPalette(c('#8c510a','#d8b365','#f6e8c3','#f5f5f5','skyblue1','skyblue3','skyblue4'), 
                                    interpolate = "spline", space = "rgb")
#### Functions
plot.som.summary <- function(nodes_in_space, fname, ...){
  nnodes = max(nodes_in_space$node)
  my.col = palette_light_qual(nnodes)
  
  png(file = paste0(fname, "_nodes.png"), width = 7.5, height = 7.5, res = 400, units = "in", type = "cairo", ...) 
  plot(lat ~ lon, data = nodes_in_space, pch = 15, col = my.col[node])
  maps::map("world", add = TRUE)
  dev.off()
  
  png(file = paste0(fname, "_summary.png"), width = 7.5, heigh = 7.5, res = 400, units = "in", type = "cairo", ...) 
  par(mfrow = c(2, 2), 
      mar = c(2, 2, 2, 2), 
      ps = 12, bg = "white", mgp = c(3, 0.2, 0))
  plot(node.lat ~ node.lon, data = nodes_in_space, cex = distance / 1000, pch = 16, col = my.col[node], xlab = "", ylab = "", main = "Distance")
  maps::map("world", add = TRUE)
  plot(node.lat ~ node.lon, data = nodes_in_space, cex = node.counts / 20, pch = 16, col = my.col[node], xlab = "", ylab = "", main = "Counts")
  maps::map("world", add = TRUE)
  plot(node.lat ~ node.lon, data = nodes_in_space, cex = node.sd.lat, pch = 16, col = my.col[node], xlab = "", ylab = "", main = "Lat sd")
  maps::map("world", add = TRUE)
  plot(node.lat ~ node.lon, data = nodes_in_space, cex = node.sd.lon, pch = 16, col = my.col[node], xlab = "", ylab = "", main = "Lon sd")
  maps::map("world", add = TRUE)
  dev.off()
}

plot.all.som.clusters <- function(nodes_in_space, fname, nclusters, ...){
  nnodes = max(nodes_in_space$node)
  my.col = c(colset_light_qual, colset_mid_qual)
  
  png(file = paste0(fname, "_clusters.png"), width = 7.5, height = 7.5, res = 400, units = "in", type = "cairo", ...) 
  par(mfrow = c(4, 4), mar = c(2, 2, 2, 2), ps = 12, bg = "white", mgp = c(3, 0.2, 0))
  for(i in 2:(nclusters)){
    print(plot(lat ~ lon, 
               data = nodes_in_space, 
               pch = 15, 
               col = my.col[as.matrix(nodes_in_space)[ ,9 + i]]))
    maps::map("world", add = TRUE)
  }
  dev.off()
}

plot.som.clusters.map <- function(nodes_in_space, som.classifications, nclusters, cor.thres, fname, network = F,...){
  my.som <- get.classif(som.classifications, nclusters)
  som.summary <- make.classification.summary(my.som)
  png(file = fname, width = 7.5, height = 7.5, res = 400, units = "in", type = "cairo", ...) 
  plot(lat~lon, 
       data = nodes_in_space, 
       pch = 15, cex = 1.5,
       xlab = "", ylab = "",
       col = som.summary$cols[as.matrix(nodes_in_space)[, 9 + nclusters]])
  maps::map("world", add=TRUE)
  if(network == T){
    som.clusters.cor.mat <- make.som.cor.mat(my.som)
    som.cors.above.thres <- get.clusters.above.thres(som.summary, som.clusters.cor.mat, cor.thres)
    all.cors <- get.clusters.above.thres(som.summary, som.clusters.cor.mat, -1)
    segments(som.cors.above.thres$lon.x, som.cors.above.thres$lat.x, som.cors.above.thres$lon.y, som.cors.above.thres$lat.y, lwd = 2, col = "#D35C37")
    points(som.summary$lon, som.summary$lat, cex = 3, pch = 16, col = "#D35C37")
    text(som.summary$lon, som.summary$lat, labels = som.summary$cluster, cex = 0.7, col = "grey90")
  }
  dev.off()
}  

plot.som.as.network <- function(som.classifications, nclusters, cor.thres){
  my.som = get.classif(som.classifications, nclusters)
  som.summary = make.classification.summary(my.som)
  som.clusters.cor.mat = make.som.cor.mat(my.som)
  som.cors.above.thres = get.clusters.above.thres(som.summary, som.clusters.cor.mat, cor.thres)
  plot(lat~lon, data = som.summary, cex = connections + 1, pch = 16, col = cols)
  maps::map("world", add = TRUE)
  segments(som.cors.above.thres$lon.x, som.cors.above.thres$lat.x, 
           som.cors.above.thres$lon.y, som.cors.above.thres$lat.y, lwd = 2)
}

plot.som.cluster.cor <- function(som.classifications, nclusters, fname){
  som.clusters.cor.mat = make.som.cor.mat(get.classif(som.classifications, nclusters)) 
  png(file = fname, width = 7.5, height = 7.5, res = 400, units = "in", type = "cairo") 
  corrplot(som.clusters.cor.mat, 
           type = "upper",  tl.col = "black", 
           tl.pos = "upper", cl.lim = c(-1, 1), 
           col = rgb.palette.RdBu(200))
  corrplot(som.clusters.cor.mat, add = TRUE, 
           type = "lower", method = "number", cl.lim = c(-1, 1), 
           col = rgb.palette.RdBu(200),  number.cex = 1.5,
           diag = FALSE, tl.pos = "n" , cl.pos = "n")
  dev.off()
}

plot.other.years.in.node <- function(nodes.in.time, yr, ...){
  plots = list()
  years.in.node = get.other.years.in.node(nodes.in.time, yr)$time
  nn = length(years.in.node)
  for(i in 1:nn){
    plots[[i]] = plot.owda.many.years(yr = years.in.node[i], ...)
  }
  grid.arrange(grobs = plots, nrow = floor(sqrt(nn)),  
               plot.margin = unit(0, "cm"), padding = unit(0, "line"))
}

plot.owda.year = function(yr, variable){
  bb = owda_raw[Time == yr,.(y=lon, x=lat, z= eval(parse(text = variable)))]
  dd = rasterFromXYZ(bb, crs = CRS('+proj=longlat +datum=WGS84'))
  oo = max(abs(bb$z), na.rm = T)
  col.seq = seq(-oo,oo,0.1)
  levelplot(dd, xlab.top = yr, xlab="", ylab="", margin=F, scales = list(y = list(tck=c(-1, -1)), x = list(tck=c(-1, -1))), main = as.character(yr),
            col.regions=drought_palette, cuts=600, at = col.seq)  + latticeExtra::layer(sp.lines(world.shp, col = "grey30",lwd=0.5))   
}

plot.owda.many.years = function(yr, variable, col.seq = seq(-6, 6, 0.1)){
  bb = owda_raw[time == yr, .(y = lon, x = lat, z = eval(parse(text = variable)))]
  dd = rasterFromXYZ(bb, crs = CRS('+proj=longlat +datum=WGS84'))
  levelplot(dd, xlab.top = yr, xlab = "", ylab = "", margin = F, 
            scales = list(y = list(tck = c(-1, -1)), x = list(tck = c(-1, -1))), main = as.character(yr),
            col.regions=drought_palette, cuts = 600, at = col.seq) + 
    latticeExtra::layer(sp.lines(world.shp, col = "grey30", lwd = 0.5))   
}

plot.soms.in.subperiods <- function(nodes_in_time, fname, clusters){ 
  subperiods <- length(nodes_in_time)
  subperiods_diff <- as.numeric(names(nodes_in_time)[2]) - as.numeric(names(nodes_in_time)[1]) # in order to plot figure title
  png(file = paste0(fname, "_clusters_in_time.png"), 
      width = 7.5, height = 10, res = 400, 
      units = "in", type = "cairo")
  par(mfrow = c(4, 3), mar = c(2, 2, 2, 2), 
      ps = 12, bg = "white", 
      mgp = c(3, 0.2, 0))
  
  for(i in 1:subperiods){
    aa <- lapply(nodes_in_time, `[[`, 9 + clusters)
    print(plot(lat ~ lon, 
               data = nodes_in_time[[i]],
               main = paste(names(nodes_in_time)[i], "-", as.numeric(names(nodes_in_time)[i]) + subperiods_diff),
               pch = 15, 
               col = colset_light_qual[as.matrix(aa[[i]])]))
    maps::map("world", add = TRUE)
  }
  dev.off()
}
