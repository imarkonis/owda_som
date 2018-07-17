library(gridExtra); library(ggplot2); library(latticeExtra); library(raster); library(rasterVis)
library(rgdal); library(ggpubr)

world.shp = maptools::readShapeLines("../owda/ne_50m_admin_0_countries_lakes.shp", proj4string=CRS('+proj=longlat +ellps=WGS84'))

rgb.palette.Qualitative.pale = colorRampPalette(c("#4575b4", "#78c679", "#f46d43", "#74add1", "#807dba", "#fee090", "#d9f0a3", "#d73027", "#abd9e9", "#fdae61", "#fa9fb5", "#ffed6f"), space = "rgb")
rgb.palette.Qualitative.bright = colorRampPalette(c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#ffed6f", "#b15928"))
#rgb.palette.Qualitative.mixed = colorRampPalette(c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd", "#ccebc5", "#ffed6f"))
my.drought.col = colorRampPalette(c('#8c510a','#d8b365','#f6e8c3','#f5f5f5','skyblue1','skyblue3','skyblue4'), interpolate = "spline", space = "rgb")


plot.som.as.network <- function(som.classifications, nclusters, cor.thres){
  my.som = get.classif(som.classifications, nclusters)
  som.summary = make.classification.summary(my.som)
  som.clusters.cor.mat = make.som.cor.mat(my.som)
  som.cors.above.thres = get.clusters.above.thres(som.summary, som.clusters.cor.mat, cor.thres)
  plot(lat~lon, data = som.summary, cex = connections + 1, pch = 16, col = cols)
  maps::map("world", add = TRUE)
  segments(som.cors.above.thres$lon.x, som.cors.above.thres$lat.x, som.cors.above.thres$lon.y, som.cors.above.thres$lat.y, lwd = 2)
}

plot.som.summary <- function(nodes_in_space, fname, ...){
  nnodes = max(nodes_in_space$node)
  my.col=rgb.palette.Qualitative.bright(nnodes)
  
  png(file=paste0(fname, "_nodes.png"), width=7.5, height=7.5, res=400, units="in", type="cairo", ...) 
  plot(lat~lon, data = nodes_in_space, pch= 15, col = my.col[node])
  maps::map("world", add=TRUE)
  dev.off()
  
  png(file = paste0(fname, "_summary.png"), width=7.5, height=7.5, res=400, units="in", type="cairo", ...) 
  par(mfrow = c(2, 2), 
      mar = c(2, 2, 2, 2), 
      ps=12, bg="white", mgp = c(3, 0.2, 0))
  plot(node.lat~node.lon, data = nodes_in_space, cex = 2*distance, pch= 16, col = my.col[node], xlab = "", ylab = "", main = "Distance")
  maps::map("world", add=TRUE)
  plot(node.lat~node.lon, data = nodes_in_space, cex = node.counts/20, pch= 16, col = my.col[node], xlab = "", ylab = "", main = "Counts")
  maps::map("world", add=TRUE)
  plot(node.lat~node.lon, data = nodes_in_space, cex = node.sd.lat, pch= 16, col = my.col[node], xlab = "", ylab = "", main = "Lat sd")
  maps::map("world", add=TRUE)
  plot(node.lat~node.lon, data = nodes_in_space, cex = node.sd.lon, pch= 16, col = my.col[node], xlab = "", ylab = "", main = "Lon sd")
  maps::map("world", add=TRUE)
  dev.off()
}

plot.all.som.clusters <- function(nodes_in_space, fname, nclusters, ...){
  nnodes = max(nodes_in_space$node)
  my.col=sample(rgb.palette.Qualitative.bright(nnodes), nnodes, replace = F)
  
  png(file=paste0(fname, "_clusters.png"), width=7.5, height=7.5, res=400, units="in", type="cairo", ...) 
  par(mfrow=c(4,4), mar=c(2,2,2,2), ps=12, bg="white", mgp = c(3, 0.2, 0))
  for(i in 2:(nclusters)){
    print(plot(lat~lon, 
               data = nodes_in_space, 
               pch = 15, 
               col = my.col[as.matrix(nodes_in_space)[,9+i]]))
    maps::map("world", add=TRUE)
  }
  dev.off()
}

plot.som.clusters.network <- function(som.classifications, nclusters, cor.thres){
  my.som = get.classif(som.classifications, nclusters)
  som.summary = make.classification.summary(my.som)
  som.clusters.cor.mat = make.som.cor.mat(my.som)
  som.cors.above.thres = get.clusters.above.thres(som.summary, som.clusters.cor.mat, cor.thres)
  plot(lat~lon, 
       data = som.summary, 
       cex = connections+1, 
       pch = 16, col = cols)
  maps::map("world", add=TRUE)
  segments(som.cors.above.thres$lon.x, som.cors.above.thres$lat.x, som.cors.above.thres$lon.y, som.cors.above.thres$lat.y, lwd = 2)
}

plot.som.clusters.map <- function(nodes_in_space, som.classifications, nclusters, cor.thres, network = F,...){
  my.som = get.classif(som.classifications, nclusters)
  som.summary = make.classification.summary(my.som)
  plot(lat~lon, 
       data = nodes_in_space, 
       pch = 15, 
       col = som.summary$cols[as.matrix(nodes_in_space)[,9+nclusters]])
  maps::map("world", add=TRUE)
  if(network == T){
    som.clusters.cor.mat = make.som.cor.mat(my.som)
    som.cors.above.thres = get.clusters.above.thres(som.summary, som.clusters.cor.mat, cor.thres)
    segments(som.cors.above.thres$lon.x, som.cors.above.thres$lat.x, som.cors.above.thres$lon.y, som.cors.above.thres$lat.y, lwd = 2)
  }
}  

plot.som.cluster.cor <- function(som.classifications, nclusters){
  som.clusters.cor.mat = make.som.cor.mat(get.classif(som.classifications, nclusters)) 
  corrplot(som.clusters.cor.mat, 
           type="upper", order="AOE", tl.col="black", 
           tl.pos="upper" , cl.lim=c(-1,1), 
           col = rgb.palette.RdBu(200))
  corrplot(som.clusters.cor.mat, add=TRUE, order="AOE",
           type="lower", method="number", cl.lim=c(-1,1), 
           col = rgb.palette.RdBu(200), 
           diag=FALSE, tl.pos="n" , cl.pos="n")
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
            col.regions=my.drought.col, cuts=600, at = col.seq)  + latticeExtra::layer(sp.lines(world.shp, col = "grey30",lwd=0.5))   
}

plot.owda.many.years = function(yr, variable, col.seq = seq(-6, 6, 0.1)){
  bb = owda_raw[time == yr, .(y = lon, x = lat, z = eval(parse(text = variable)))]
  dd = rasterFromXYZ(bb, crs = CRS('+proj=longlat +datum=WGS84'))
  levelplot(dd, xlab.top = yr, xlab = "", ylab = "", margin = F, 
            scales = list(y = list(tck = c(-1, -1)), x = list(tck = c(-1, -1))), main = as.character(yr),
            col.regions=my.drought.col, cuts = 600, at = col.seq) + 
    latticeExtra::layer(sp.lines(world.shp, col = "grey30", lwd = 0.5))   
}

plot.gsoms.in.subperiods <- function(nodes.in.time, fname, clusters){ 
  subperiods = length(nodes.in.time)
  
  png(file=paste0(fname, "_clusters_in_time.png"), 
      width=7.5, height=7.5, res=400, 
      units="in", type="cairo")
  par(mfrow=c(4,4), mar=c(2,2,2,2), 
      ps=12, bg="white", 
      mgp = c(3, 0.2, 0))
  
  for(i in 1:subperiods){
    aa = lapply(nodes.in.time, `[[`, 9+clusters)
    print(plot(lat~lon, 
               data = nodes.in.time[[i]], 
               pch = 15, 
               col = rgb.palette.Qualitative.bright(clusters)[as.matrix(aa[[i]])]))
    maps::map("world", add=TRUE)
  }
  dev.off()
}