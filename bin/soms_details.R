source("./source/graphics.R")
source("./source/functions.R")
load("./data/owda_for_som.rdata") #created in import_data

coolBlueHotRed <- function(n, alpha = 1) {rgb.palette.RdBu (n)[n:1]}
my.drought = colorRampPalette(c('#8c510a','#d8b365','#f6e8c3','#f5f5f5','skyblue1','skyblue3','skyblue4'), interpolate = "spline", space = "rgb")
rgb.palette.RdBu <- colorRampPalette(rep(c("#2166ac", "#67a9cf", "#d1e5f0" ,"white", "#fddbc7" ,"#ef8a62" ,"#b2182b"),each =2)[14:1], space = "rgb")


for(v in 1:(ncol(owda_for_som)-1000)){
  var_unscaled <- aggregate(owda_for_som[, 930], by = list(som_sp_6x6$unit.classif), FUN = mean, simplify = TRUE)[, 2] 
  plot(som_sp_6x6, type = "property", 
       property = var_unscaled, 
       main = colnames(owda_for_som)[930], 
       palette.name = coolBlueHotRed)
}

groups = 5
som.hc <- cutree(hclust(dist(som_sp_10x10$codes[[1]])), groups)
plot(som_sp_10x10, type = "mapping", main = "Cluster Map", bgcol = colset_light_qual[som.hc], border = NA)
add.cluster.boundaries(som_sp_10x10, som.hc)




pdf(file = "./results/figs/som_sp_6x6.pdf", width = 15, height = 15)
plot(som_sp_6x6, main = "")
dev.off()

# A not-so-quick but dirty way to plot the codes numbers
plot(som_sp_6x6, main = "", keepMargins = T)
par(new=TRUE)
plot(som_sp_6x6, main = "", type = "mapping", classif = T, labels = 1:36, border = NA, cex = 0.5)

