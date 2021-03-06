#Explore the classification efficiency
library(clusterCrit)
source("./source/functions.R")
source("./source/graphics.R")

load("./results/soms_sp_6x6.Rdata") #created in build maps
load("./results/soms_sp_10x10.Rdata") #created in build maps
load("./results/soms_sp_14x14.Rdata") #created in build maps
load("./data/owda_for_som.rdata")  #created in import_data

x <- som_sp_10x10$codes[[1]]
som_hc <- cutree(hclust(dist(x), method = "ward.D2"), 2)
cluster_comparison <- as.matrix(intCriteria(x, som_hc, c("all")))

for(i in 3:30){  #Takes some time: you can load below
  som_hc <- cutree(hclust(dist(x), method = "ward.D2"), i)
  cluster_comparison <- cbind(cluster_comparison, 
                             intCriteria(x, som_hc, c("all")))
  print(i)
} 
cluster_comparison <- apply(cluster_comparison, 2, unlist)
colnames(cluster_comparison) <- 2:30
cluster_comparison = data.frame(t(cluster_comparison))

save(cluster_comparison, file = "./results/cluster_comp.rdata")
load("./results/cluster_comp.rdata")

#all plots
for(i in 1:31){
  aa = parse(text = colnames(cluster_comparison)[i])
  print(xyplot(eval(aa)~2:30, cluster_comparison))
}

#some plots
xyplot(davies_bouldin ~ 2:30, cluster_comparison)
xyplot(c_index ~ 2:30, cluster_comparison)
xyplot(calinski_harabasz ~ 2:30, cluster_comparison)
xyplot(sd_scat ~ 2:30, cluster_comparison)
xyplot(sd_dis ~ 2:30, cluster_comparison)
xyplot(wemmert_gancarski ~ 2:30, cluster_comparison)

#merge clusters with owda data
som_sp_6x6_map_31 <- create.som.clusters(som_sp_6x6, som_sp_6x6_map, 31)
owda_sp_6x6 <- make.classif(som_sp_6x6_map_31, owda_raw, 31)
save(som_sp_6x6, som_sp_6x6_map, som_sp_6x6_map_17, owda_sp_6x6, file = "results/soms_sp_6x6.Rdata")

som_sp_10x10_map_31 <- create.som.clusters(som_sp_10x10, som_sp_10x10_map, 31)
owda_sp_10x10 <- make.classif(som_sp_10x10_map_31, owda_raw, 31)
save(som_sp_10x10, som_sp_10x10_map, som_sp_10x10_map_17, owda_sp_10x10, file = "results/soms_sp_10x10.Rdata")

som_sp_14x14_map_31 <- create.som.clusters(som_sp_14x14, som_sp_14x14_map, 31)
owda_sp_14x14 <- make.classif(som_sp_14x14_map_31, owda_raw, 31)
save(som_sp_14x14, som_sp_14x14_map, som_sp_14x14_map_17, owda_sp_14x14, file = "results/soms_sp_14x14.Rdata")

#Examine maximum corellation between clusters and variance: Make it a single function
cor_som_sp <- data.table(rbind(data.table(som = as.factor("6x6"), clusters = 2:31, max_cor = som.cor(owda_sp_6x6)), 
                               data.table(som = as.factor("10x10"), clusters = 2:31, max_cor = som.cor(owda_sp_10x10)),
                               data.table(som = as.factor("14x14"), clusters = 2:31, max_cor = som.cor(owda_sp_14x14))))

sd_som_sp <- data.table(rbind(data.table(som = as.factor("6x6"), clusters = 2:31, sd = som.sd(owda_sp_6x6, nclusters = 31)), 
                              data.table(som = as.factor("10x10"), clusters = 2:31, sd = som.sd(owda_sp_10x10, nclusters = 31)),
                               data.table(som = as.factor("14x14"), clusters = 2:31, sd = som.sd(owda_sp_14x14, nclusters = 31))))

g2 <- ggplot(cor_som_sp, aes(clusters, max_cor, color = som)) +
  geom_point() +
  geom_smooth(span = 0.3, se = F) +
  labs(x = "Number of clusters", y = "Maximum Correlation") + 
  scale_color_manual(values = colset_light[c(8, 11, 4)]) + 
  theme_bw() 

g1 <- ggplot(sd_som_sp, aes(clusters, sd, col = som)) +
  geom_point() +
  geom_smooth(span = 0.3, se = F) +
  labs(x = "Number of clusters", y = "Stand. Dev.") +
  scale_color_manual(values = colset_light[c(8, 11, 4)]) + 
  theme_bw() 

gg_all <- ggarrange(g1 + rremove("legend"), 
                    g2 + theme(legend.position = c(0.76, 0.33), 
                              legend.background = element_rect(fill = NA)),  
                    labels = c("a", "b"),
                    nrow = 1, ncol = 2)

ggsave("./results/figs/clustering_comp.png", gg_all, units = "cm", width = 20, height = 10)
