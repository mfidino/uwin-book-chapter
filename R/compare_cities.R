library(sf)
library(FedData)
library(raster)
library(dplyr)
library(NbClust)
library(factoextra)
library(FactoMineR)

dat <- readRDS("./data/municipal_with_covars.RDS")

contig <- readRDS("./data/contig_US.RDS")

# get just the covariates

covar <- dat[,c("water", "urban_open","urban", 
                "forest", "shrub", "ag")]
covar <- data.frame(covar)[,1:6]
cpca <- prcomp(covar[,-1])



my_tss <- rep(NA, 10)
for(i in 1:20){
  my_tss[i] <- kmeans(cpca$x, i)$tot.withinss
  
}

nclust <- fviz_nbclust(
  cpca$x, kmeans, "silhouette"
)

# based on this it is three
my_clust <- kmeans(cpca$x, 3)

svg("pca_plot.svg", height = 6, width = 8)
plot(cpca$x[,1:2], type = "n", bty = "l", las = 1,
     xlim = c(-0.8,0.8), ylim = c(-0.8,0.8), cex.lab = 1.2)

my_col <- c("#469f81ff", "#f8ca4bff","#964f35ff")
for(i in 1:3){
  loca <- which(my_clust$cluster == i)
  points(
    cpca$x[loca,1:2],
    pch = i + 20,
    bg = scales::alpha(my_col[i], 0.4),
    cex = 1.1
  )
}
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)

# get arrows
ml <- cpca$rotation
for(i in 3:5){
  arrows(
    0,0, ml[i,1], ml[i,2],
    lwd = 3
  )
  
}

text(x = ml[3,1], y = ml[3,2], "Forest", pos = 3, cex = 1.2)
text(x = ml[4,1], y = ml[4,2], "Shrub", pos = 4, cex = 1.2)
text(x = ml[5,1]-0.07, y = ml[5,2]+0.02, "Agriculture", pos = 3, 
     cex = 1.2)


legend(
  "bottomleft",
  legend = c("Forest", "Agriculture", "Shrub"),
  bty = "n",
 # title = "Dominant landcover of metro area",
  pch = 21:23,
  pt.bg = scales::alpha(my_col, 0.6),
  cex = 1.2
  )
text(
  x = -0.86,
  y = -0.45,
  labels = "Dominant landcover",
  pos = 4,
  cex = 1.2
)  
dev.off()

plot(dat["forest"])

svg("forest_cover.svg", height = 7, width = 7)
plot(contig, main = "Forest", col = "gray60")
plot(dat["forest"], add = TRUE, breaks = seq(0,1,0.1))
dev.off()

svg("shrub_cover.svg", height = 7, width = 7)
plot(contig, main = "Shrub", col = "gray60")
plot(dat["shrub"], add = TRUE, breaks = seq(0,1,0.1))
dev.off()

svg("urban_cover.svg", height = 7, width = 7)
plot(contig, main = "Urban", col = "gray60")
plot(dat["urban"], add = TRUE, breaks = seq(0,1,0.1))
dev.off()

svg("ag_cover.svg", height = 7, width = 7)
plot(contig, main = "Agriculture", col = "gray60")
plot(dat["ag"], add = TRUE, breaks = seq(0,1,0.1))
dev.off()

svg("urban_open.svg", height = 7, width = 7)
plot(contig, main = "Urban openspace", col = "gray60")
plot(dat["urban_open"], add = TRUE, breaks = seq(0,1,0.1))
dev.off()



