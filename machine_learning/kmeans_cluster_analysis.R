library(dslabs)
library(dplyr)
library(tidyverse)
library(stats)
library(cluster)    # clustering algorithms
library(factoextra)

data("tissue_gene_expression")
#calculate distance

?kmeans

# Dissimilarity matrix
d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))

# Hierarchical clustering using Complete Linkage

hc<-hclust(d, method = "complete")

# Plot the obtained dendrogram
plot(hc,,label=rownames(tissue_gene_expression$x))

#kmeans with 7 clusters
k2<-kmeans(tissue_gene_expression$x,centers=7)
table(k2$cluster,tissue_gene_expression$y)
fviz_cluster(k2, data = d)

library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]

#heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = sample(colors))
#heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = sample(colors))
#heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = rev(colors))
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)

