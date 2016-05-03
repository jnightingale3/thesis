spclust <- hclust(dist.spp, method='ward.D')
#plot(spclust)
require(dendextend)
spclust.dend <- as.dendrogram((spclust))
# spclust.dend <- assign_values_to_leaves_edgePar(spclust.dend,
#                                                 value=id_seasons(labels(spclust)),
#                                                 edgePar = 'col')

spclust.dend <- assign_values_to_leaves_edgePar(
  spclust.dend, value=(id_habitats((spclust$labels)[spclust$order])),
    # factor(paste(id_habitats((spclust$labels)[spclust$order]),
    #       id_seasons((spclust$labels)[spclust$order]), sep=''))),
                                                  edgePar = 'col')

plot(spclust.dend)


# sil plot
cutk <- cutree(spclust.dend, k=3)
silk <- silhouette(cutk, dist.spp)
sils <- sortSilhouette(silk)
plot(sils, col=cutk+1)
