################## functional data ########################

#extract and need to remove non-numeric cols
#TODO convert into dummy variables
CWM <- assign_habitat(fd.test$CWM, site=rownames(fd.test$CWM))
CWM %<>% extract(,colSums(is.na(CWM)) == 0) # only loses migration, which dont need
row.names(CWM) <- row.names(spsite)

CWM_res <- CWM[,-(c(14,17:20))]
CWM_res[14] <- as.numeric(paste(CWM_res[,14]))
CWM_pre <- CWM[,17:20]
#### varpart

# create dummy explanatory tables
# can't include factors
habmat <- model.matrix(~ habitat, data=CWM_pre)[,-1]
seamat <- model.matrix(~ season, data=CWM_pre)[,-1]

varpart(Y=CWM_res, habmat, seamat)
plot(varpart(Y=CWM_res, habmat, seamat))

# varpart(Y=fd.test$FDiv, habmat, seamat)
# plot(varpart(Y=fd.test$FDiv, habmat, seamat))
# 
# varpart(Y=fd.test$FDis, habmat, seamat)
# plot(varpart(Y=fd.test$FDis, habmat, seamat))



fdclust <- hclust(vegdist(CWM_res, method='gower'))
#plot(fdclust, labels=FALSE)
require(dendextend)
fdclust.dend <- as.dendrogram((fdclust))

## define seasons
fdseason <- rep('summer', nrow(dendro_data(fdclust.dend)$labels))
fdseason[grep('inv',(dendro_data(fdclust.dend)$labels)$label)] <- 'winter'
fdseason %<>% data.frame() %>%
 assign_habitat(site=(dendro_data(fdclust.dend)$labels)$label)


## make tree coloured by season
require(ggdendro)
fdclust.dend %<>%
  assign_values_to_leaves_edgePar(value=fdseason$season, edgePar = 'col') %>%
  # assign_values_to_branches_edgePar(value=fdseason, edgePar = 'col') %>%
  # set('branches_col', c('green', 'red', 'blue')) %>%
  ggplot %>% print

fdclust.dend %<>%
  assign_values_to_leaves_edgePar(value=fdseason$habitat, edgePar = 'col') %>%
  # assign_values_to_branches_edgePar(value=fdseason, edgePar = 'col') %>%
  # set('branches_col', c('green', 'red', 'blue')) %>%
  ggplot %>% print

# ggdendrogram(fdclust.dend)

habord <- order(fdseason$habitat)
reorder(fdclust.dend, wts=habord)
