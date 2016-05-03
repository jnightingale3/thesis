### script to choose clustering methods
### first choose species clustering method
### then use the correct distance method to calculate CWMs via dbFD()
require(vegan) # for vegdist, decostand
require(ade4)  # for dist.binary
require(FD)    # for gowdis()
require(plyr)  # for working through a list
require(magrittr)
source('def.extract_metadata.R')

##################################################################
########################## SPECIES DATA ##########################
##################################################################

# use same species data as in functional analysis
clusdat <- spsite

# calculate distance matrices of habitat, season and both
dat.habsea <- extract_metadata(rownames(clusdat))
dist.habsea <- gowdis(dat.habsea[,4:5])
dist.hab    <- gowdis(subset(dat.habsea, select=habitat))
dist.sea    <- gowdis(subset(dat.habsea, select=season))

# order the habitat data for plotting later
ordered_habsea <- dat.habsea[order(dat.habsea$season),]
ordered_habsea <- ordered_habsea[order(ordered_habsea$habitat),]

## difference distance methods
# abundance methods
sp.jac <- vegdist(log1p(clusdat), 'jac') # Jaccard of log-transformed data
sp.cao <- vegdist(clusdat, 'cao') # Cao index
sp.cho <- vegdist(decostand(clusdat, 'norm'), 'euc') # chord distance
sp.hel <- vegdist(decostand(clusdat, 'hell'), 'euc') # Hellinger distance
# pres/abs methods
sp.bja <- vegdist((clusdat > 0), 'jac') # binary Jaccard
sp.mat <- dist.binary(clusdat, method=2)

# correlation between these
source('def.panelutils.R')
pairs(cbind(as.numeric(sp.jac), as.numeric(sp.cho), 
            as.numeric(sp.cao), as.numeric(sp.hel),
            as.numeric(sp.bja), as.numeric(sp.mat)),
      lower.panel = panel.smooth, upper.panel = panel.cor,
      diag.panel = panel.hist)
# some strong correlations, e.g. between both Jaccards, or chord and Hellinger
# others fairly weak, especially with matching coefficient
# from histograms, Cao and matching produce symettrical distributions,
# which may be a useful property


#### run all cluster and distance methods to find optimum
allthedistmats <- list(sp.jac, sp.cho, sp.cao, sp.hel, sp.bja, sp.mat)
clustermethods <- c('complete', 'average', 'mcquitty', 'ward.D', 'ward.D2')
## loop to apply all the clustering chosen methods to each distance method
alltheclusmeth <- llply(allthedistmats, function(x) {
  # this function will try a number of clustering methods
  # and return all clusters in a list
  ic <- 1 # reset
  cluslist <- list() # initialise with empty list
  for(ic in 1:length(clustermethods)) {
    cluslist[[ic]] <- hclust(x, method=clustermethods[ic])
  }
  return(cluslist)
})

### for each list of lists,
### calculate measures of interest 
### and return as a (list of) data frames
id <- 1
clusmethchoose <- list()
for(id in 1:6)  { # id = index of distance matrices
  
  clusmethchoose[[id]] <- ldply(alltheclusmeth[[id]], function(x) {
    resdf <- data.frame(
      dist.method = x$dist.method,
      clus.method = x$method,
      dima = as.character(attr(allthedistmats[[id]], 'call'))[2],
      coph_cor_p = cor(allthedistmats[[id]], cophenetic(x)),
      coph_cor_k = cor(allthedistmats[[id]], cophenetic(x), method='kendall'),
      coph_cor_s = cor(allthedistmats[[id]], cophenetic(x), method='spearman'),
      rsq_habsea = summary(lm(cophenetic(x) ~ dist.habsea))$r.squared,
      rsq_hab = summary(lm(cophenetic(x) ~ dist.hab))$r.squared,
      rsq_sea = summary(lm(cophenetic(x) ~ dist.sea))$r.squared
    )
    return(resdf)
  })
}

clusranking <- ldply(clusmethchoose,rbind)
clusranking$dismeth <- sprintf('%s %s', clusranking$dist.method, clusranking$dima)

#### what is the best by each method?
## how do the correlate?
pairs(clusranking[,-(1:3)], diag.panel = panel.hist,
      lower.panel = panel.smooth, upper.panel = panel.cor_s)
# the linear model only explains variation from habitat, not season
# copheneteic correlations are correlated, but 
# not that well (~0.75 Pearson vs others)

## find the max 
clusranking_max <- apply(clusranking[,-(1:3)], 2, which.max) 
# agreement of non-parametric coph_cors and rsq of full and habitat-only models
# which are these?
clusranking[unique(clusranking_max),]
## find the min
clusranking_min <- apply(clusranking[,-(1:3)], 2, which.min) # 9 basically the worst!
# which are these?
clusranking[unique(clusranking_min),]

## best overall method?
clusranking_ranked <- colwise(rank)(clusranking[,-(c(1:3, 9))])
format(clusranking[rank(rowMeans(clusranking_ranked), ties.method = 'min'),-c(3,5)],digits=2)

# methods of interest 
methofint <- clusranking[,c(10, 2, 4, 6:7)]
# print table sorted by each method
format(head(methofint[order(methofint$coph_cor_p, decreasing = T),]), digits=2)
format(head(methofint[order(methofint$coph_cor_s, decreasing = T),]), digits=2)
format(head(methofint[order(methofint$rsq_habsea, decreasing = T),]), digits=2)

# save tables
methofint$ID <- sprintf('%s %s', methofint$dismeth, methofint$clus.method)
bp <- (head(methofint[order(methofint$coph_cor_p, decreasing = T),c(6, 1:5)], 10))
bs <- (head(methofint[order(methofint$coph_cor_s, decreasing = T),c(6, 1:5)], 10))
br <- (head(methofint[order(methofint$rsq_habsea, decreasing = T),c(6, 1:5)], 10))

intersect(intersect(bp$ID, bs$ID), br$ID) # only cao clusdat complete in all
clusranking[which(clusranking$dist.method == 'cao' & 
                     clusranking$clus.method=='complete'),] # but it's not the best

# pairwise
intersect(head(bp$ID,5), head(bs$ID,5)) # jaccard and cao, UPGMA
intersect(head(bp$ID,7), head(br$ID,7)) # none
intersect(head(br$ID,7), head(bs$ID,7)) # none

## what are the r squared for these?
methofint[match(intersect(head(bp$ID,5), head(bs$ID,5)), methofint$ID),-c(1,2)]
# mediocre


### look at some Shepard plots
opar <- par()
par(mfrow=c(2,2))
plot(allthedistmats[[1]], cophenetic(alltheclusmeth[[1]][[2]]), 
     main='Jaccard UPGMA', xlab='Distance', ylab='Cophenetic correlation')
abline(0,1,col='red')
plot(allthedistmats[[3]], cophenetic(alltheclusmeth[[3]][[2]]), 
     main='Cao UPGMA', xlab='Distance', ylab='Cophenetic correlation')
abline(0,1,col='red')
plot(allthedistmats[[5]], cophenetic(alltheclusmeth[[5]][[2]]), 
     main='Jaccard (binary) UPGMA', xlab='Distance', ylab='Cophenetic correlation')
abline(0,1,col='red')
plot(allthedistmats[[3]], cophenetic(alltheclusmeth[[3]][[3]]), 
     main='Cao McQuitty', xlab='Distance', ylab='Cophenetic correlation')
abline(0,1,col='red')
par(opar)


#### Cao and binary Jaccard with UPGMA are the clear winners
## how similar are the distance measures?
plot(allthedistmats[[3]], allthedistmats[[5]], xlab='Cao', ylab='Jaccard binary')
cor(allthedistmats[[3]], allthedistmats[[5]])^2
cor(allthedistmats[[3]], allthedistmats[[5]], method='spearman')^2
## explains about 50% of the variance of the other


## plot the dendrograms
plot(alltheclusmeth[[3]][[2]], hang=-1) # eesh :/
plot(alltheclusmeth[[5]][[2]], hang=-1)

summary(lm(allthedistmats[[3]] ~ dist.hab ))#+ dist.sea))
AIC(lm(allthedistmats[[3]] ~ dist.hab))# + dist.sea))

summary(lm(allthedistmats[[5]] ~ dist.hab + dist.sea))
AIC(lm(allthedistmats[[5]] ~ dist.hab))# + dist.sea))

# weak but significant seasonal component



##################################################################
############################ CWM DATA ############################
##################################################################

# extract community weighted means from dbFD output
cluscwm <- fd.test$CWM
# need binary variables to be numeric [0,1]
# contender for least elegant line of code ever:
cluscwm[,c(which(as.logical(colwise(is.factor)(cluscwm)))[-1])] <-
  colwise(as.numeric)(cluscwm[,c(
    which(as.logical(colwise(is.factor)(cluscwm)))[-1])]) - 1
cwm.gow <- gowdis(cluscwm) # Gower distances between communities

### try different clustering approaches
iw <- 1 # reset
cwmlist <- list() # initialise with empty list
for(iw in 1:length(clustermethods)) {
  cwmlist[[iw]] <- hclust(cwm.gow, method=clustermethods[iw])
}

### check out the differences as above
cwmmethchoose <- ldply(cwmlist, function(x) {
  resdf <- data.frame(
    dist.method = 'Gower',
    clus.method = x$method,
    coph_cor_p = cor(cwm.gow, cophenetic(x)),
    coph_cor_k = cor(cwm.gow, cophenetic(x), method='kendall'),
    coph_cor_s = cor(cwm.gow, cophenetic(x), method='spearman'),
    rsq_habsea = summary(lm(cophenetic(x) ~ dist.habsea))$r.squared,
    rsq_hab = summary(lm(cophenetic(x) ~ dist.hab))$r.squared,
    rsq_sea = summary(lm(cophenetic(x) ~ dist.sea))$r.squared
  )
  return(resdf)
})
# UPGMA is the obvious winner (best at everything except Rsquared); 
# McQuitty is also ok

### look at Shepard plots
opar <- par()
par(mfrow=c(1,2))
plot(cwm.gow, cophenetic(cwmlist[[2]]), 
     main='Gower UPGMA', xlab='Distance', ylab='Cophenetic correlation')
abline(0,1,col='red')
plot(cwm.gow, cophenetic(cwmlist[[3]]), 
     main='Gower McQuitty', xlab='Distance', ylab='Cophenetic correlation')
abline(0,1,col='red')
par(opar)
# UPGMA is also more balanced


summary(lm(cwm.gow ~ dist.hab + dist.sea))
summary(lm(cwm.gow ~ dist.habsea))
summary(lm(cwm.gow ~ dist.sea))
# There is no seasonal component at all! only habitat!
summary(lm(cophenetic(cwmlist[[3]]) ~factor(dist.hab)))
summary(lm(cophenetic(cwmlist[[3]]) ~dist.sea))

plot(cwmlist[[3]], hang=-1)
cwmdend <- as.dendrogram(cwmlist[[3]])
click_rotate(cwmdend)
vegemite(cwm.gow, cwmlist[[3]], use = ordered_habsea)
