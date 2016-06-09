# load data
setwd('~/Dropbox/Thesis/Repo/thesis/Functional Diversity/R')


### run full distance-based FD analysis using package FD
fd.test <- dbFD(x=trait_fd, a=spsite, corr = 'cailliez', calc.FGR=F)
# summary(fd.test) # what have we got?

# llply(fd.test, .fun=print) # print all the output


## look at some CWMs

# extract this data, add habitat info and reshape for plotting

cwms <- fd.test$CWM
cwms$site <- rownames(fd.test$CWM)
cwms$habitat <- id_habitats(cwms$site)  
cwms$season  <- id_seasons(cwms$site)
cwms %<>% melt(id=c('site', 'habitat', 'season'))
cwms$value <- as.numeric(cwms$value) # convert these values to numbers for plotting
# introduces NAs where value was a factor; remove them for now
cwms <- na.omit(cwms)


p <- ggplot(cwms, aes(y=value))
p <- p + facet_wrap(~variable, scales='free_y')
ph <- p + geom_boxplot(aes(x=habitat))
print(ph) #

## pull out some interesting ones
cwms_sub <- subset(cwms, variable %in% c('mass', 'bill.length', 'tarsus'))
q <- ggplot(cwms_sub, aes(y=value))
q <- q + facet_grid(variable ~ habitat, scales='free') + ylab('CWM') +theme_bw()
qh <- q + geom_boxplot(aes(x=season, fill=season))# + theme_classic()
print(qh) 
q <- ggplot(cwms_sub, aes(y=value))
q <- q + facet_grid(variable ~ season, scales='free') + ylab('CWM') +theme_bw()
qs <- q + geom_boxplot(aes(x=habitat, fill=habitat))# + theme_classic()
print(qs) 
  

# ps <- p + geom_boxplot(aes(x=season))
# print(ps)

## look at dispersion
fdis <- data.frame(fdisp = fd.test$FDis)
fdis$habitat = id_habitats(rownames(fdis))
fdis$season = id_seasons(rownames(fdis))

p <- ggplot(fdis, aes(y=fdisp))
p <- p + geom_boxplot(aes(x=habitat))
p <- p + facet_wrap(~season, scales='free_y')
print(p)

p <- ggplot(fdis, aes(y=fdisp))
p <- p + geom_boxplot(aes(x=season))
p <- p + facet_wrap(~habitat, scales='free_y')
print(p)



## CWM = migrant - deprecated
# cwmmig <- data.frame(
#   (fd.test$CWM[,17])[fd.test$CWM[,17] != 'resdt'],
#    habitats$season[fd.test$CWM[,17] != 'resdt'],
#    habitats$habitat[fd.test$CWM[,17] != 'resdt'],
#    habitats$site[fd.test$CWM[,17] != 'resdt'])
# names(cwmmig) <- c('status', 'season', 'habitat', 'site')
# cwmmig 

#################species data########################

#### varpart

# create dummy explanatory tables
# can't include factors
habmat <- model.matrix(~ habitat, data=habitats)[,-1]
seamat <- model.matrix(~ season, data=habitats)[,-1]

varpart(Y=log1p(spsite), habmat, seamat)
plot(varpart(Y=log1p(spsite), habmat, seamat))

# pres/abs
varpart(Y=(spsite>0), habmat, seamat)
plot(varpart(Y=(spsite>0), habmat, seamat))

varpart(Y=fd.test$FDiv, habmat, seamat)
plot(varpart(Y=fd.test$FDiv, habmat, seamat))

varpart(Y=fd.test$FDis, habmat, seamat)
plot(varpart(Y=fd.test$FDis, habmat, seamat))


spclust <- hclust(dist(log1p(spsite)))
plot(spclust, labels=FALSE)
require(dendextend)
spclust.dend <- as.dendrogram((spclust))
spclust.dend <- assign_values_to_leaves_edgePar(spclust.dend,
                                                value=habitats$season,
                                                edgePar = 'col')

spclust.dend <- assign_values_to_branches_edgePar(spclust.dend,
                                                value=habitats$season,
                                                edgePar = 'col')
plot(spclust.dend, leaflab = 'none')
require(ggdendro)
ggdendrogram(spclust.dend)
print(ggplot(spclust.dend))

### PCOA
#require(ade4)
#dat.umig_short <- dat.umig[,-17]
sp.pcoa <- pcoa(dist.trait, correction = 'cailliez', rn=rownames(trait_mat))
str(sp.pcoa)
summary(sp.pcoa)
biplot(sp.pcoa)



## extract scores
sp.scores <- data.frame(sp.pcoa$li)
# join with mig status
sp.scores$mig <- master_trait$mig

with(sp.scores,plot(mig, A1))
with(sp.scores,plot(mig, A2))
with(sp.scores,plot(mig, A3))
with(sp.scores, plot(A1, A2))


