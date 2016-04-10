# load data
setwd('~/Dropbox/Thesis/Repo/thesis/Functional Diversity/R')
source('dat.mig.ufrj.R')

# load packages
require(FD) # for functional diversity metrics
require(reshape2) # for data manipulation for plotting

# create new data frame for this analysis
# only including species for which I have data
# TODO complete trait matrix!
spsite <- dat.spsite[,names(dat.spsite) %in% dat.umig$sp]
# add site to row names to identify communities 
row.names(spsite) <- as.character(dat.spsite$site)
# analysis requires at least 3 unique species to be observed!
spsite <- spsite[(rowSums(spsite != 0)>2),]

# store species names in row names - can't include as a column
row.names(dat.umig) <- dat.umig$sp

# species data need to be identical & in the same order in both matrices
spsite <- spsite[,sort(dat.umig$sp)] %>% droplevels
dat.umig <- dat.umig[sort(dat.umig$sp),-c(1:3)] %>% droplevels
# check
identical(row.names(dat.umig), row.names(t(spsite)))
sum(rowSums(spsite) > 2) == nrow(spsite)

# run full distance-based FD analysis using package FD
fd.test <- dbFD(x=dat.umig, a=spsite)
# summary(fd.test) # what have we got?

# llply(fd.test, .fun=print) # print all the output


## look at some CWMs

# extract this data, add habitat info and reshape for plotting
source('dat.habitat.ufrj.R')
cwms <- assign_habitat(fd.test$CWM, site=rownames(fd.test$CWM)) %>% 
  melt(id=c('site', 'habitat', 'season'))
cwms$value <- as.numeric(cwms$value) # convert these values to numbers for plotting
# introduces NAs where value was a factor; remove them for now
cwms <- na.omit(cwms)

require(ggplot2)
p <- ggplot(cwms, aes(y=value))
p <- p + facet_wrap(~variable, scales='free_y')
   
ph <- p + geom_boxplot(aes(x=habitat))
print(ph) #too many boxes!

## pull out some interesting ones
cwms_sub <- subset(cwms, 
                   variable %in% c('Diet.Inv', 'Diet.Vfish', 
                                   'ForStrat.watbelowsurf', 'ForStrat.ground'))
q <- ggplot(cwms_sub, aes(y=value))
q <- q + facet_grid(variable ~ habitat) + ylab('CWM') +theme_bw()
qh <- q + geom_boxplot(aes(x=season, fill=season))# + theme_classic()
print(qh) 
  

# ps <- p + geom_boxplot(aes(x=season))
# print(ps)

## look at dispersion
fdis <- fd.test$FDis %>% cbind(habitats) 
names(fdis)[1] <- 'fdisp' 

p <- ggplot(fdis, aes(y=fdisp))
p <- p + geom_boxplot(aes(x=habitat))
p <- p + facet_wrap(~season, scales='free_y')
print(p)

p <- ggplot(fdis, aes(y=fdisp))
p <- p + geom_boxplot(aes(x=season))
p <- p + facet_wrap(~habitat, scales='free_y')
print(p)



## CWM = migrant
cwmmig <- data.frame(
  (fd.test$CWM[,17])[fd.test$CWM[,17] != 'resdt'],
   habitats$season[fd.test$CWM[,17] != 'resdt'],
   habitats$habitat[fd.test$CWM[,17] != 'resdt'],
   habitats$site[fd.test$CWM[,17] != 'resdt'])
names(cwmmig) <- c('status', 'season', 'habitat', 'site')
cwmmig 

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
require(ade4)
dat.umig_short <- dat.umig[,-17]
sp.pcoa <- dudi.pco(vegdist(dat.umig_short, method='gower'))
str(sp.pcoa)
summary(sp.pcoa)
biplot(sp.pcoa)



## extract scores
sp.scores <- data.frame(sp.pcoa$li)
# join with mig status
sp.scores$mig <- dat.umig$mig

with(sp.scores,plot(mig, A1))
with(sp.scores,plot(mig, A2))
with(sp.scores,plot(mig, A3))
with(sp.scores, plot(A1, A2))


