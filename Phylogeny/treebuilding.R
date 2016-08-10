### phylo analysis, take 1
setwd('/home/josh/Dropbox/Thesis/Repo/thesis/Phylogeny')


#############################
##### Building the tree #####
#############################

## load packages
require(ape)       # consensus tree; pairwise distance 
require(ggplot2)   # plotting
require(plyr)      # going through community data
require(lattice)   # plots
require(magrittr)  # %>%
require(PCPS)      # PCPS
require(phytools)  # edge lengths
require(picante)   # NRI
require(PVR)       # PVR
require(SYNCSA)    # phlogenetic composition
require(vegan)     # ADONIS

### read in data

# mega tree in nexus format from birdtree.org
all_trees <- read.nexus('20129.tre')
# list of correspondence my taxonomy:tree taxonomy :/
spname_lookup <- read.csv("spp-name-lookup.csv")


## build tree!

# use phytools function to get consensus tree
# default is 50% majoroty-rule (consensus(..., p=0.5))
# branch length from non-negative least squares gives ultrametric tree :)
consedge <- consensus.edges(all_trees, method='least.squares')
is.ultrametric(consedge) # TRUE!
plot(consedge) # check it out



###########################
##### Alpha diversity #####
###########################

# pairwise distances between tips of tree
dist.phylo <- cophenetic.phylo(consedge)

### mean pairwise distance for communities
# get community data
phy.spsite <- subset(dat.spsite, select=-32)
# change colnames to match tree
colnames(phy.spsite) <- 
  spname_lookup$phyl_name[match(colnames(phy.spsite), spname_lookup$tax_name)]
# use underscores not spaces to match tree
colnames(phy.spsite) <- gsub(' ', '_', colnames(phy.spsite))

# check colnames match
colnames(phy.spsite) %in% colnames(dist.phylo)

# mean pairwise distance
nri.total <- ses.mpd(phy.spsite, dist.phylo,
                     null.model = 'richness',
                     abundance.weighted = TRUE,
                     runs=999, iterations=1000)
nri.total$NRI <- nri.total$mpd.obs.z * -1

### plot/analyse by season, habitat
source('def.extract_metadata.R')
nri.total <- cbind(nri.total, extract_metadata(rownames(nri.total)))

bwplot(NRI ~ habitat | season, data=nri.total)
lm.nri.hs <- lm(NRI ~ habitat * season, data=nri.total); summary(lm.nri.hs)
### significantly higher NRI on beach; no difference in NRI between seasons BUT
### disparity significantly larger in winter than in summer


##########################
##### Beta diversity #####
##########################


### community phylogenetic distance
### via fuzzy clustering
org_for_matP <- organize.syncsa(comm=phy.spsite, dist.spp=dist.phylo)
sync.tot <- matrix.p(org_for_matP$community, org_for_matP$dist.spp)
syncdist <- vegdist(sync.tot$matrix.P)

lm.pdis.hab <- adonis(syncdist ~ nri.total$habitat); (lm.pdis.hab) # sig
lm.pdis.sea <- adonis(syncdist ~ nri.total$season); (lm.pdis.sea)  # nonsig
lm.pdis.habsea <- adonis(syncdist ~ nri.total$habitat * nri.total$season); 
(lm.pdis.habsea)
# significant between habitat, not between season 


### PCPS
pcps.tot <- pcps(org_for_matP$community, org_for_matP$dist.spp, 
                 method='bray', squareroot = T)
pcps.tot

pcps.tot_eig<-pcps.tot$values # eigenvalues
pcps.tot_eig %>% head

pcps.tot_vec<-pcps.tot$vectors # eigenvectors
head(pcps.tot_vec)


pcps.tot_cor<-pcps.tot$correlations # Correlacao dos PCPS com a matriz P
head(pcps.tot_cor[,1:6])
 
scores_pcps.tot<-scores(pcps.tot, choices = c(1, 2))
scores_pcps.tot %>%plot # doesn't work
sitescores <-scores_pcps.tot$scores.sites

attr(pcps.tot_cor, 'dimnames')[1] # order of species
clades <- c(rep('Anseriformes',4), 'Gruiformes', rep('Pelecaniformes',3), rep('Scolopacidae', 3), rep('Charadriiformes', 3), 'Anseriformes', 'Charadriiformes', 'Ciconiiformes', rep('Anseriformes', 4), 'Pelecaniformes', rep('Gruiformes', 2), rep('Charadriiformes', 4), 'Ciconiiformes', 'Anseriformes', 'Pelecaniformes', 'Suliformes', 'Pelecaniformes', 'Phoenicopteriformes', rep('Pelecaniformes',2), 'Charadriiformes', rep('Podicepediformes', 3), rep('Charadriiformes', 8), rep('Scolopacidae', 2), 'Charadriiformes') %>%
  abbreviate(minlength=3)

## biplot
plot(pcps.tot, display='text')
plot(pcps.tot, display='points', groups=clades, showlabel=T)

bipdat <- as.data.frame(scores_pcps.tot$scores.sites)
bipdat %<>% cbind(phy.env)
bipdat[,3] %<>% factor %>% relevel(ref='1')
bipdat[,4] %<>% factor
names(bipdat)[3:4] <- c('Lake', 'Winter')

a <-betadisper(dist(sitescores),( (attr(sitescores, 'dimnames')[[1]])%>%id_habitats ) )
permutest(a) # sig
b <-betadisper(dist(sitescores),( (attr(sitescores, 'dimnames')[[1]])%>%id_seasons ) )
permutest(b) # not sig
# clade centroids
cc <- betadisper(dist(spscores), clades)
permutest(cc)
plot(a, main='', sub='')

#getting the convex hull of each unique point set
#code from Gota Morota via stack exchange
#http://stats.stackexchange.com/questions/22805/how-to-draw-neat-polygons-around-scatterplot-regions-in-ggplot2
find_hull <- function(df) df[chull(df$pcps.1, df$pcps.2), ]
hulls <- ddply(bipdat[,-4], "Lake", find_hull)

p <- ggplot(bipdat) + theme_bw() + geom_hline(yintercept=0) + geom_vline(xintercept=0)+
  scale_shape_manual(values = c(21, 24))
p <- p + geom_polygon(data = hulls, alpha = 0.25, aes(x=pcps.1, y=pcps.2, fill=Lake)) 
p <- p + geom_point(aes(x=pcps.1, y=pcps.2, shape=Winter, fill=Lake), size=2,
                    alpha=.75) +   
  xlab( paste('PCPS1 ', round(pcps.tot_eig[1,2]*100, 1), '%', sep='')) + 
  ylab( paste('PCPS2 ', round(pcps.tot_eig[2,2]*100, 1), '%', sep=''))
p <- p + geom_point(data=data.frame(a$centroids), aes(x=PCoA1, y=PCoA2), 
                    color=c('blue', 'red'), size=3, shape=15)#, alpha=.6)
p <- p + geom_text(data=data.frame(cc$centroids), 
                   aes(x=PCoA1, y=-PCoA2, label=attr(cc$centroids, 'dimn')[[1]]))
p <- p + theme(legend.position="none")
print(p) 

# what are the top sp scores
spscores <- scores_pcps.tot$scores.species
head(spscores[order(abs(spscores[,1]), decreasing=T),1], 30) # ducks &c; Chauna
head(spscores[order(abs(spscores[,2]), decreasing=T),2], 30) # Ciconiiformes
head(spscores[order(abs(spscores[,3]), decreasing=T),2], 30) # Ciconiiformes
# and which groups have no influence?
tail(spscores[order(abs(spscores[,1]), decreasing=T),1], 30) # rails, Ciconiiformes
tail(spscores[order(abs(spscores[,2]), decreasing=T),2], 30) # ducks


## for an example beach site, what taxa contribute most and least
## (row 114 is Praia do PARNA 2016i)
(spscores[,1]*sync.tot$matrix.P[114,])[
  order(abs(spscores[,1]*sync.tot$matrix.P[114,]))]


### do the scores relate to habitat/season?
adonis(dist(sitescores) ~ nri.total$season*nri.total$habitat)     # all PCPS
adonis(dist(sitescores[,1]) ~ nri.total$season*nri.total$habitat) # PCPS 1
adonis(dist(sitescores[,2]) ~ nri.total$season*nri.total$habitat) # PCPS 2


########################################
#############significance###############
pcps.tsig <- pcps.sig(org_for_matP$community, org_for_matP$dist.spp, ( (attr(sitescores, 'dimnames')[[1]])%>%id_habitats ), analysis = 'rda', pcps.choices = c(1, 2))

########################################


#### Checking species data
### difference between summer and winter for each species
spp_saison <- apply(phy.spsite, 2, FUN=function(x) {
  sumr <- x[nri.total$season=='Summer']
  wint <- x[nri.total$season=='Winter']
  return(mean(sumr) - mean(wint)) # calculates diff between seasonal mean counts
})

# biggest differences?
spp_saison[order(abs(spp_saison), decreasing = T)] %>% round(1)
# biggest differences, weighted by sp's influence on PCPS2
spp_saison[order(abs(spscores[,2] * spp_saison[match(rownames(spscores), names(spp_saison))]), decreasing = T)] %>% round(1) # terns top; herons bottom
# how great are those differences in influence
(spscores[,2] * spp_saison[match(rownames(spscores), names(spp_saison))])[
  order(abs(spscores[,2] * spp_saison[match(rownames(spscores), names(spp_saison))]), decreasing = T)] %>% round(1)
## they are gret, basically only L. maculipennis and 




########################################################
####### Phylogenetic signal in functional traits #######
########################################################


#### traits
phy.trait <- trait_fd[-32,]

## fix rownames
rownames(phy.trait) <- 
  spname_lookup$phyl_name[match(rownames(phy.trait), spname_lookup$tax_name)]
# use underscores not spaces to match tree
rownames(phy.trait) <- gsub(' ', '_', rownames(phy.trait))

# check colnames match
rownames(phy.trait) %in% colnames(dist.phylo) # true
# expand factor (bill shape) to 3 dummy variables
phy.trait %<>% cbind(model.matrix(~ phy.trait$bill.shape -1))
# fix column names
names(phy.trait)[grepl('[$]', colnames(phy.trait))] <- c(
  'bill.down', 'bill.straight', 'bill.up')
phy.trait %<>% subset(select=-bill.shape)

# environment
phy.env <- model.matrix(~., data=subset(nri.total, select=c(habitat, season)))[,-1]



###################
#### Using PVR ####
###################
cons.pvr <- PVRdecomp(consedge)
pvr.all <- PVR(cons.pvr, trait=phy.trait[,1])
pvr.all
VarPartplot(pvr.all) # nope
#, envVar = phy.env[1:51,]
## can only do one trait at a time?
## and vector of environmental variables must be same length as number of species??

### PSR
psr.all <- PSR(cons.pvr, trait=phy.trait[,1], Brownian.model = T) # sloooooooowww
