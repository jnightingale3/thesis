### This file takes the species-site and trait data
### and converts into two distance matrices,
### one for census and one for species traits

### require packages 
# require(vegan) # vegdist() function for Bray-Curtis distance
# require(cluster) # daisy() for Gower distance on mixed numeric/class variables

### species data
## this one is straightforward, using Cao index
## log1p() function does ln(x+1) transformation (don't need with Cao)
dist.spp <- vegdist((dat.spsite>0), method='cao')
# dist.spp <- vegdist(dat.spsite>0, method='raup')

### trait data
## to give equal weight to the three types of traits,
## morphological, dietary and behavioural,
## a separate distance matrix is calculated for each type of trait
## which is then averaged to give an overall distance matrix

## only create matrices for species included in analysis
traitsp <- which(master_trait$species %in% colnames(dat.spsite))

# behavioural
dist.beh <- gowdis(behav_mat[traitsp,], asym.bin = 1:ncol(behav_mat))
# diet contents
dist.diet <- gowdis(diet_mat[traitsp,], asym.bin = 1:ncol(diet_mat))
# morphological
dist.mph <- gowdis(morph_mat[traitsp,])

## why are there NAs in these distance matrices?
## behaviour matrix is complete (no missing data)
## diet matrix only missing data for one species
## help file says NA generated if data missing from both of pair
## clearly not the case here!
## morphological data set has missing values, and no NAs in distance mat!
##### anyway, remove the influence of NAs on overall trait matrix
## by setting them to 0 (therefore not included in sum, equivalent to na.rm=T)

# sum(is.na(dist.beh)) # 6
dist.beh0 <- dist.beh
dist.beh0[is.na(dist.beh0)] <- 0
# sum(is.na(dist.diet)) # 80
dist.diet0 <- dist.diet
dist.diet0[is.na(dist.diet0)] <- 0
# sum(is.na(dist.mph)) # 0

# overall trait distance matrix
dist.trait <- (dist.beh0 + dist.diet0 + dist.mph) / 3
attr(dist.trait, 'Labels') <- colnames(dat.spsite)
# sum(is.na(dist.trait)) # 0 - good; need 0 for future functions


# how do these correlate?
# source('def.panelutils.R')
# pairs(x=data.frame(
#   beh = as.vector(dist.beh0),
#   die = as.vector(dist.diet0),
#   mph = as.vector(dist.mph),
#   mean = as.vector(dist.trait)
# ), upper.panel = panel.cor)
# individual mats not correlated r<.2; mean represents all three with r>.5 :)