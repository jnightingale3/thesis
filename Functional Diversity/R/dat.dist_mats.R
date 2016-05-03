### This file takes the species-site and trait data
### and converts into two distance matrices,
### one for census and one for species traits

### require packages 
# require(vegan) # vegdist() function for Bray-Curtis distance
# require(cluster) # daisy() for Gower distance on mixed numeric/class variables

### species data
## this one is straightforward, using Bray-Curtis distance (default)
## ln(x+1) transformation
dist.spp <- vegdist(log1p(dat.spsite)>0, method='jac')
# dist.spp <- vegdist(dat.spsite>0, method='raup')

### trait data
## to give equal weight to the three types of traits,
## morphological, dietary and behavioural,
## a separate distance matrix is calculated for each type of trait
## which is then averaged to give an overall distance matrix

# behavioural
dist.beh <- gowdis(behav_mat, asym.bin = 1:ncol(behav_mat))
# diet contents
dist.diet <- gowdis(diet_mat, asym.bin = 1:ncol(diet_mat))
# morphological
dist.mph <- gowdis(morph_mat)

# overall trait distance matrix
dist.trait <- (dist.beh + dist.diet + dist.mph) / 3

sum(is.na(dist.beh))
dist.beh[is.na(dist.beh)] <- 0
sum(is.na(dist.diet))
dist.diet[is.na(dist.diet)] <- 0
sum(is.na(dist.mph))

# # how do these correlate?
# pairs(x=data.frame(
#   beh = as.vector(dist.beh),
#   die = as.vector(dist.diet),
#   mph = as.vector(dist.mph),
#   mean = as.vector(dist.mean)
# )) # individual variables not correlated; mean represents all three :)