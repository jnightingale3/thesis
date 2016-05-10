### uses dbFD function to calculate community weighted means

# create new data frame for this analysis
# only including species for which I have data

# species data need to be identical & in the same order in both matrices
common_spp <- intersect(attr(dist.trait, 'Labels'), colnames(dat.spsite))
spsite <- dat.spsite[,colnames(dat.spsite) %in% common_spp]

# which spp excluded?
# colnames(dat.spsite)[which(colnames(dat.spsite) %in% common_spp == FALSE)]
# colSums(dat.spsite[,which(colnames(dat.spsite) %in% common_spp == FALSE)]>0)
# none!

# store species names in row names - can't include as a column
#row.names(dat.umig) <- dat.umig$sp

# create trait matrix only including correct species
# used later for CWM calculations
trait_fd <- trait_mat[rownames(trait_mat) %in% common_spp,]
# check
# all.equal(rownames(trait_fd), colnames(spsite)) # nope
# sum(rownames(trait_fd) %in% colnames(spsite)) # all present, in wrong order
# reorder trait_fd rows to match spsite columns
trait_fd <- trait_fd[match(colnames(spsite),rownames(trait_fd)),]
# all.equal(rownames(trait_fd), colnames(spsite)) # success!
# check all sites have more than two species
# sum(rowSums(spsite>0) > 2) == nrow(spsite) # yes

## vector of trait weights for dbFD function
## to ensure equal weighting to behav, diet and morph traits
# tw <- rep(NA, ncol(trait_fd))
# tw[which(names(trait_fd) %in% behav_vars)] <- 1 / 3 / length(behav_vars)
# tw[which(names(trait_fd) %in% diet_vars )] <- 1 / 3 / length(diet_vars)
# tw[which(names(trait_fd) %in% morph_vars)] <- 1 / 3 / length(morph_vars)

### run full distance-based FD analysis using package FD
# using data frame of traits
# fd.test <- dbFD(x=trait_fd, a=spsite, w=tw, corr = 'cailliez', calc.FGR=F)
# using customised distance matrix
fd.test <- dbFD(x=dist.trait, a=spsite, corr='cailliez', calc.FGR = F)
# fd.test <- dbFD(x=trait_fd, a=spsite, w=tw,
# corr = 'cailliez', calc.FGR=F)
# summary(fd.test) # what have we got?

# llply(fd.test, .fun=print) # print all the output


## look at some CWMs

# extract this data, add habitat info and reshape for plotting
source('def.id_habitats.R') # functions to extract site details from names
# cwms <- fd.test$CWM
# cwms$site <- rownames(fd.test$CWM)
# cwms$habitat <- id_habitats(cwms$site)  
# cwms$season  <- id_seasons(cwms$site)
# cwms %<>% melt(id=c('site', 'habitat', 'season'))
# cwms$value <- as.numeric(cwms$value) # convert these values to numbers for plotting
# # introduces NAs where value was a factor; remove them for now
# cwms <- na.omit(cwms)