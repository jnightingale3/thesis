####################
### using SYNCSA ###
####################

######################################################
##### limit to Scolopacidae only for comparative #####
# limit to habitat phy.env[,1] or season phy.env[,2] #
######################################################
phy.envh <- subset(phy.env, select = 'habitatLake')
# phy.env <- phy.envh


phy.spsite2 <- subset(phy.spsite, select=c(Calidris_alba, Calidris_canutus, Calidris_fuscicollis, Tringa_flavipes, Tringa_melanoleuca))
# phy.spsite_old <- phy.spsite
phy.spsite <- phy.spsite2[rowSums(phy.spsite2) > 0,]
names(phy.env) <- 'winter'
# phy.env_old <- phy.env
phy.env2 <- subset(phy.envh, rownames(phy.envh) %in% rownames(phy.spsite) == T)
phy.env <- phy.env2
# dist.phylo_old <- dist.phylo
prunetree <- prune.sample(phy.spsite, consedge)
prinedist <- cophenetic.phylo(prunetree)
dist.phylo <- prinedist
# phy.trait_old <- phy.trait
traitnew <- phy.trait[rownames(phy.trait) %in% c('Calidris_alba', 'Calidris_canutus', 'Calidris_fuscicollis', 'Tringa_flavipes', 'Tringa_melanoleuca'),]
phy.trait <- traitnew
# phy.trait[,1:3] %<>% decostand(method='range', MARGIN = 2)


######################################################
################ put it back to normal ###############
######################################################
phy.spsite <- phy.spsite_old
phy.trait <- phy.trait_old
dist.phylo <- dist.phylo_old
phy.env <- phy.env_old

######################################################
# check everything is correct
nrow(phy.env); nrow(phy.trait); nrow(phy.spsite); nrow(dist.phylo)

## all traits
org <- organize.syncsa(phy.spsite, phy.trait, dist.phylo, phy.env)
syncsa.all <- syncsa(org$community, org$traits, org$dist.spp, org$environmental,
                     dist='euc', na.rm=T)

#

## trait subsets
# make sub-dataframes
phy.beh <- subset(phy.trait, select=behav_vars)
phy.die <- subset(phy.trait, select=diet_vars)
phy.mor <- subset(phy.trait, select=c(mass, tarsus, grep('bill', names(phy.trait))))

# do analyses
# behaviour
org <- organize.syncsa(phy.spsite, phy.beh, dist.phylo, phy.env)
syncsa.beh <- syncsa(org$community, org$traits, org$dist.spp, org$environmental,
                     dist='euc', na.rm=T)
# diet
org <- organize.syncsa(phy.spsite, phy.die, dist.phylo, phy.env)
syncsa.die <- syncsa(org$community, org$traits, org$dist.spp, org$environmental,
                     dist='euc', na.rm=T)
# morphology
org <- organize.syncsa(phy.spsite, phy.mor, dist.phylo, phy.env)
syncsa.mor <- syncsa(org$community, org$traits, org$dist.spp, org$environmental,
                     dist='euc', na.rm=T)


## overall
syncsa.all$Statistics # TE*, PE*, PT**, BF***
syncsa.beh$Statistics # TE*, PE*, PT* , BF***
syncsa.die$Statistics # TE*, PE*, PT***,BF***
syncsa.mor$Statistics # TE-, PE*, PT-,  BF**

get_mat.cor <- function(x) {return(x$Statistics[c(1,3,4,7,9),1])}

sync_res <- data.frame(matrix(1:6, nrow=1))
sync_res[1,] <- c('All', get_mat.cor(syncsa.all))
sync_res[2,] <- c('Behavior', get_mat.cor(syncsa.beh))
sync_res[3,] <- c('Diet', get_mat.cor(syncsa.die))
sync_res[4,] <- c('Morphology', get_mat.cor(syncsa.mor))
names(sync_res) <- c('Traits', 'Trait.Environment', 'Phylogeny.Environment',
                     'PhySignal.Metacommunity', 'Conservatism', 'PhySignal.SppPool')
sync_res[,2:5] %<>% round(2)
sync_res

## Once more with peeling
get_mat.p <- function(x) {return(x$Statistics[c(1,3,4,7,9),2])}

sync_resp <- data.frame(matrix(1:6, nrow=1))
sync_resp[1,] <- c('All', get_mat.p(syncsa.all))
sync_resp[2,] <- c('Behavior', get_mat.p(syncsa.beh))
sync_resp[3,] <- c('Diet', get_mat.p(syncsa.die))
sync_resp[4,] <- c('Morphology', get_mat.p(syncsa.mor))
names(sync_resp) <- c('Traits', 'Trait.Environment', 'Phylogeny.Environment',
                     'PhySignal.Metacommunity', 'Conservatism', 'PhySignal.SppPool')
sync_resp[,2:4] %<>% round(3)
sync_resp

