############################################################################
####### script to test trait composition by habitat, season and both #######
######################## for migratory species only ########################
############################################################################


## how many species and individuals are migratory?
totalspp <- ncol(spsite) # total number of species
# vector of migratory status
migorres <- master_trait$mig[match(colnames(spsite), master_trait$species)]
totalmig <- sum(migorres != 'res') # how many spp migrate?
totalres <- sum(migorres == 'res') # how many spp resident?
totalaus <- sum(migorres == 'aus') # how many austral migrants?
totalbor <- sum(migorres == 'bor') # how many boreal resident?

# how many individuals recorded in total?
indivtot <- colSums(spsite) %>% sum
indivmig <- colSums(spsite[,which(migorres != 'res')]) %>% sum # how many migrate?
indivres <- colSums(spsite[,which(migorres == 'res')]) %>% sum # how many don't?


# create new data frame for this analysis
# only keep migratory species, and exclude the flamingoes
# because not strictly *seasonal* migrants (present all year, status unclear)
mig_spp <- (subset(master_trait, mig != 'res')# & order != 'Phoenicopteriformes')
            )$species %>% droplevels
# species data need to be identical & in the same order in both matrices
# mig_common_spp <-  intersect(rownames(trait_mat), colnames(dat.spsite))
spsite_mig <- dat.spsite[,colnames(dat.spsite) %in% mig_spp]

# create trait matrix only including correct species
trait_mig <- trait_mat[rownames(trait_mat) %in% mig_spp,]
# check
# all.equal(rownames(trait_mig), colnames(spsite_mig)) # nope
# sum(rownames(trait_mig) %in% colnames(spsite_mig)) # all present, in wrong order
# reorder trait_fd rows to match spsite columns
trait_mig <- trait_mig[match(colnames(spsite_mig),rownames(trait_mig)),]
# all.equal(rownames(trait_fd), colnames(spsite)) # success!
# check all sites have more than two species
# sum(rowSums(spsite_mig>0) > 2) == nrow(spsite_mig) # nope
# remove those sites with too few spp
spsite_mig_all <- spsite # save original for future reference
spsite_mig <- spsite_mig[which((rowSums(spsite_mig>0))>2),] # remove
# check all spp occur 
# sum(colSums(spsite_mig>0)>0) == ncol(spsite_mig) # yes


# get habitat & season data
datg.hab <- data.frame(id_habitats(rownames(spsite_mig))); names(datg.hab) <- 'hab'
datg.sea <- data.frame(id_seasons(rownames(spsite_mig))); names(datg.sea) <- 'sea'
datg.habsea <- data.frame(datg.hab, datg.sea)

# make distance matrices
distg.hab <- gowdis(datg.hab)
distg.sea <- gowdis(datg.sea)
distg.habsea <- gowdis(datg.habsea)




# make table of sample sizes by category
# this is the same as the N-table for migrant vs resident comparison
# so don't need to run the code
# ntab_mig <- table(id_habitats(rownames(spsite_mig)),
#                   id_seasons(rownames(spsite_mig))) %>%
#   matrix(ncol=2) %>% data.frame
# names(ntab_mig) <- c('Summer', 'Winter')
# ntab_mig[4,] <- colSums(ntab_mig)
# ntab_mig$Total <- rowSums(ntab_mig)
# rownames(ntab_mig) <- c('Beach', 'Grass', 'Lake', 'Total')
# ntab_mig[is.na(ntab_mig)] <- 0 # missing values are not present
# stargazer(ntab_mig, summary=F, digits=0, label='ntab_mig', title='Number of included surveys in each habitat and season category', font.size='small', rownames=T, table.placement='b')





###################################
## functional composition (CWMs) ##
###################################

# calculate CWMs - uses mean/dominant traits, not distance
trait_mig$bill.shape %<>% droplevels # remove unused factor levels
cwmig <- functcomp(trait_mig, as.matrix(spsite_mig), CWM.type = 'dom',
                 bin.num=c(behav_vars, diet_vars))

# separate cwmig matrices
cwmig.beh <- subset(cwmig, select=behav_vars) 
cwmig.diet<- subset(cwmig, select=diet_vars)
cwmig.mph <- subset(cwmig, select=morph_vars)

# how do they correlate?
# behav & dist a lot; mean with everything
# morph is doing something else
# pairs(data.frame(as.vector(cwmig.behdist), as.vector(cwmig.diedist),
#                  as.vector(cwmig.mordist), as.vector(cwmig.dist)), 
#       upper.panel = panel.cor)

# make them numeric
# cwmig.beh %<>% colwise(as.numeric)(cwmig.beh) %>% subtract(1)
# cwmig.diet%<>% colwise(as.numeric)(cwmig.diet)%>% subtract(1)

# sepatate dist matrices
cwmig.behdist <- gowdis(cwmig.beh)#, asym.bin = 1:ncol(cwmig.beh))
cwmig.diedist <- gowdis(cwmig.diet)#, asym.bin = 1:ncol(cwmig.diet))
cwmig.mordist <- gowdis(cwmig.mph)
# and overall
cwmig.dist <- (cwmig.behdist + cwmig.diedist + cwmig.mordist) / 3


##################
## test overall ##
##################
lmg.fc.h  <-lm(cwmig.dist ~ distg.hab); summary(lmg.fc.h) # sig
lmg.fc.s  <-lm(cwmig.dist ~ distg.sea); summary(lmg.fc.s) # v sig
lmg.fc.hs <-lm(cwmig.dist ~ distg.habsea); summary(lmg.fc.hs) # vv sig

## test sub matrices

## test beh ##
lmg.cb.h  <- lm(cwmig.behdist ~ distg.hab); summary(lmg.cb.h) # not sig
lmg.cb.s  <- lm(cwmig.behdist ~ distg.sea); summary(lmg.cb.s) # v sig
lmg.cb.hs <- lm(cwmig.behdist ~ distg.habsea); summary(lmg.cb.hs )# v sig

## test diet ##
lmg.cd.h  <- lm(cwmig.diedist ~ distg.hab); summary(lmg.cd.h) # not sig
lmg.cd.s  <- lm(cwmig.diedist ~ distg.sea); summary(lmg.cd.s) # v sig
lmg.cd.hs <- lm(cwmig.diedist ~ distg.habsea); summary(lmg.cd.hs) # v sig

## test morph ##
lmg.cm.h  <- lm(cwmig.mordist ~ distg.hab); summary(lmg.cm.h) # v sig
lmg.cm.s  <- lm(cwmig.mordist ~ distg.sea); summary(lmg.cm.s) # not sig
lmg.cm.hs <- lm(cwmig.mordist ~ distg.habsea); summary(lmg.cm.hs) # sig