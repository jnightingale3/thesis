### function to draw random species pools

obs.dat <- dat.spsite
meta.ob <- extract_metadata(rownames(obs.dat))

obs.win <- obs.dat[meta.ob$season=='Winter',]
obs.sum <- obs.dat[meta.ob$season=='Summer',]

winter_pool <- data.frame(
  sp = colnames(obs.win),   # names of species
  wt = colSums(obs.win > 0) # number of occurrences
) 
winter_pool %<>% subset(wt > 0)

#winter_pool[sample.int(nrow(winter_pool), 5, replace=F, prob=winter_pool$wt), 1]

summer_pool <- data.frame(
  sp = colnames(obs.sum),   # names of species
  wt = colSums(obs.sum > 0) # number of occurrences
) 
summer_pool %<>% subset(wt > 0)

##########################
####### SIMULATION #######
##########################

#### number of repetitions
Nrep <- 20
iN <- 1

# empty matrices
winter_blank_mat <- obs.win 
winter_blank_mat[,] <- 0
summer_blank_mat <- obs.sum 
summer_blank_mat[,] <- 0
fake_community <- rbind(summer_blank_mat, winter_blank_mat)

#### create empty vessels for storing dbFD() data
fake.nbsp <- matrix(0, nrow=nrow(fake_community), ncol=Nrep, 
                    dimnames=list(rownames(fake_community)))
fake.FDiv <- matrix(0, nrow=nrow(fake_community), ncol=Nrep, 
                    dimnames=list(rownames(fake_community)))
fake.FDis <- matrix(0, nrow=nrow(fake_community), ncol=Nrep,
                    dimnames=list(rownames(fake_community)))
fake.FRic <- matrix(0, nrow=nrow(fake_community), ncol=Nrep,
                    dimnames=list(rownames(fake_community)))
fake.FEve <- matrix(0, nrow=nrow(fake_community), ncol=Nrep,
                    dimnames=list(rownames(fake_community)))

### go for it!
for (iN in 1:Nrep) {
  
  ####define seasonal species pools
  ### winter
  winter_blank_mat[,] <- 0 # reset
  iw <- 1
  for(iw in (1:nrow(obs.win))) {
    samp <- sample.int(n=nrow(winter_pool),
                       size=rowSums(obs.win > 0)[iw],
                       replace=F,
                       prob=winter_pool$wt)
    winter_blank_mat[iw,samp] <- 1
  }
  
  ### summer
  summer_blank_mat[,] <- 0 # reset
  is <- 1
  for(is in (1:nrow(obs.sum))) {
    samp <- sample.int(n=nrow(summer_pool),
                       size=rowSums(obs.sum > 0)[is],
                       replace=F,
                       prob=summer_pool$wt)
    summer_blank_mat[is,samp] <- 1
  }
  
  
  #### finalise fake data for dbFD()
  fake_community <- rbind(summer_blank_mat, winter_blank_mat)
  # order the same as normal matrices
  fake_community %<>% extract(match(rownames(obs.dat), rownames(fake_community)),)
  fake_trait_mat <- trait_mat
  
  
  ########### getting things ready for dbFD()
  # species data need to be identical & in the same order in both matrices
  fcommon_spp <- colnames(fake_community)
  fcommon_spp <- fcommon_spp[colSums(fake_community) > 0]
  fcommon_spp <- intersect(rownames(fake_trait_mat), fcommon_spp)
  
  fake.spsite <- fake_community[,colnames(fake_community) %in% fcommon_spp]
  # analysis requires at least 3 unique species to be observed!
  fake.spsite <- fake.spsite[(rowSums(fake.spsite > 0) > 2),]
  
  # store species names in row names - can't include as a column
  #row.names(dat.umig) <- dat.umig$sp
  
  # create trait matrix only including correct species
  fake_trait_fd <- fake_trait_mat[rownames(fake_trait_mat) %in% fcommon_spp,]
  # check
  all.equal(rownames(fake_trait_fd), colnames(fake.spsite))
  sum(rowSums(fake.spsite) > 2) == nrow(fake.spsite)
  ############
  
  #### run the analysis for functional metrics
  dbfd.fake <- dbFD(x=fake_trait_fd, a=fake.spsite, corr = 'cailliez', calc.CWM=F)
  
  #### store results of this iteration
  fake.nbsp[match(names(dbfd.fake$nbsp), rownames(fake.nbsp)), iN] <- dbfd.fake$nbsp
  fake.FDiv[match(names(dbfd.fake$FDiv), rownames(fake.FDiv)), iN] <- dbfd.fake$FDiv
  fake.FDis[match(names(dbfd.fake$FDis), rownames(fake.FDis)), iN] <- dbfd.fake$FDis
  fake.FRic[match(names(dbfd.fake$FRic), rownames(fake.FRic)), iN] <- dbfd.fake$FRic
  fake.FEve[match(names(dbfd.fake$FEve), rownames(fake.FEve)), iN] <- dbfd.fake$FEve
  
  if(iN %% 5 == 0) {print(paste('Rep ', iN, ' of ', Nrep, sep=''))}
}

### save these files to avoid re-running simulations!
# write.csv(fake.nbsp, file='dat.fake.nbsp.csv')
# write.csv(fake.FDiv, file='dat.fake.FDiv.csv')
# write.csv(fake.FDis, file='dat.fake.FDis.csv')
# write.csv(fake.FRic, file='dat.fake.FRic.csv')
# write.csv(fake.FEve, file='dat.fake.FEve.csv')