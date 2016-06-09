###############################################################
### compare traits between migrant and resident communities ###
###############################################################
res_spp <- (subset(master_trait, mig == 'res')# & order != 'Phoenicopteriformes')
)$species %>% droplevels

# only keep the same sites that had enough mig spp for CWM calculations:
mig_sites <- rownames(spsite_mig)
# species data need to be identical & in the same order in both matrices
# res_common_spp <-  intersect(rownames(trait_mat), colnames(dat.spsite))
spsite_res <- dat.spsite[rownames(dat.spsite) %in% mig_sites,
                         colnames(dat.spsite) %in% res_spp]
# check all sites have more than two species
# sum(rowSums(spsite_res>0) > 2) == nrow(spsite_res) # yes
# remove those sites with too few spp
# spsite_res_all <- spsite # save original for future reference
# spsite_res <- spsite_res[which((rowSums(spsite_res>0))>2),] # remove
# check all spp occur 
# sum(colSums(spsite_res>0)>0) == ncol(spsite_res) # no
# remove species that don't occur
spsite_res <- spsite_res[,(colSums(spsite_res)>0)]
res_spp <- colnames(spsite_res)
# trait_res  <- trait_res[(colSums(spsite_res)>0),]



# make table of sample sizes by category
ntabcom <- table(id_habitats(rownames(spsite_res)),
                 id_seasons(rownames(spsite_res))) %>%
  matrix(ncol=2) %>% data.frame
names(ntabcom) <- c('Summer', 'Winter')
ntabcom[3,] <- colSums(ntabcom)
ntabcom$Total <- rowSums(ntabcom)
rownames(ntabcom) <- c('Beach', 'Lake', 'Total')
ntabcom[is.na(ntabcom)] <- 0 # missing values are not present
# stargazer(ntabcom, summary=F, digits=0, label='ntabcom', title='Number of included surveys in each habitat and season category', font.size='small', rownames=T, table.placement='b')





# create trait matrix only including correct species
trait_res <- trait_mat[rownames(trait_mat) %in% res_spp,]
trait_res$bill.shape %<>% droplevels # remove unused factor levels
# check
# nrow(trait_res) == ncol(spsite_res) # same number of species
# all.equal(rownames(trait_res), colnames(spsite_res)) # nope
# reorder trait_fd rows to match spsite columns
trait_res <- trait_res[match(colnames(spsite_res),rownames(trait_res)),]
# all.equal(rownames(trait_fd), colnames(spsite)) # success!
# check all sites have more than two species
# sum(rowSums(spsite_res>0) > 2) == nrow(spsite_res) # yes

# get habitat & season data
datr.hab <- data.frame(id_habitats(rownames(spsite_res))); names(datr.hab) <- 'hab'
datr.sea <- data.frame(id_seasons(rownames(spsite_res))); names(datr.sea) <- 'sea'
datr.habsea <- data.frame(datr.hab, datr.sea)

# make distance matrices
distr.hab <- gowdis(datr.hab)
distr.sea <- gowdis(datr.sea)
distr.habsea <- gowdis(datr.habsea)

# calculate CWMs - uses mean/dominant traits, not distance
cwres <- functcomp(trait_res, as.matrix(spsite_res), CWM.type = 'dom',
                   bin.num=c(behav_vars, diet_vars))

cwresmig <- rbind(cwmig, cwres) # join them together for analysis
cwresmig$migrant <- c(rep('mig', nrow(cwmig)), rep('res', nrow(cwres))) %>% factor

# function to compare communities with Mann-Whitney test and extract p value
comcomp <- function(x) {
  return(wilcox.test(x ~ cwresmig$migrant, paired=TRUE)$p.value)
}
# apply to each column (i.e., each trait)
traitcommwp <- p.adjust(numcolwise(comcomp)(cwresmig), method='fdr') %>% round(3)

# calculate mean of each trait according to whether migratory or not
traitcomsum <- ddply(cwresmig, .(migrant), numcolwise(mean))[,-1]
traitcomsum[,-(1:3)] %<>% multiply_by(100) %>% round(1)
antilogcols <- which(colnames(traitcomsum) %in% c('mass', 'bill.length', 'tarsus'))
traitcomsum[,antilogcols] %<>% exp

# results table for stargazer
migrestab <- data.frame(
  Trait = gsub('[.]', ' ', colnames(traitcomsum)) %>% capitalize,
  Migrants = as.numeric(traitcomsum[1,]) %>% round,
  Residents = as.numeric(traitcomsum[2,]) %>% round,
  Difference = as.numeric(traitcomsum[1,] - traitcomsum[2,]) %>% round,
  'p value' = traitcommwp
)[order(traitcommwp),] %>% na.omit # sort by p-value and remove NAs
# migrestab # nice table

migres_sig <- migrestab$p.value < 0.05
