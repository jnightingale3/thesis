### This file reads in the trait matrix
### and divides into three, based on the type of trait
### (morphological, diet, behavioural)

## load data
# set working directory
# setwd('~/Dropbox/Thesis/Repo/thesis/Functional Diversity/R')

## read main spreadsheet
master_trait <- read.csv("data/traits/trait_matrix.csv")

## make list of ID variables to be aligned with all three matrices 
id_vars <- c('order', 'species', 'mig')
# .idno <- which(names(master_trait) %in% id_vars) # which columns will they be?

## list all variables in each category
behav_vars <- c('scavenging', "pecking", "probing", "jabbing","scything", 
                "foot.trembling", "turning", "hammering", "swimming", "diving",
                "aerial", "kelptoparasitism", "dipping", "skimming")
diet_vars  <- c("insects", "crabs", "crustaceans", "worms", "snails" ,"molluscs",
                "herps", "fish", "plant", "other", "eggs", "diatoms")
morph_vars <- c('mass', 'bill.length', 'tarsus', 'bill.shape')



### split into three

## behavioural traits
behav_mat <- subset(master_trait, select=c(behav_vars))
behav_mat[is.na(behav_mat)] <- 0  # if not recorded, assume does not occur
#behav_mat %<>% equals(1) %>% data.frame # convert to logical for Gower distance

## diet contents traits
diet_mat <- subset(master_trait, select=c(diet_vars))
# convert data to Boolean
diet_mat[is.na(diet_mat)] <- 0 # if not recorded, assume does not occur in diet
#diet_mat %<>% equals(1) %>% data.frame # logical for Gower

## morphological traits
morph_mat <- subset(master_trait, select=c(morph_vars))
# log-transform the numerical columns -> normality
morph_mat[,( as.logical(colwise(is.numeric)(morph_mat)) )] %<>% log


## define overall trait matrix with correct NA handling, 
## based on above (and therefore without id vars)
trait_mat <- cbind(morph_mat, diet_mat, behav_mat)
rownames(trait_mat) <- master_trait$species
