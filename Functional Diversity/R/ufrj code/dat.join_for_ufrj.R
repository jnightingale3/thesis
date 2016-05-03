# Joins with Wilman et al's (2014) EltonTraits 1.0
# Is not completely accurate, and too low resolution for my analysis
# But it means I can get on with something while I compile trait matrix

# load data for all birds of the world
wilman <- read.csv('data/traits/Wilman2014Birds.csv')
# load my data
source('dat.census_matrices.R')
#str(wilman) # check
# load package for pipe operator
require(magrittr)

# choose which columns I want
wilman_wanted <- wilman %>%
  subset(select=c('BLFamilyLatin', 'Scientific', 'English', 
                  (names(wilman)[grep('Diet', names(wilman))])[1:10],
                  (names(wilman)[grepl('ForStrat', names(wilman))])[1:7],
                  'PelagicSpecialist', 'Nocturnal', 'BodyMass.Value' ))
names(wilman_wanted)[2] <- 'sp' # for compatibility in join()

spdf <- data.frame(sp=factor(final.spp.list)) # spp list as dataframe for join

# join species names with Wilman data
# left join - therefore only keeps species from species list
dat.ufrj <- join(spdf, wilman_wanted, by='sp', type='left') %>% 
  na.omit %>% droplevels
# remove traits which don't occur in any species here
dat.ufrj <- dat.ufrj[,(c(1:3, colSums(dat.ufrj[,4:ncol(dat.ufrj)])) > 0)]

rm(bodge, spdf, dat.addunobs, dat.no0, dat.trans)
#rm(wilman, wilman_wanted)