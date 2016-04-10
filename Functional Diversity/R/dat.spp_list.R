## set working directory
setwd('/home/josh/Dropbox/Thesis/Repo/thesis/Functional Diversity/R')

## load required packages
require(plyr) # for join_all() function to join list of dataframes

## read in each year's census data into a list
dat.list <- list(
  'd2005inv' = read.csv("data/spp.matrices/2005inv.csv"),
  'd2006inv' = read.csv("data/spp.matrices/2006inv.csv"),
  'd2006ver' = read.csv("data/spp.matrices/2006ver.csv"),
  'd2007inv' = read.csv("data/spp.matrices/2007inv.csv"),
  'd2007ver' = read.csv("data/spp.matrices/2007ver.csv"),
  'd2008inv' = read.csv("data/spp.matrices/2008inv.csv"),
  'd2009ver' = read.csv("data/spp.matrices/2009ver.csv"),
  'd2009ve2' = read.csv("data/spp.matrices/2009ver2.csv"), # an additional count
  'd2012inv' = read.csv("data/spp.matrices/2012inv.csv"),
  'd2012ver' = read.csv("data/spp.matrices/2012ver.csv"),
  'd2013ver' = read.csv("data/spp.matrices/2013ver.csv"),
  'd2014inv' = read.csv("data/spp.matrices/2014inv.csv"),
  'd2015ver' = read.csv("data/spp.matrices/2015ver.csv")
)

## rename columns for join
il <- 1 # initialise list index
for (ii in (1:length(names(dat.list))) ) {
  names(dat.list[[ii]])[-1] <- paste( names(dat.list[[ii]])[-1], 
                                      names(dat.list)[ii], sep='-')
}

## join all years into a master species checklist
# which will subsequently need extensive cleaning
master <- join_all(dat.list, by='sp', type='full') 

## check all totals are numeric - they aren't
#str(master)

## remove all 'species' which were never observed
## (should remove most of the guff)

# create separate dataframe of counts (they are removed below)
counts <- master[,-1]
master$total <- rowSums(counts, na.rm=T)

## remove blank lines
noblank <- subset(master, sp != '')
## remove anything unidentified
# luckily no species' binomials contain 'sp'
# grep function matches a pattern; 
# invert=TRUE returns elements that *don't* match it
noblankID <- noblank[grep('sp', noblank$sp, invert=TRUE),]

# also remove variations on 'maçaricos' that appear
# (indicating unidentified waders)
source('def.maca_whacker.R')
noblankIDmac <- droplevels(maca_whacker(noblankID))

# list of species that were observed and identified
# use droplevels to remove unused factor levels
spp.obs <- droplevels(subset(noblankIDmac, total > 0))

## show list of species
# may still contain synonyms, typos etc
#sort(levels(spp.obs$sp))

## lump synonmys and fix typos - function defined elsewhere
source('def.remove_synonyms.R')
# names follow van Perlo (2009), Birds of Brazil
nosyn <- droplevels(remove_synonyms(spp.obs))

# make a list of species
spp.list <- sort(levels(nosyn$sp))
#spp.list

# remove unwanted species - file defined elsewhere
source('def.remove_unwanted.R')
final.spp.list <- remove_unwanted(spp.list)
# save file
#write.csv(final.spp.list, file='data/spp.matrices/final.spp.list.csv', row.names=F)

# remove un-needed objects
# for when this file is sourced
unneeded <-  c("counts","ii","il","master","noblank","noblankID",
               "noblankIDmac","nosyn","spp.list","spp.obs")
rm(list=unneeded); rm(unneeded)

########################
## find weird records ##
########################

# # which list is it in?
# lapply(dat.list, function(x) {"Phoenicopterus ruber" %in% levels(x[,1])})
# 
# # how many records and where?
# lapply(dat.list, function(x) {(x[(x[,1] == 'Anhima cornuta'),-1])})
# lapply(dat.list, function(x) {(x[(x[,1] == "Phoenicopterus ruber"),-1])})
# lapply(dat.list, function(x) {(x[(x[,1] == 'Jabiru mycteria'),-1])})
