### This file takes the raw census data
### and converts to a species-site table
### with species as columns and sites as rows
### as required for functional diversity analysis.
### Only species selected in dat.spp_list.R are included.

## set working directory
# setwd('/home/josh/Dropbox/Thesis/Repo/thesis/Functional Diversity/R')

# load packages and functions needed and not sourced
# require(magrittr)

# read in raw census data (stored in dat.list)
# and generate a clean species list
source('dat.spp_list.R')


# remove censuses where nothing was counted, and species w/o trait data
dat.no0 <- llply(dat.list, .fun=function(x) {
  # which rows have at least one observation?
  non0_row <- x %>% extract(,-1) %>% as.matrix %>% 
    rowSums(na.rm=TRUE) %>% is_greater_than(0)
  # remove rows with 0 observations then clean species names
  clean_x <- x[non0_row,] %>% remove_synonyms  %>% droplevels
  
    # only include species on the final (cleaned-up) list
  trait_spp <- which(clean_x$sp %in% final.spp.list)
  obs_x <- clean_x[trait_spp,] %>% droplevels
  return(obs_x)
}) 

# transpose matrices so I have species and sites
dat.trans <- llply(dat.no0, .fun=function(x) {
  # remove species column so only numeric vectors are transposed
  nosp <- x %>% extract(,2:ncol(x)) %>% t %>% data.frame %>% set_colnames(x$sp)
  rownames(nosp) <- names(x[2:ncol(x)]) %>% factor
  return(nosp)
})


# ## get site names (which are row names)
# allsites <- (llply(dat.trans, rownames))
# sitenames<- c(allsites[[1]], allsites[[2]])
# ij <- 1
# allnames <- c(for(ij in 1:length(names(dat.trans))) {
#   (rownames(dat.trans[[ij]]))
#   })
# allnames <- rbind(for(ij in 1:length(names(dat.trans))) {
#   (rownames(dat.trans[[ij]]))
#   })

## all matrices should be consistent
## therefore, need to include species not seen on that census
spp.list <- final.spp.list
bodge <- matrix(NA, nrow=1, ncol=(length(spp.list))) %>% data.frame
names(bodge) <- c(as.character(spp.list))

## join together by all common columns

## this part incomplete - unwanted species, typos etc getting through- how?

dat.addunobs <- llply(dat.trans, .fun=function(x) {
  badger <- (join(x, bodge, type='left'))
  badger <- badger[,sort(names(badger))]
  badger[is.na(badger)] <- 0
  rownames(badger) <- rownames(x)
  badger$site <- rownames(x)
  return(badger)
})

# # check all have same number of columns
# sum(lapply(dat.addunobs, ncol) != ncol(dat.addunobs[[1]])) == 0
# # for some reason some columns are duplicated - WTF?
# # which columns are they?
# names(dat.spsite[,grep('.1', names(dat.spsite))])
# # check that they are duplications with no extra info
# equals( (dat.spsite[,grep('dominicanus', names(dat.spsite))])[1],
#         (dat.spsite[,grep('dominicanus', names(dat.spsite))])[2]) %>%
#   na.omit %>% equals(FALSE) %>% sum # great - so can just delete them!


# join all these together into single data frame
# sites as rows, species as column
dat.spsite <- join_all(dat.addunobs, type='full')
# bodge it for now by removing columns containing '.1'
dat.spsite <- dat.spsite[,grep('.1', names(dat.spsite),invert=TRUE)]
# analysis requires at least 3 unique species to be observed
# include only surveys where at least 5 species were observed
dat.spsite <- dat.spsite[(rowSums(dat.spsite != 0)>4),]

## Remove site column so data frame is all numeric
source('def.clean_snames.R') # function to standardise site labels
sitenames <- clean_snames(dat.spsite$site)
rownames(dat.spsite) <- sitenames # store in row names of dataframe
dat.spsite %<>% subset(select=-site)

## remove 'counts' where nothing (from spp.list) was counted
# which rows are zero
.emptyrow <- which(rowSums(dat.spsite, na.rm=T) == 0)
if(length(.emptyrow) > 0) {dat.spsite <- dat.spsite[-(.emptyrow),]}