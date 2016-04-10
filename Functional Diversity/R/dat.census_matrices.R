## set working directory
setwd('/home/josh/Dropbox/Thesis/Repo/thesis/Functional Diversity/R')

# load packages and functions needed and not sourced
require(magrittr)

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
  
 # %>% maca_whacker %>% remove_unwanted
  # only include species for which I have trait data
  trait_spp <- which(clean_x$sp %in% final.spp.list)
  obs_x <- clean_x[trait_spp,] %>% droplevels
}) 

# transpose matrices so I have species and sites
dat.trans <- llply(dat.no0, .fun=function(x) {
  # remove species column so only numeric vectors are transposed
  nosp <- x %>% extract(,2:ncol(x)) %>% t %>% data.frame %>% set_colnames(x$sp)
  nosp$site <- names(x[2:ncol(x)]) %>% factor
  return(nosp)
})


## all matrices should be consistent
## therefore, need to include species not seen on that census
spp.list <- final.spp.list
bodge <- matrix(NA, nrow=1, ncol=(length(spp.list)+1)) %>% data.frame
names(bodge) <- c(as.character(spp.list), 'site')

## join together by all common columns

##tthis part incomplete - unwanted species, typos etc getting through- how?

dat.addunobs <- llply(dat.trans, .fun=function(x) {
  badger <- (join(x, bodge, type='left'))
  badger <- badger[,sort(names(badger))]
  badger[is.na(badger)] <- 0
  
  return(badger)
})

# check all have same number of columns
sum(lapply(dat.addunobs, ncol) != ncol(dat.addunobs[[1]])) == 0
# no; 
# [[3]] and [[4]]has extra Himantpus mexicanus; [[5]] extra Larus dominicanus
# for some reason those columns are duplicated - WTF?
# bodge it for now by removing columns containing '.1

# join all these together into single data frame
# sites as rows, species as column
dat.spsite <- join_all(dat.addunobs, type='full')
dat.spsite <- dat.spsite[,grep('.1', names(dat.spsite),invert=TRUE)]