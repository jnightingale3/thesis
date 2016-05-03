## exploratory data analysis of species data

# relation between presence and abundance:
plot( (colSums(dat.spsite)), colSums(dat.spsite > 0) ) # good scatter
# average occurrence
sort( colSums(dat.spsite) / colSums(dat.spsite > 0))


### choose some species to look at

## abundance
sort(colSums(dat.spsite))
# habitat restricted species in low numbers:
which(names(dat.spsite) == 'Syrigma sibilatrix') # 44
# common species that occurs in several habitats & large numbers
which(names(dat.spsite) == 'Plegadis chihi') # 35

## pres/abs
sort(colSums(dat.spsite > 0))
# species in few sites but high numbers:
which(names(dat.spsite) == 'Sterna hirundinacea') # 40
# widespread species that occurs in low numbers
which(names(dat.spsite) == 'Ardea cocoi') # 7


# vector of these species
chosen_ones <- c(44, 35, 40, 7)

# explore transformations
# require(vegan) # for decostand()
opar <- par() # store par() settings
par(mfrow=c(2,2)) # 4 plots
# function to do four histograms
fhist <- function(x, which) {
  ii <- 1
  for(ii in 1:length(which)) {
    hist(x[, (which[ii]) ], xlab='Abundance', main=names(x)[which[ii]] )
  }
}

# raw data
fhist(dat.spsite, chosen_ones)

# ln(x+1) transformed
fhist(log1p(dat.spsite + 1) ) , chosen_ones) # still very left skewed
# alternative (Anderson) log# chord
fhist(decostand(dat.spsite, method='normalize'), chosen_ones) # v v skewed-tranformation
fhist(decostand(dat.spsite, 
                method='log', logbase=10), chosen_ones) # bit better?

### standardise by site

# chord
fhist(decostand(dat.spsite, method='normalize'), chosen_ones) # v v skewed

# Hellinger
fhist(decostand(dat.spsite, method='hell'), chosen_ones) # better ish

### double standardisation
# chi squared
fhist(decostand(dat.spsite, method='chi'), chosen_ones) # v v skewed
# Wisconsin
fhist(wisconsin(dat.spsite), chosen_ones) # v v skewed

### species only
fhist(decostand(dat.spsite, method='range'), chosen_ones) # v v skewed
fhist(decostand(dat.spsite, method='freq'), chosen_ones) # v v skewed

par(opar)
### Therefore ##########################################################
# Anderson with base 10 seems best? in terms of approximate normality ##
########################################################################

# have a butchers without the 0s:
# amend fhist function:

# function to do four histograms
fhist2 <- function(x, which) {
  ii <- 1
  for(ii in 1:length(which)) {
    hist( (x[, (which[ii]) ])[which(x[, (which[ii]) ] != 0)], 
          xlab='Abundance', main=names(x)[which[ii]] )
  }
}

# Hellinger
fhist2(decostand(dat.spsite, method='hell'), chosen_ones) # ok but left skewed
# alternative (Anderson) log-tranformation
fhist2(decostand(dat.spsite, 
                method='log', logbase=10), chosen_ones) # bit better? or worse?
# chord
fhist2(decostand(dat.spsite, method='normalize'), chosen_ones) # fairly uniform

# by species
fhist2(decostand(dat.spsite, method='range'), chosen_ones) # v v skewed
fhist2(decostand(dat.spsite, method='freq'), chosen_ones) # v v skewed

### double standardisation
# chi squared
fhist2(decostand(dat.spsite, method='chi'), chosen_ones) # v skewed
# Wisconsin
fhist2(wisconsin(log1p(dat.spsite)), chosen_ones) # v v skewed


### try some different distance metrics

## hellinger
dist.spp <- dist(decostand(dat.spsite, method='hell'))
## chord
dist.spp <- dist(decostand(dat.spsite, method='norm'))

dist.spp <- dist(decostand(dat.spsite, method='chi'))
