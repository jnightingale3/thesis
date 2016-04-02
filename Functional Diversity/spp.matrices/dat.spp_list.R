## set working directory
setwd('/home/josh/Dropbox/Thesis/Repo/thesis/Functional Diversity/spp.matrices')

## load required packages
require(plyr) # for join_all() function to join list of dataframes

## read in each year's census data into a list
dat.list <- list(
  'd2005inv' = read.csv("2005inv.csv"),
  'd2006inv' = read.csv("2006inv.csv"),
  'd2006ver' = read.csv("2006ver.csv"),
  'd2007ver' = read.csv("2007ver.csv"),
  'd2008inv' = read.csv("2008inv.csv"),
  'd2009ver' = read.csv("2009ver.csv"),
  'd2009ve2' = read.csv("2009ver2.csv"), # an additional count
  'd2012inv' = read.csv("2012inv.csv"),
  'd2012ver' = read.csv("2012ver.csv"),
  'd2013ver' = read.csv("2013ver.csv"),
  'd2014inv' = read.csv("2014inv.csv"),
  'd2015ver' = read.csv("2015ver.csv")
)

## join all years into a master species checklist
# which will subsequently need extensive cleaning
master <- join_all(dat.list, by='sp', type='full') 

## check all totals are numeric
str(master)

## remove all 'species' which were never observed
## (should remove most of the guff)

# create separate dataframe of counts
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
maca_whacker <- c("Maçaricos", "maçariquinhos pequenos", 
                  "macariquinhos pequenos", "macarico ni")
noblankIDmac <- subset(noblankID, sp %in% maca_whacker == FALSE)

# list of species that were observed and identified
# use droplevels to remove unused factor levels
spp.obs <- droplevels(subset(noblankIDmac, total > 0))

## show list of species
# may still contain synonyms, typos etc
#sort(levels(spp.obs$sp))

## lump synonmys and fix typos
# names follow van Perlo (2009), Birds of Brazil
nosyn <- droplevels(within(spp.obs, {
  # convert to character vector to enable changing factor levels
  sp <- as.character(sp)
  
  # correct inconsistencies
  sp[sp=="Ajaja ajaja"] <- "Platalea ajaja"
  sp[sp=="Platalea (Ajaja) ajaja"] <- "Platalea ajaja"
  sp[sp=="Ajaia ajaja"] <- "Platalea ajaja"
  sp[sp=="Sterna simplex"] <- "Phaetusa simplex"
  sp[sp=="Larus maculipennis"] <- "Chroicocephalus maculipennis"
  sp[sp=="Phalacrocorax brasiliensis"] <- "Phalacrocorax brasilianus"
  sp[sp=="Ardea ibis"] <- "Bubulcus ibis"
  sp[sp=="Butorides striatus"] <- "Butorides striata"
  sp[sp=="Casmerodius  albus"] <- "Casmerodius albus"
  sp[sp=="Catoptrophorus semipalmatus"] <- "Tringa semipalmatus"
  sp[sp=="Ceryle torquata"] <- "Megaceryle torquata"
  sp[sp=="Ceryle torquatus"] <- "Megaceryle torquata"
  sp[sp=="Zonibyx modestus"] <- "Charadrius modestus"
  sp[sp=="Cygnus melanocorypha"] <- "Cygnus melanocoryphus"
  sp[sp=="Cygnus melancoryphus"] <- "Cygnus melanocoryphus"
  sp[sp=="Fulica  armillata"] <- "Fulica armillata"
  sp[sp=="Haematopus paliatus"] <- "Haematopus palliatus"
  sp[sp=="Himantopus melanurus"] <- "Himantopus mexicanus"
  sp[sp=="Gallinago galinago"] <- "Gallinago paraguaiae"
  sp[sp=="Phoenicoparrus andinus"] <- "Phoenicopterus andinus"
  sp[sp=="Sterna eurygnatha"] <- "Sterna sandvicensis"
  sp[sp=="Sterna superciliaris"] <- "Sternula superciliaris"
  sp[sp=="Sterna maxima" ] <- "Thalasseus maximus"
  
  # convert back to factor
  sp <- factor(sp)
  }))

# make a list of species
spp.list <- sort(levels(nosyn$sp))
spp.list

# remove unwanted species (from wrong families)
spp_to_delete <- c("Anthus correndera", "Anthus lutescens", # pipits
                   "Anumbius annumbi", # Firewood-gatherer
                   "Certhiaxis cinnamomea", # spinetail
                   "Circus buffoni", "Gavião do banhado", # Long-winged Harrier
                   "Furnarius rufus", # Rufous Hornero
                   "Hemithraupis guira", # Guira Tanager
                   "Milvago chimango", "Milvago chimachima",  "Caracara plancus", # caracaras
                   "Pitangus sulphuratus", # Bem-te-vi                 
                   "Rhea americana", # Rhea
                   "Rostrhamus sociabilis", # Snail Kite
                   "Tachycineta leucorrhoa", # White-rumped Swallow
                   "Xolmis irupero" # White Monjita
                   )

# show (hopefully final) list of species
final.spp.list <- subset(spp.list, spp.list %in% spp_to_delete == FALSE)
final.spp.list
write.csv(final.spp.list, file='final.spp.list.csv', row.names=F)