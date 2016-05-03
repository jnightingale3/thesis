# function to remove unwanted species (from wrong families)
# input x is a list of species

remove_unwanted <- function(x) {
    
  spp_to_delete <- c("Anthus correndera", "Anthus lutescens", # pipits
                     "Anumbius annumbi", # Firewood-gatherer
                     "Buteo magnirostris", # Roadside Hawk
                     "Caracara plancus", # a caracara
                     "Certhiaxis cinnamomea", # spinetail
                     "Circus buffoni", "Gavião do banhado", # Long-winged Harrier
                     "Circus buffoni ", # same species, with added typo
                     "Furnarius rufus", # Rufous Hornero
                     "Hemithraupis guira", # Guira Tanager
                     "Milvago chimachima", "Milvago chimango", # more caracaras
                     "Pitangus sulphuratus", # Bem-te-vi                 
                     "Rhea americana", # Rhea
                     "Rostrhamus sociabilis", # Snail Kite
                     "Tachycineta leucorrhoa", # White-rumped Swallow
                     "Xolmis irupero" # White Monjita
  )
  
  # return final list of species
  final.x <- subset(x, x %in% spp_to_delete == FALSE)
  
  return(final.x)
  
}