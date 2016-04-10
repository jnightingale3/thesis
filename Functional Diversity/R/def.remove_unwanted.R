# function to remove unwanted species (from wrong families)
# input x is a list of species

remove_unwanted <- function(x) {
    
  spp_to_delete <- c("Anthus correndera", "Anthus lutescens", # pipits
                     "Anumbius annumbi", # Firewood-gatherer
                     "Certhiaxis cinnamomea", # spinetail
                     "Circus buffoni", "Gavião do banhado", # Long-winged Harrier
                     "Furnarius rufus", # Rufous Hornero
                     "Hemithraupis guira", # Guira Tanager
                     "Milvago chimango", "Milvago chimachima",  "Caracara plancus", 
                     # caracaras
                     "Pitangus sulphuratus", # Bem-te-vi                 
                     "Rhea americana", # Rhea
                     "Rostrhamus sociabilis", # Snail Kite
                     "Tachycineta leucorrhoa", # White-rumped Swallow
                     "Xolmis irupero" # White Monjita
  )
  
  # show (hopefully final) list of species
  final.x <- subset(x, x %in% spp_to_delete == FALSE)
  
  return(final.x)
  
}