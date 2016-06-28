### The function defined below combines species called by
### various names in different spreadsheets by standardising
### terminology. It also fixes typos that occur in the raw data
### (including some entries made against the wrong species althogether,
### detected by comparison with field data sheets)

remove_synonyms <- function(x) {within(x, {
  # convert to character vector to enable changing factor levels
  sp <- as.character(sp)
  
  # combine synonmys - alphabetical by target species name
  sp[sp=="Casmerodius albus"] <- "Ardea alba"
  sp[sp=="Ardea ibis"] <- "Bubulcus ibis"
  sp[sp=="Butorides striatus"] <- "Butorides striata"
  sp[sp=="Zonibyx modestus"] <- "Charadrius modestus"
  sp[sp=="Larus maculipennis"] <- "Chroicocephalus maculipennis"
  sp[sp=="Cygnus melanocorypha"] <- "Cygnus melanocoryphus"
  sp[sp=="Gallinago galinago"] <- "Gallinago paraguaiae"
  sp[sp=="Porphyriops melanops"] <- "Gallinula melanops"
  sp[sp=="Himantopus melanurus"] <- "Himantopus mexicanus"
  sp[sp=="Himantopus himantopus"] <- "Himantopus mexicanus"
  sp[sp=="Ceryle torquata"] <- "Megaceryle torquata"
  sp[sp=="Ceryle torquatus"] <- "Megaceryle torquata"
  sp[sp=="Calidris melanotos  (Micropalama himantopus)"] <- 'Micropalama himantopus'
  sp[sp=="Phalacrocorax brasiliensis"] <- "Phalacrocorax brasilianus"
  sp[sp=="Sterna simplex"] <- "Phaetusa simplex"
  sp[sp=="Phoenicoparrus andinus"] <- "Phoenicopterus andinus"
  sp[sp=="Ajaja ajaja"] <- "Platalea ajaja"
  sp[sp=="Platalea (Ajaja) ajaja"] <- "Platalea ajaja"
  sp[sp=="Ajaia ajaja"] <- "Platalea ajaja"
  sp[sp=="Podicephorus major"] <- "Podiceps major"
  sp[sp=="Sterna eurygnatha"] <- "Sterna sandvicensis"
  sp[sp=="Sterna superciliaris"] <- "Sternula superciliaris"
  sp[sp=="Sterna maxima" ] <- "Thalasseus maximus"
  sp[sp=="Catoptrophorus semipalmatus"] <- "Tringa semipalmatus"
  
  
  # fix typos - sorted as above
  sp[sp=="Casmerodius  albus"] <- "Ardea alba"
  sp[sp=="Anhima cornuta"] <- "Chauna torquata"
  sp[sp=="Chroicocephalus  maculipennis "] <- "Chroicocephalus maculipennis"
  sp[sp=="Croicocephalus maculipennis"] <- "Chroicocephalus maculipennis"
  sp[sp=="Circus buffoni "] <- "Circus buffoni"
  sp[sp=="Cygnus melancoryphus"] <- "Cygnus melanocoryphus"
  sp[sp=="Fulica  armillata"] <- "Fulica armillata"
  sp[sp=="Fulica leucoptera "] <- "Fulica leucoptera"
  sp[sp=="Gallinago paraguaiae "] <- "Gallinago paraguaiae"
  sp[sp=="Gallinula melanops "] <- "Gallinula melanops"
  sp[sp=="Haematopus paliatus"] <- "Haematopus palliatus"
  sp[sp=="Jabiru mycteria"] <- "Mycteria americana"
  sp[sp=="Phimosus infuscatus "] <- "Phimosus infuscatus"
  sp[sp=="Rostrhamus sociabilis "] <- "Rostrhamus sociabilis"
  sp[sp=="Rhynchops niger"] <- "Rynchops niger"
  
  
  # convert back to factor
  sp <- factor(sp)
})}