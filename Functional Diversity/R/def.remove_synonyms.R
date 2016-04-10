remove_synonyms <- function(x) {within(x, {
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
  sp[sp=="Casmerodius  albus"] <- "Ardea alba"
  sp[sp=="Catoptrophorus semipalmatus"] <- "Tringa semipalmatus"
  sp[sp=="Ceryle torquata"] <- "Megaceryle torquata"
  sp[sp=="Ceryle torquatus"] <- "Megaceryle torquata"
  sp[sp=="Zonibyx modestus"] <- "Charadrius modestus"
  sp[sp=="Cygnus melanocorypha"] <- "Cygnus melanocoryphus"
  sp[sp=="Cygnus melancoryphus"] <- "Cygnus melanocoryphus"
  sp[sp=="Fulica  armillata"] <- "Fulica armillata"
  sp[sp=="Haematopus paliatus"] <- "Haematopus palliatus"
  sp[sp=="Himantopus melanurus"] <- "Himantopus mexicanus"
  sp[sp=="Himantopus himantopus"] <- "Himantopus mexicanus"
  sp[sp=="Gallinago galinago"] <- "Gallinago paraguaiae"
  sp[sp=="Phoenicoparrus andinus"] <- "Phoenicopterus andinus"
  sp[sp=="Sterna eurygnatha"] <- "Sterna sandvicensis"
  sp[sp=="Sterna superciliaris"] <- "Sternula superciliaris"
  sp[sp=="Sterna maxima" ] <- "Thalasseus maximus"
  sp[sp=="Calidris melanotos  (Micropalama himantopus)"] <- 'Micropalama himantopus'
  
  # convert back to factor
  sp <- factor(sp)
})}