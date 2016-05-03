### functions to identify habitats and seasons
### based on standardised site names (from clean_snames)

id_habitats <- function(x) { # x is a character vector of sitenames
  h <- rep(NA, length(x)) # initialise
  h[grepl('PAPAG', x)] <- 'Lake'
  h[grepl('PANTA', x)] <- 'Grass'
  h[grepl('CAIEA', x)] <- 'Lake'
  h[grepl('FUNDO', x)] <- 'Grass'
  h[grepl('RINCO', x)] <- 'Lake'
  h[grepl('ALEMO', x)] <- 'Grass'
  h[grepl('LMEIO', x)] <- 'Lake'
  h[grepl('DOURO', x)] <- 'Lake'
  h[grepl('PAULO', x)] <- 'Lake'
  h[grepl('Pont', x)] <- 'Lake'
  h[grepl('TALHA', x)] <- 'Lake'
  h[grepl('BABAL', x)] <- 'Grass'
  h[grepl('PPRAI', x)] <- 'Beach'
  h[grepl('PRABA', x)] <- 'Beach'
  h[grepl('CANAL', x)] <- 'Lake'
  h[grepl('JDIAS', x)] <- 'Lake'
  h[grepl('PAURA', x)] <- 'Lake'
  h[grepl('BNITO', x)] <- 'Lake'
  h[grepl('RONDA', x)] <- 'Grass'
  h[grepl('SAIDA', x)] <- 'Lake'
  h[grepl('SIMAO', x)] <- 'Lake'
  h[grepl('FIGUA', x)] <- 'Lake'
  h[grepl('VEANA', x)] <- 'Lake'
  h[grepl('PJOAO', x)] <- 'Lake'
  h[grepl('BARRA', x)] <- 'Beach'
  h[grepl('DUNAS', x)] <- 'Grass'
  
  h <- factor(h)
  
  return(h)
}

# h <- id_habitats(sitenames); h # show
# table(h) # summary so far
# sitenames[is.na(h)] # what's left?
# rm(h) # clean up

id_seasons <- function(x) { # x is a character vector of sitenames
  s <- rep(NA, length(x))
  s[grepl('v', x)] <- 'Summer'
  s[grepl('i', x)] <- 'Winter'
  s <- factor(s)
  return(s)
}

# table(id_seasons(sitenames))
