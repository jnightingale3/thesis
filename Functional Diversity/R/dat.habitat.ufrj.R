#rm(habitats) # clear previous version to avoid factor problems
habitats <- data.frame(
  site = row.names(spsite),
  habitat = 'NA', # default values
  season = 'winter') # conditionally corrected below



# habitats classified by pattern-matching site name
# this is very inexact! 
# TODO: classify habitats properly with google earth!
habitats <- within(habitats, {
  habitat <- as.character(habitat)
  habitat[grep('lagoa', site, ignore.case=T)] <- 'lagoa'
  habitat[grep('praia', site, ignore.case=T)] <- 'praia'
  habitat[grep('barra', site, ignore.case=T)] <- 'praia'
  habitat[grep('granja', site, ignore.case=T)] <- 'grass'
  habitat[grep('garanja', site, ignore.case=T)] <- 'grass' # typo
  habitat[grep('caieira', site, ignore.case=T)] <- 'lagoa'
  habitat[grep('fundo', site, ignore.case=T)] <- 'grass'
  #habitat[grep('alemoa', site, ignore.case=T)] <- 'grass'
  habitat[grep('santana', site, ignore.case=T)] <- 'lagoa'
  habitat[grep('banhado', site, ignore.case=T)] <- 'grass'
  habitat[grep('talha', site, ignore.case=T)] <- 'lagoa'
  habitat[grep('figueira', site, ignore.case=T)] <- 'lagoa'
  habitat[grep('rinc', site, ignore.case=T)] <- 'lagoa'
  habitat[grep('dunas', site, ignore.case=T)] <- 'grass'
  habitat[grep('ponto', site, ignore.case=T)] <- 'lagoa'
  habitat <- factor(habitat)
  
  # season also identified by pattern-matching
  # but filenames do contain season so it's ok!
  season <- as.character(season)
  season[grep('ver', site)] <- 'summer' 
  season <- factor(season)
})

# # check something plausible has come out
summary(habitats)

# write file 
#write.csv(habitats, file='data/traits/habitat.csv')