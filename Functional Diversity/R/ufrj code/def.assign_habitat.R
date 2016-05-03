assign_habitat <- function(data, site) {
  within(data, {
    site <- factor(site)
    
    habitat <- NA
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
    season <- NA
    season[grep('ver', site)] <- 'summer' 
    season[grep('ve2', site)] <- 'summer' 
    season[grep('inv', site)] <- 'winter' 
    season <- factor(season)
  })}
  
try <- fd.test$CWM
try_out <- assign_habitat(try, rownames(try))
