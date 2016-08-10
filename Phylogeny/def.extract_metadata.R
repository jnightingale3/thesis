### function that extracts site metadata
### from a vector of census codes (site, date and season)

extract_metadata <- function(x) {
 metadata <-  data.frame(
    sitecode = (x),
    site = gsub('.[0-9]*[i,v]', '', (x)),
    years = paste('20', gsub('[^0-9]', '', (gsub("[0-9][.]", '', 
                                                 (x)))),
                  sep='') %>% as.numeric,
    habitat = id_habitats((x)),
    season = id_seasons((x))
  )
 return(metadata)
}