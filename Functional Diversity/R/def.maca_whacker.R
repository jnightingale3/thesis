maca_whacker <- function(x) {
  
  maclist <- c("Maçaricos", "maçariquinhos pequenos", 
    "macariquinhos pequenos", "macarico ni")
  
  whacked <- subset(x, sp %in% maclist == FALSE)
  
  return(whacked)
  
}