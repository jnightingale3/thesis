# script to compare CWMs between summer and winter surveys

cwmig$season <- id_seasons(rownames(cwmig))

# function to compare seasons with Mann-Whitney test
seacomp <- function(x) {
  return(wilcox.test(x ~ cwmig$season)$p.value)
}
# apply to each column (i.e., each trait)
seacommwp <- p.adjust(numcolwise(seacomp)(cwmig), method='fdr') %>% round(3)
seacommwp
