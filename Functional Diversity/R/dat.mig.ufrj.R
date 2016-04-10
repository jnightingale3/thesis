# Adds info on migratory status
# based on published species accounts at PNLP
source('dat.join_for_ufrj.R') # Wilman trait data for my species

mig <- data.frame(sp = spp.list)
pnlp <- read.csv("data/traits/PNLP_NovaLista.csv")

mig.status <- join(mig, pnlp) %>% na.omit %>% droplevels
levels(mig.status$status) # categories from PNLP checklist

# the categories above are too detailed for my purposes; lump many
mig <- within(mig.status, {
  mig <- NA
  mig[grep('R', status)] <- 'resdt'
  mig[mig=='M'] <- 'residt'
  mig[grep('N', status)] <- 'north'
  mig[grep('S', status)] <- 'south'
  mig <- factor(mig)
}) %>% droplevels
levels(mig$mig) # much better!

# # save for later
# write.csv(mig.status, file='data/traits/dat.mig.ufrj.csv')

# join to create final species trait matrix for analysis
dat.umig <- join(dat.ufrj, subset(mig, select=c('sp', 'mig'))) %>% 
                   na.omit %>% droplevels
rm(mig, mig.status, pnlp) # clean workspace