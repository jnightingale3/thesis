## analyse functional ecology metrics (from Legendre paper)

obsdf <- data.frame(
  Dispersion = fd.test$FDis,
  Divergence = fd.test$FDiv,
  Evenness = fd.test$FEve,
  No.Species = fd.test$sing.sp,
  Richness = fd.test$FRic,
  Habitat = id_habitats(names(fd.test$FDis)),
  Season = id_seasons(names(fd.test$FDis)),
  Site= names(fd.test$FDis))

oblong <- melt(obsdf, id.vars = c('Site', 'Habitat', 'Season'))
names(oblong)[4:5] <- c('Metric', 'Value')
