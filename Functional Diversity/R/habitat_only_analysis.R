## PCOA of habitat

require((ade4))

# log transformed data
habpcoa <- dudi.pco(vegdist(log1p(spsite), method='gower'))
biplot(habpcoa)

habjoin <- data.frame(cbind(habpcoa$li, habitats))


opar <- par(); par(mfrow=c(2,2)); with(habjoin, plot(season, A1))
with(habjoin, plot(season, A2))
with(habjoin, plot(habitat, A1))
with(habjoin, plot(habitat, A2)); par(opar)

require(lattice)
bwplot(A1 ~ season | habitat, data=habjoin)
bwplot(A1 ~ habitat | season, data=habjoin)

# pres/abs data
habpcoa <- dudi.pco(vegdist(spsite > 0, method='raup', binary = TRUE))
biplot(habpcoa)

habjoin <- data.frame(cbind(habpcoa$li, habitats))


opar <- par(); par(mfrow=c(2,2)); with(habjoin, plot(season, A1))
with(habjoin, plot(season, A2))
with(habjoin, plot(habitat, A1))
with(habjoin, plot(habitat, A2)); par(opar)

### same pattern is observed with log-trans and pres/abs data
## gradient from grassland to lagoon to beach