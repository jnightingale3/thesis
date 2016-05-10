# script to test species and trait composition by habitat, season and both

# get data
dat.hab <- data.frame(id_habitats(rownames(spsite)))
dat.sea <- data.frame(id_seasons(rownames(spsite)))
dat.habsea <- data.frame(dat.hab, dat.sea)

# make distance matrices
dist.hab <- gowdis(dat.hab)
dist.sea <- gowdis(dat.sea)
dist.habsea <- gowdis(dat.habsea)

# look at linear models
summary(lm(dist.spp ~ dist.hab)) # v sig
summary(lm(dist.spp ~ dist.sea)) # a bit sig
summary(lm(dist.spp ~ dist.habsea)) # v sig
AIC(lm(dist.spp ~ dist.hab)) # easily the best by about 300
AIC(lm(dist.spp ~ dist.sea))
AIC(lm(dist.spp ~ dist.habsea))

# calculate CWMs
cwm <- functcomp(trait_fd, as.matrix(spsite), CWM.type = 'dom')

# separate cwm matrices
cwm.beh <- subset(cwm, select=behav_vars) 
cwm.diet<- subset(cwm, select=diet_vars)
cwm.mph <- subset(cwm, select=morph_vars)

# make them numeric
cwm.beh %<>% colwise(as.numeric)(cwm.beh) %>% subtract(1)
cwm.diet%<>% colwise(as.numeric)(cwm.diet)%>% subtract(1)

# sepatate dist matrices
cwm.behdist <- gowdis(cwm.beh, asym.bin = 1:ncol(cwm.beh))
cwm.diedist <- gowdis(cwm.diet, asym.bin = 1:ncol(cwm.diet))
cwm.mordist <- gowdis(cwm.mph)
# and overall
cwm.dist <- (cwm.behdist + cwm.diedist + cwm.mordist) / 3

## test overall
summary(lm(cwm.dist ~ dist.hab)) # v sig
summary(lm(cwm.dist ~ dist.sea)) # not sig
summary(lm(cwm.dist ~ dist.habsea)) # v sig
AIC(lm(cwm.dist ~ dist.hab)) # easily the best by about 60
AIC(lm(cwm.dist ~ dist.sea))
AIC(lm(cwm.dist ~ dist.habsea))

## test sub matrices

## test beh
summary(lm(cwm.behdist ~ dist.hab)) # v sig
summary(lm(cwm.behdist ~ dist.sea)) # v sig
summary(lm(cwm.behdist ~ dist.habsea)) # v sig
AIC(lm(cwm.behdist ~ dist.hab)) # easily the best by about 25
AIC(lm(cwm.behdist ~ dist.sea))
AIC(lm(cwm.behdist ~ dist.habsea))

## test diet
summary(lm(cwm.diedist ~ dist.hab)) # v sig
summary(lm(cwm.diedist ~ dist.sea)) # not sig
summary(lm(cwm.diedist ~ dist.habsea)) # v sig
AIC(lm(cwm.diedist ~ dist.hab)) # easily the best by about 60
AIC(lm(cwm.diedist ~ dist.sea))
AIC(lm(cwm.diedist ~ dist.habsea))

## test morph
summary(lm(cwm.mordist ~ dist.hab)) # v sig
summary(lm(cwm.mordist ~ dist.sea)) # not sig
summary(lm(cwm.mordist ~ dist.habsea)) # v sig
AIC(lm(cwm.mordist ~ dist.hab)) # easily the best by about 10
AIC(lm(cwm.mordist ~ dist.sea))
AIC(lm(cwm.mordist ~ dist.habsea))