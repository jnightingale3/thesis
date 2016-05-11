############################################################################
# script to test species and trait composition by habitat, season and both #
############################################################################

# get habitat & season data
dat.hab <- data.frame(id_habitats(rownames(spsite))); names(dat.hab) <- 'hab'
dat.sea <- data.frame(id_seasons(rownames(spsite))); names(dat.sea) <- 'sea'
dat.habsea <- data.frame(dat.hab, dat.sea)

# make distance matrices
dist.hab <- gowdis(dat.hab)
dist.sea <- gowdis(dat.sea)
dist.habsea <- gowdis(dat.habsea)

#######################
## species distances ##
#######################
# look at linear models
lm.sp.h  <- lm(dist.spp ~ dist.hab); summary(lm.sp.h) # v sig
lm.sp.s  <- lm(dist.spp ~ dist.sea); summary(lm.sp.s) # a bit sig
lm.sp.hs <- lm(dist.spp ~ dist.habsea); summary(lm.sp.hs) # v sig
# AIC(lm(dist.spp ~ dist.hab)) # easily the best by about 300
# AIC(lm(dist.spp ~ dist.sea))
# AIC(lm(dist.spp ~ dist.habsea))


###################################
## functional composition (CWMs) ##
###################################

# calculate CWMs - uses mean/dominant traits, not distance
cwm <- functcomp(trait_fd, as.matrix(spsite), CWM.type = 'dom',
                 bin.num=c(behav_vars, diet_vars))

# separate cwm matrices
cwm.beh <- subset(cwm, select=behav_vars) 
cwm.diet<- subset(cwm, select=diet_vars)
cwm.mph <- subset(cwm, select=morph_vars)

# how do they correlate?
# behav & dist a lot; mean with everything
# morph is doing something else
# pairs(data.frame(as.vector(cwm.behdist), as.vector(cwm.diedist),
#                  as.vector(cwm.mordist), as.vector(cwm.dist)), 
#       upper.panel = panel.cor)

# make them numeric
# cwm.beh %<>% colwise(as.numeric)(cwm.beh) %>% subtract(1)
# cwm.diet%<>% colwise(as.numeric)(cwm.diet)%>% subtract(1)

# sepatate dist matrices
cwm.behdist <- gowdis(cwm.beh)#, asym.bin = 1:ncol(cwm.beh))
cwm.diedist <- gowdis(cwm.diet)#, asym.bin = 1:ncol(cwm.diet))
cwm.mordist <- gowdis(cwm.mph)
# and overall
cwm.dist <- (cwm.behdist + cwm.diedist + cwm.mordist) / 3


##################
## test overall ##
##################
lm.fc.h  <-lm(cwm.dist ~ dist.hab); summary(lm.fc.h) # v sig
lm.fc.s  <-lm(cwm.dist ~ dist.sea); summary(lm.fc.s) # not sig
lm.fc.hs <-lm(cwm.dist ~ dist.habsea); summary(lm.fc.hs) # v sig
# AIC(lm(cwm.dist ~ dist.hab)) # easily the best by about 60
# AIC(lm(cwm.dist ~ dist.sea))
# AIC(lm(cwm.dist ~ dist.habsea))

## test sub matrices

## test beh ##
lm.cb.h  <- lm(cwm.behdist ~ dist.hab); summary(lm.cb.h) # v sig
lm.cb.s  <- lm(cwm.behdist ~ dist.sea); summary(lm.cb.s) # not sig
lm.cb.hs <- lm(cwm.behdist ~ dist.habsea); summary(lm.cb.hs )# v sig
# AIC(lm(cwm.behdist ~ dist.hab)) # easily the best by about 25
# AIC(lm(cwm.behdist ~ dist.sea))
# AIC(lm(cwm.behdist ~ dist.habsea))

# # which behaviour(s) are different in winter?
# seas_behav <- apply(cwm.beh, 2, function(x) {
#   mod <-glm(x ~ dat.sea$sea, family=binomial(link = "logit"))
#     coef <- coef(summary(mod))[2]
#   return(coef)}) %>% data.frame %>%
#   cbind(apply(cwm.beh, 2, function(x) {
#       mod <-glm(x ~ dat.sea$sea, family=binomial(link = "logit"))
#       pvalue = coef(summary(mod))[8]
#       return(pvalue)}) %>% as.numeric); names(seas_behav) <- c('Coefficient', 'p value')

# probing, pecking, scavenging less common winter


## test diet ##
lm.cd.h  <- lm(cwm.diedist ~ dist.hab); summary(lm.cd.h) # v sig
lm.cd.s  <- lm(cwm.diedist ~ dist.sea); summary(lm.cd.s) # not sig
lm.cd.hs <- lm(cwm.diedist ~ dist.habsea); summary(lm.cd.hs) # v sig
# AIC(lm(cwm.diedist ~ dist.hab)) # easily the best by about 60
# AIC(lm(cwm.diedist ~ dist.sea))
# AIC(lm(cwm.diedist ~ dist.habsea))


## test morph ##
lm.cm.h  <- lm(cwm.mordist ~ dist.hab); summary(lm.cm.h) # v sig
lm.cm.s  <- lm(cwm.mordist ~ dist.sea); summary(lm.cm.s) # not sig
lm.cm.hs <- lm(cwm.mordist ~ dist.habsea); summary(lm.cm.hs) # v sig
# AIC(lm(cwm.mordist ~ dist.hab)) # easily the best by about 10
# AIC(lm(cwm.mordist ~ dist.sea))
# AIC(lm(cwm.mordist ~ dist.habsea))