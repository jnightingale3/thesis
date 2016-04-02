alba <- read.csv("~/Dropbox/Thesis/Repo/thesis/Activity Scans/alba.csv")

alba <- within(alba,  {prop.forag = (forag / (forag + roost + preen))
                       flock.size = (forag + roost + preen)} )

# foraging
with(alba, plot(location, prop.forag, main='Proportion of birds foraging') )
with(alba, plot(location, rank(prop.forag), main='Rank prop. of birds foraging') )

# flock sizes
with(alba, plot(location, log(flock.size), main='Flock size') )
with(alba, plot(location, rank(flock.size), main='Rank flock size') )

# flock size and proportion foraging
with(alba, plot(log(flock.size), prop.forag))
lm.flock.forag <- lm(prop.forag ~ flock.size, data=alba)
#summary(lm.)

## fit binomial glm
glm.prop.fsize <- glm(cbind(forag, (roost + preen)) ~ flock.size, 
                      family=quasibinomial(link = "logit"))
summary(glm.prop.fsize)
# add fitted values to alba dataframe
alba <- cbind(alba, predict(glm.prop.fsize, se.fit=T))
alba$upper <- alba$fit + (alba$fit *alba$se.fit)
alba$lower <- alba$fit - (alba$fit *alba$se.fit)

require(ggplot2)

q <- ggplot(alba, aes(x=flock.size, y=prop.forag))
#q <- q + geom_smooth(method='glm', family='binomial', formula='y~x')
q <- q + geom_ribbon(aes(ymax=upper, ymin=lower), colour='pink', fill='pink')
q <- q + geom_line(aes(y=fit), colour='green')
q <- q + geom_point(shape=21)  + geom_rug() + theme_classic()
q <- q + scale_x_log10()
print(q)
