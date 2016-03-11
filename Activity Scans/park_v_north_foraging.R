alba <- read.csv("~/Dropbox/Thesis/Repo/thesis/Activity Scans/alba.csv")

alba <- within(alba,  {prop.forag = (forag / (forag + roost + preen))
                       flock.size = (forag + roost + preen)} )

# foraging
with(alba, plot(location, prop.forag, main='Proportion of birds foraging') )
with(alba, plot(location, rank(prop.forag), main='Rank prop. of birds foraging') )

# flock sizes
with(alba, plot(location, flock.size, main='Flock size') )
with(alba, plot(location, rank(flock.size), main='Rank flock size') )

# flock size and proportion foraging
with(alba, plot(flock.size, prop.forag))
lm.flock.forag <- lm(prop.forag ~ flock.size, data=alba)
summary(lm.)
abline(with(alba, )))
with(alba, summary(glm(cbind(forag, (roost + preen)) ~ flock.size, family=binomial(link = "logit"))))