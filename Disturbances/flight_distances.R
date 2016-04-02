fd <- read.csv("~/Dropbox/Thesis/Repo/thesis/Disturbances/flightdist.csv")

require(lattice)
bwplot(paces~site, data=fd) # not much difference
wilcox.test(paces~site, data=fd) # not sig


bwplot(paces~method, data=fd) # not much difference
wilcox.test(paces~method, data=fd) # not sig, but closer

bwplot(paces~direction, data=fd) # not much difference
wilcox.test(paces~direction, data=fd) # not sig

### average and confints
hist(fd$paces) # looks normal enough
shapiro.test(fd$paces) # no significant deviation from normality

## convert to meters
fd$m <- fd$paces * 0.7

median(fd$m) # 22.4
mad(fd$m) # 7.3

mean(fd$m) # 22.2
source("/home/josh/Documents/R Scripts/standard_error.R") # for se() and ci()
cis <- ci(fd$m) # 20.4 - 24.0 - not too bad!
cis


### check error carried forward
# area of each circle of disturbance
areas <- pi * ( (cis / 2)^2 )
areas

# multiply by number of disturbers (guess approx 50)
tot.disturb <- areas * 50
tot.disturb

# difference in square km
(tot.disturb[2] - tot.disturb[1]) / 1000 / 1000 # 0.006, not too much!
