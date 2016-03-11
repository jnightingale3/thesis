#################
### Read data ###
#################
temps <- read.csv("~/Dropbox/Thesis/Repo/thesis/Temperature/alltemp.csv")

##################
### Swash temp ###
##################

# create specific data set, omitting missing values
swash <- na.omit(subset(temps, select=c(time, swash.temp)))
# tell R how to understand times
swash$time <- as.POSIXct(swash$time, format='%T')

# order data by time (for plotting curve later)
swash <- swash[order(swash$time),]
with(swash, plot(time, swash.temp)) # plot relationship - looks curvey?

# fit parabola
qm.swash <- lm(swash.temp ~ poly(time, 2), data=swash) # 2nd order polynomial
summary(qm.swash) # very significant model, reasonable R^2 ~0.3
lines(swash$time, qm.swash$fitted.values) # a perfect curve :)

# ggplot
require(ggplot2)
q <- ggplot(swash, aes(x=time, y=swash.temp))
q <- q + geom_smooth(method=lm, formula='y ~ poly(x, 2)')
q <- q + geom_point(shape=21)
q <- q + theme_classic(base_size=17)
print(q)

##################
### Sand temp ###
##################

# create specific data set, omitting missing values
sand <- na.omit(subset(temps, select=c(time, sand.temp)))
# tell R how to understand times
sand$time <- as.POSIXct(sand$time, format='%T')

# order data by time (for plotting curve later)
sand <- sand[order(sand$time),]
with(sand, plot(time, sand.temp)) # plot relationship - looks curvey?

# fit parabola
qm.sand <- lm(sand.temp ~ poly(time, 2), data=sand) # 2nd order polynomial
summary(qm.sand) # very significant model, good R^2 ~0.65
lines(sand$time, qm.sand$fitted.values) # a perfect curve :)

# ggplot
q <- ggplot(sand, aes(x=time, y=sand.temp))
q <- q + geom_smooth(method=lm, formula='y ~ poly(x, 2)')
q <- q + geom_point(shape=21)
q <- q + theme_classic(base_size=17)
print(q)


#################################
### Correlation between temps ###
#################################

# create data set
both.temps <- na.omit(subset(temps, select=c(sand.temp, swash.temp)))
with(both.temps, plot(sand.temp, swash.temp))

# linear relationship?
lm.bothtemp <- lm(swash.temp ~ sand.temp, data=both.temps)
summary(lm.bothtemp) # v sig; R^2 ~0.4 which is ok
# coefficient ~0.2 - swash temp much less variable!
abline(lm.bothtemp) # add trendline to graph


############
### Map! ###
############

with(temps, plot(longitude, latitude, asp=1)) # it's the coast!
# points v clustered around park entrance; ...go farther!
