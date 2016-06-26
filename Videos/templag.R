require(lubridate) # minutes()

inmet <- read.csv("~/Dropbox/Thesis/Repo/thesis/Temperature/inmet.csv")

inmet$data <- as.character(inmet$data)
data0 <- paste('0', inmet$data, sep='')
inmet$data[nchar(inmet$data)==7] <- data0[nchar(inmet$data)==7]
inmet$date <- as.POSIXct(strptime(inmet$data, format = '%d%m%Y'))
inmet$datetime <- as.POSIXct(strptime(sprintf('%s%s', inmet$data, inmet$hora),
                                      format='%d%m%Y%H', tz='GMT'))

temp <- read.csv("~/Dropbox/Thesis/Repo/thesis/Temperature/alltemp.csv")
temp$datetime <- sprintf('%s %s', temp$date, temp$time)
temp$datetime <- as.POSIXct(strptime(temp$datetime, '%d/%m/%y %H:%M:%S'))

range(temp$datetime, na.rm = T)
farol <- subset(inmet, datetime <= max(temp$datetime, na.rm = T))
farol$hour <- format(farol$datetime, format='%d/%m/%y %H', tz='')

temp$hour <- format(temp$datetime, format='%d/%m/%y %H')

temps <- subset(temp, select=c(datetime, hour, swash.temp, sand.temp), 
                datetime >= min(farol$datetime, na.rm=T))

# tempjoin <- join(temps, farol, by='hour')
# with(tempjoin, plot(temp_inst, swash.temp))
# with(tempjoin, plot(temp_inst, sand.temp))

# summary(lm(swash.temp ~ temp_inst, data=tempjoin))
# summary(lm(sand.temp ~ temp_inst, data=tempjoin))$r.squared

ih <- 0
lagmax <- 60
lagged <- matrix(rep(as.POSIXct(NA), (lagmax+1)*nrow(temps)), ncol=lagmax+1)
lagged %<>% data.frame
names(lagged) <- paste('lag', 0:(lagmax), sep='')
temps$lagged <- NA
sand.varexp <- swash.varexp <- rep(NA, lagmax)
for (ih in 0:lagmax) {
  temps$lagged <- temps$datetime + minutes(ih)
  temps$hour <- format(temps$lagged, '%d/%m/%y %H') 
  lagged[,ih+1] <- match(temps$hour, farol$hour)
  
  swash.varexp[ih] <- summary(lm(temps$swash.temp ~ 
                                   farol$temp_max[(lagged[,ih+1])]))$r.squared
  sand.varexp[ih] <- summary(lm(temps$sand.temp ~ 
                                   farol$temp_max[(lagged[,ih+1])]))$r.squared
}

plot(1:lagmax, swash.varexp,type='l')
plot(1:lagmax, sand.varexp, type='l')

which.max(swash.varexp)
which.max(sand.varexp)

plot(farol$temp_inst[(lagged[,4])], temps$swash.temp)
plot(farol$temp_max[(lagged[,45])], temps$swash.temp)                     
abline( -7.305                ,   1.349  )

#aic
winswa <- lagged[,4]
summary(lm(temps$swash.temp ~ farol$temp_inst[winswa]))
AIC(lm(temps$sand.temp ~ farol$temp_max[winswa]+farol$precipitacao[winswa]))
summary(lm(temps$swash.temp ~ poly(farol$temp_inst[winswa],2)))

# ggplot
swadf <- data.frame(airtemp = farol$temp_inst[winswa],
                    swatemp = temps$swash.temp)
require(ggplot2)
p <- ggplot(swadf, aes(x=airtemp, y=swatemp))
p <- p + geom_point(shape=21) + geom_rug() + theme_classic()
# p <- p + stat_smooth(method = "lm", formula = y ~ x)
p <- p + stat_smooth(method = "lm", formula = y ~ poly(x, 2))
print(p)