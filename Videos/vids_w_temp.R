## ID videos with temperature data
require(birk) # for which.closest()
require(magrittr) # ceci n'est pas un pipe
require(plyr)
# need unique_clocks.R to ID unique flocks first

## read in data
# videos
vids <- read.csv("/home/josh/Dropbox/Thesis/Repo/thesis/Videos/video_details.csv")
# temp
temp <- read.csv("~/Dropbox/Thesis/Repo/thesis/Temperature/alltemp.csv")

# add date-time
vids$datetime <- sprintf('%s %s', vids$Date, vids$local.time)
vids$datetime <- as.POSIXct(strptime(vids$datetime, '%d/%m/%y %H:%M:%S'))
temp$datetime <- sprintf('%s %s', temp$date, temp$time)
temp$datetime <- as.POSIXct(strptime(temp$datetime, '%d/%m/%y %H:%M:%S'))

# code formed from date and flock number
vids$flockid <- sprintf('%s-%s', vids$Date, vids$Flock)

### find the closest time
N <- length(vids$datetime)
time_matcher <- data.frame(vid=rep(NA,N), temp=rep(NA,N)); ii <- 1
for (ii in 1:N) {
  tii <- which.closest(temp$datetime, vids$datetime[ii])  
  time_matcher$vid[ii] <- ii
  time_matcher$temp[ii] <- tii
}
time_matcher$diff <- difftime(vids$datetime[time_matcher$vid],
                              temp$datetime[time_matcher$temp],
                              units-'mins') %>% abs
time_matcher$closenough <- time_matcher$diff <= 15
sum(time_matcher$closenough)

time_matcher$flockid <- vids$flockid
time_matcher$videocode <- vids$Video

# unique flock ids
unqflid <- unique(time_matcher$flockid)
time_matcher$flockno <- match(time_matcher$flockid, unqflid)

# what is the first video code for each flock?
vidmin <- ddply(time_matcher, .(flockid), function(x){min(x$vid)})
time_matcher$vidno <- time_matcher$vid - 
  vidmin$V1[match(time_matcher$flockid, vidmin$flockid)] + 1


# table with videos ordered per flock
time_matcher[order(time_matcher$flockno, time_matcher$diff),]

