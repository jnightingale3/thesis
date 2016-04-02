### checking distribution of video times
## when should i conduct more fieldwor

vids <- read.csv("/home/josh/Dropbox/Thesis/Repo/thesis/Videos/video_details.csv")

vids$time <- as.POSIXct(vids$local.time, format='%T')

hist(vids$time, breaks='hours')

# overall, need more before 1400, and especially before 1000
# no surprises there ;)

#### species-specific analyses ###

# use flock ID to create vector of species
vids$sp <- NA
vids$sp[grepl('Fus', vids$Flock)] <- 'fus'
vids$sp[grepl('San', vids$Flock)] <- 'san'
vids$sp <- factor(vids$sp)
summary(vids$sp)

# histograms
with(vids[vids$sp=='fus',], hist(time, breaks='hours', main='fus', freq=T))
with(vids[vids$sp=='san',], hist(time, breaks='hours', main='san', freq=T))

## the problem is only serious with fusc
## therefore, future films of fusc should be in the morning
## san at all times of day (and lots of them!)