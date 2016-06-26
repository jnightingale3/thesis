## ID number of unique flocks

# read in data
vids <- read.csv("/home/josh/Dropbox/Thesis/Repo/thesis/Videos/video_details.csv")

# code formed from date and flock number
vids$flockid <- sprintf('%s-%s', vids$Date, vids$Flock)

# ID species of flock
vids$sp <- 'Fus' # set all to fuscicollis
vids$sp[grepl('San', vids$flockid)] <- 'San'

# how many unique flocks?
length(unique(vids$flockid)) # 74
# by species:
length(unique(vids$flockid[vids$sp=='Fus'])) # 43
length(unique(vids$flockid[vids$sp=='San'])) # 31

# need more sanderling videos - aim for equal sample sizes
# by randomly selecting 10 more flocks to watch videos from
flocks_w_2plus <- table(vids$flockid[vids$sp=='San']) >1 # flocks with 2+ videos
# randomly draw 10 different flocks:
sample(table(vids$flockid[vids$sp=='San'])[flocks_w_2plus],
       12, replace=F)

# which video to watch from the flock?
sample(1:3, 1) # 1:n, where n = total number of videos
