### use the simulated data to make comparisons with real data

fake_fd_metrics = list(
  Dispersion = as.matrix(read.csv('data/dat.fake.FDis.csv', row.names = 1)),
  Divergence = as.matrix(read.csv('data/dat.fake.FDiv.csv', row.names = 1)),
  Evenness = as.matrix(read.csv('data/dat.fake.FEve.csv', row.names = 1)),
  No.Species = as.matrix(read.csv('data/dat.fake.nbsp.csv', row.names = 1)),
  Richness = as.matrix(read.csv('data/dat.fake.FRic.csv', row.names = 1))
)

ij <- 1
SES <- matrix(0, nrow=nrow(spsite), ncol=length(fake_fd_metrics))
colnames(SES) <- letters[1:length(fake_fd_metrics)]
for (ij in 1:length(fake_fd_metrics)) {
  metric <- names(fake_fd_metrics)[ij]
  Iobs <- obsdf[,metric]
  Isim <- rowMeans(fake_fd_metrics[[ij]])
  sigsim <- apply(fake_fd_metrics[[ij]], 1 , sd) %>% as.numeric
  
  SES[,ij] <- (Iobs - Isim) / sigsim
  colnames(SES)[ij] <- metric
}
rownames(SES) <- rownames(spsite)
SES %<>% as.data.frame

sesl <- SES %>% melt
names(sesl) <- c('Metric', 'SES')
# # plot
# p <- ggplot(sesl) 
# p <- p + geom_vline(xintercept=0, color='red') + 
#   geom_histogram(aes(x=Value), alpha=2/3) + 
#   geom_vline(xintercept=0, color='red') +
#   facet_wrap(~Metric, scales='free')
# print(p)

##############
#### old #####
##############

# expected_values = llply(fake_fd_metrics, .fun=function(x) {
#   return(data.frame(
#     lower  = apply(x, 1, FUN=function(x){return(sort(x)[ceiling(length(x)*.025)])}),
#     median = apply(x, 1, median),
#     upper  = apply(x, 1, FUN=function(x){return(sort(x)[floor(length(x)*.975)])}),
#     sd = apply(x, 1, sd),
#     row.names = rownames(x)
#   ))
# })
# 
# # l_ply(expected_values, .fun=function(x) {
# #   hist(x$median)
# # })
# 
# 
# sitestokeep <- which(rownames(expected_values[[1]]) %in% rownames(fdat))
# names(sitestokeep) <- rownames(expected_values[[1]])[which(rownames(expected_values[[1]]) %in% rownames(fdat))]
# identical(names(sitestokeep), rownames(fdat))
# 
# 
# fdat_simt <- ldply(fake_fd_metrics, .fun=function(x) {apply(x, 1, mean) })
# fdat_sim <- data.frame(t(fdat_simt[,-1]))
# colnames(fdat_sim) <- gsub('fake.', '',fdat_simt[,1])
# rownames(fdat_sim) <- colnames(fdat_simt)[-1]
# 
# fdat_sdt <- ldply(fake_fd_metrics, .fun=function(x) {apply(x, 1, sd) })
# fdat_sd <- data.frame(t(fdat_sdt[,-1]))
# colnames(fdat_sd) <- gsub('fake.', '',fdat_sdt[,1])
# rownames(fdat_sd) <- colnames(fdat_sdt)[-1]
# 
# fdat_oex <- (fdat[,na.omit(match(colnames(fdat_sim), colnames(fdat)))] - # obs
#   fdat_sim[sitestokeep,]) / # minus expected
#   fdat_sd[sitestokeep,] # divided by standard deviation -> SES
# fdat_oex <- cbind(fdat_oex, extract_metadata(rownames(fdat_oex)))
# fdat_oex$nbsp <- fdat_sim$nbsp[sitestokeep] # don't need SES of species number!
# 
# fdat.long <- melt(fdat_oex, id.vars = c(colnames(metadata), 'nbsp'))
# 
# 
# 
# head(expected_values[['fake.nbsp']])
# 
# SES.FDiv <- (fdat$FDiv - # observed FD
#   expected_values[['fake.FDiv']][sitestokeep,'median']) / # minus expected
#   expected_values[['fake.FDiv']][sitestokeep,'sd']# divided by sd
# 
# hist(SES.FDiv)
# 
# SES.FDiv <- cbind(SES.FDiv, extract_metadata(rownames(fdat)))
# with(SES.FDiv, plot(years, SES.FDiv)); abline(0,0, col='blue')
# with(SES.FDiv, summary(lm(SES.FDiv ~ years))) # nada
# with(SES.FDiv, plot(habitat, SES.FDiv))
# with(SES.FDiv, plot(season, SES.FDiv))
# 
# with(SES.FDiv, wilcox.test(SES.FDiv)) # significantly higher than expected
# 
# summary(lm(SES.FDiv ~ habitat, data=SES.FDiv))
# summary(lm(SES.FDiv ~ habitat + fdat$nbsp, data=SES.FDiv))
# 
# with(SES.FDiv, plot(fdat$nbsp, SES.FDiv))
# abline(lm(SES.FDiv$SES.FDiv ~ fdat$nbsp))
# summary(lm(SES.FDiv$SES.FDiv ~ fdat$nbsp)) # sigly more
# 
# with(fdat, plot(habitat, nbsp))
