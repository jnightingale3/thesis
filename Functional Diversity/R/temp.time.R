## trends over time
# require(magrittr)
# require(reshape2)
# require(ggplot2)
# require(gridExtra)
source('def.extract_metadata.R')
metadata <- extract_metadata(rownames(fd.test$CWM))


fdat <- cbind(metadata, fd.test$FDiv, fd.test$FEve, fd.test$FDis, 
              fd.test$FRic, fd.test$nbsp)
names(fdat) <- gsub('fd.test[$]', '', names(fdat))
fdat.long <- melt(fdat, id.vars = c(colnames(metadata), 'nbsp'))

p <- ggplot(fdat.long, aes(x=years, y=value))
p <- p + geom_point(aes(shape=season,fill=habitat, color=habitat))
p <- p + facet_grid(variable ~ season, scales='free')
p <- p + geom_smooth(method = 'lm', aes(color=habitat), se = F)
p <- p + scale_x_continuous(breaks = seq(from=min(fdat$years), to=max(fdat$years), 
                                                   by=2), minor_breaks = NULL)
p1 <- p + ylab('Observed value')
print(p1)

p <- ggplot(fdat.long, aes(x=nbsp, y=value))
p <- p + geom_point(aes(shape=season,fill=habitat, color=habitat))
p <- p + facet_grid(variable ~ season, scales='free')
p <- p + geom_smooth(method = 'lm', aes(color=habitat), se = F)
p2 <- p + ylab('Observed value') + ggtitle('')
print(p2)

p <- ggplot(fdat.long, aes(x=season, y=value))
p <- p + geom_boxplot(aes(fill=season))
p <- p + geom_point(shape=21, alpha=.5)
p <- p + facet_grid(variable~habitat, scales='free')
p3 <- p + ylab('Observed value')
print(p3)