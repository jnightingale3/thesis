#### script to build a table of dominant traits by habitat ####

spsitewhab <- spsite
spsitewhab$hab <- (id_habitats(rownames(spsite)))

## communities by habitat
# total
hab_com_sum <- ddply(spsitewhab, .(hab), numcolwise(sum))
# mean
hab_com_ave <- ddply(spsitewhab, .(hab), numcolwise(function(x){round(mean(x),1)}))
# total no. surveys
hab_com_NCt <- ddply(spsitewhab, .(hab), numcolwise(length)) 
# number of occurrence
spsite_pa <- (spsite>0) %>% data.frame; spsite_pa$hab <- spsitewhab$hab
hab_com_Noc <- ddply(spsite_pa, .(hab), colwise(function(x){sum(as.numeric(x))}))
# frequency of occurrence
hab_com_frq <- (hab_com_Noc / hab_com_NCt) %>% 
  multiply_by(100) %>% round(1)

# get a printable vector of species' migratory status
migorres2<-migorres
levels(migorres2)<-c('Austral', 'Boreal', 'Resident')
migorres2 %<>% as.character
migorres2 <- c('', migorres2)

hab_com_tab <- cbind(colnames(hab_com_sum), migorres2,
                     t(hab_com_frq), '     ', t(hab_com_ave)) %>% 
  data.frame(stringsAsFactors=F)
hab_com_tab[1,1] <- ''
hab_com_tab[1,3:4] <- levels(spsitewhab$hab)
names(hab_com_tab) <- c('Species', 'Life history',
                        ' ', 'Frequency', '  ', '   ', 'Mean')
hab_com_tab[is.na(hab_com_tab)] <- ''

#######################################################
## replace the below chi squared thing
## with Mann-Whitney U tests on all CWMs
## using the cwm data frame
## then can compare all traits
## (including frequency of straight bills :) 
#######################################################


## analyse functional composition by totals per habitat
habcwms <- functcomp(trait_fd, as.matrix(hab_com_sum[,-1]), CWM.type = 'dom',
          bin.num=c(behav_vars, diet_vars)) %>% data.frame
habcwms[,1:3] %<>% exp %>% round(1)
habcwms[,4] %<>% capitalize
habcwms[,5:ncol(habcwms)] %<>% multiply_by(100) %>% round(1)
habcwms #%<>% t

# results table for stargazer
habcwmtab <- data.frame(
  Trait = gsub('[.]', ' ', colnames(habcwms)) %>% capitalize,
  Beach = as.numeric(habcwms[1,]),
  Lake = as.numeric(habcwms[2,]))
habcwmtab[4,-1] <- habcwms[,4]
habcwmtab$Chi.sq <- NA
habcwmtab$p.value <- NA

ih <- NA
for (ih in 5:nrow(habcwmtab)) {
  vars    <- habcwmtab[ih,2:3] %>% matrix %>% as.numeric
  chtestout <- prop.test(x=vars, n=rep(100,nlevels(spsitewhab$hab)))
  habcwmtab$Chi.sq[ih] <- chtestout$statistic %>% round(1)
  habcwmtab$p.value[ih]<- chtestout$p.value
}
habcwmtab$p.value %<>% p.adjust(method='fdr')
habcwmtab <- habcwmtab[order(habcwmtab$p.value),]
habcwmtab$Chi.sq %<>% round(1)
habcwmtab$p.value%<>% round(3)
habcwmtab[which(habcwmtab$p.value<0.001),'p.value'] <- '<0.001'
habcwmtab[is.na(habcwmtab)] <- '-'
habcwmtab[which(habcwmtab$p.value=='NaN'),'p.value'] <- '-'
