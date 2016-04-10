## code from F de Bello

distfuncG<-gowdis(traits1839[, c(3:19, 22, 23, 108:113)])
nana<-apply(as.matrix(distfuncG), 2, sum, na.rm=T)
distfuncGshort<-as.dist((as.matrix(distfuncG)[nana>0, nana>0]))
distfuncGshort[is.na(distfuncGshort)]<-mean(distfuncGshort, na.rm=T)
pcoaG<-dudi.pco(distfuncGshort, scannf = F, nf = 15)
sum(pcoaG$eig[1:15])/sum(pcoaG$eig)

wdist<-function(ppca.scores, w){
  weight<-w/sum(w)#just to make sure the sum is 1
  scores<-ppca.scores
  dissim<-matrix(0, dim(scores)[1], dim(scores)[1])#create the empty dissimilarity result
  colnames(dissim)<-rownames(scores)
  rownames(dissim)<-rownames(scores)
  for(i in 1:dim(scores)[2]){
    scoresi<-scores[, i]
    names(scoresi)<-rownames(scores)
    distai<-w[i]*(as.matrix(dist(scoresi)))^2
    dissim<-distai+dissim
  }
  results<-as.dist(sqrt(dissim))
  return(results)
}

wdist1<-wdist(pcoaG$li, pcoaG$eig[1:15])