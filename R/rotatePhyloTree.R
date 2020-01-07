tipOrder<-function(phyloTree){
  tporder<-phyloTree$edge[,2]
  tporder<-tporder[tporder<=Ntip(phyloTree)]
  tporder<-as.character(phyloTree$tip.label[tporder])
  return(tporder)
}

rotatePhyloTree<-function(phyloTree,spid){
  tip<-which(phyloTree$tip.label==spid)
  lcas <- mrca(phyloTree)[,spid]
  # phyloTree$edge.length<-rep(1,241)
  phyloTree$edge.length<-NULL
  tgroup<-dist.nodes(phyloTree)[,tip]
  tgroup<-tgroup[lcas]
  names(tgroup)<-names(lcas)
  #---
  ct<-1;tp<-tgroup
  for(i in sort(unique(tgroup))){
    tgroup[tp==i]<-ct;ct<-ct+1
  }
  #---
  tord<-rev(rank(rev(tgroup), ties.method = "first"))
  #---
  phyloTree<-rotateConstr(phyloTree,names(sort(tord,decreasing=TRUE)))
  tord<-tord[tipOrder(phyloTree)]
  #---
  tgroup<-tgroup[names(tord)]
  phyloTree$tip.group<-tgroup
  #---
  lcas<-lcas[names(tord)]
  #atualiza lca do spid, troca pelo nodo mais proximo
  tp<-phyloTree$edge[,2]
  lcas[spid]<-phyloTree$edge[which(tp==tip),1]
  phyloTree$tip.lcas<-lcas
  #---
  return(phyloTree)
}
