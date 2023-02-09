
make_genetic_map<-function(directory,MyMap,marker_t1,marker_t2,SNPS){
  
  pdf(paste0(directory,"step2/genetic_maps/genetic_map_",marker_t1,marker_t2,"_",length(SNPS),".pdf"),10,7)
  par(mfrow=c(13,1))
  chromosomes<-c("chr_01","chr_02","chr_03","chr_04","chr_05","chr_06","chr_07","chr_08","chr_09","chr_10","chr_11","chr_12")
  
  for (i in 1:length(chromosomes)){
    #print(i)
    par(mar=c(0,3,0,2)) 
    x=MyMap[MyMap$CHROMOSOME%in%chromosomes[i],]
    x_subset=MyMap_subset[MyMap_subset$CHROMOSOME%in%chromosomes[i],]
    t1<-which(x$LOCUS==marker_t1)
    if (length(t1)==0){
      t1<-0 }
    
    t2<-which(x$LOCUS==marker_t2)
    if (length(t2)==0){
      t2<-0 }
    
    #print(t1)
    #print(t2)
    plot(x=x$POSITION,y=rep(0,dim(x)[1]),
         col = ifelse(1:nrow(x) == t1, "red", "black"),
         pch = ifelse(1:nrow(x) == t1, 19, 19),
         cex = ifelse(1:nrow(x) == t1, 2, 0.2),
         axes=0,xaxt=NULL,xlim=c(0,max(MyMap$POSITION)+0.2))
    par(new=TRUE)
    
    #PLOT SUBSET
    plot(x_subset$POSITION,rep(0,dim(x_subset)[1]),col="blue",pch=19,cex=1.5,
         axes=0,xaxt=NULL,xlim=c(0,max(MyMap$POSITION)+0.2))
    par(new=TRUE)
    
    #ADD TRAIT MARKERS
    plot(x=x$POSITION,y=rep(0,dim(x)[1]),
         col = ifelse(1:nrow(x) == t2, "red", "black"),
         pch = ifelse(1:nrow(x) == t2, 19, 19),
         cex = ifelse(1:nrow(x) == t2, 2, 0.2),
         axes=0,xaxt=NULL,xlim=c(0,max(MyMap$POSITION)+0.2))
    
    text(x$POSITION,y=rep(0,dim(x)[1]),labels=ifelse(1:nrow(x) == t1,marker_t1,NA),cex=1,pos = 1,offset = 1)
    text(x$POSITION,y=rep(0,dim(x)[1]),labels=ifelse(1:nrow(x) == t2,marker_t2,NA),cex=1,pos = 1,offset = 1)
    
    mtext(text = paste0("chr",i),side=2,cex=0.8,las=2)
    
    
    if (i==12){
      par(mar=c(0,3,0,2)) 
      plot(x=-1,y=-1,axes=0,xaxt=NULL,xlim=c(0,max(MyMap$POSITION)+0.2))
      axis(side = 1,at=seq(0,max(MyMap$POSITION)+0.2,0.2),labels=c(seq(0,max(MyMap$POSITION)+0.2,0.2)*100),
           tick = TRUE,outer=TRUE,line=-2.5)
      #axis(side=2,labels=NA)#,tick = FALSE)
      mtext(side=1,text="CM", outer = TRUE, cex = 0.6,line = -1.5,adj = 0.95)
    }
  }
  
  dev.off()
  
}


################################

chr_genetic_map<-function(directory, file_name){
  
  marker_tab<-read.csv(paste0(directory,'/',file_name,'.csv'))
  colnames(marker_tab)<-c("marker","position")
  marker_tab$position<- as.numeric(marker_tab$position)
  
  ## pch
  pch_all<-rep(19,dim(marker_tab)[1])
  pch_all[1]<-1
  pch_all[dim(marker_tab)[1]]<-1
  ## text one up one down
  pos_all<-rep(c(1,3),dim(marker_tab)[1])
  ## text all down but 2 overlapping
  pos_all<-rep(1,dim(marker_tab)[1])
  pos_all[18]<-3
  pos_all[14]<-3
  
  ####################### plot
  
  pdf(paste0(directory,"/",file_name,".pdf"),10,5)
  
  plot(x=marker_tab$position,y=rep(0,dim(marker_tab)[1]),
       col="black",pch=pch_all,cex=0.5,
       axes=0,xaxt='n',ann = FALSE, xlim=c(0,max(marker_tab$position,na.rm=TRUE)+0.2))
  
  text(marker_tab$position,y=rep(0,dim(marker_tab)[1]),col="blue",
       labels=marker_tab$marker,cex=0.25,pos = pos_all,offset = 2,srt=90,xpd=TRUE)
  text(marker_tab$position,y=rep(0,dim(marker_tab)[1]),
       labels=round((marker_tab$position)/1000000,digits = 1),
       cex=0.25,pos = pos_all,offset = 0.3,xpd=TRUE,srt=45)
  
  abline(h=0)
  
  mtext(text = file_name,side=3,cex=0.8,las=1)
  
  dev.off()

}
