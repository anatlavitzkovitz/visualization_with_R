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
