rm(list=ls())
install.packages("libxml2", repos="http://xmlsoft.org/sources/",type="source")
install.packages("libiconv", repos="http://ftp.gnu.org/pub/gnu/libiconv/",type="source")
install.packages("XML")
#If install.packages("XML") can not install the XML package you can use the follow one.
#install.packages("XML",repos="http://www.omegahat.org/R",type="source")
#We also can install the XML package by the follow way.
#install.packages(pkgs="f://CS513final//libxml2-2.7.3.tar.gz", repos=NULL,type="source")
#install.packages(pkgs="f://CS513final//libiconv-1.14.tar.gz", repos=NULL,type="source")
#install.packages(pkgs="f://CS513final//XML_3.6-1.tar.gz", repos=NULL,type="source")
library(XML)
mydata<-read.csv("f://CS513final//machine_location1.csv")

#calculate the distance between two cities
distance_between<-function(lat1,long1,h1,lat2,long2,h2)
{  
  R <- 6371.004 
  LAT1 <- lat1 *  pi/180
  LON1 <- long1 * pi/180
  x1 <-  -cos(LAT1) * cos(LON1)
  y1 <-  cos(LAT1) * sin(LON1)
  z1 <-  sin(LAT1)
  LAT2 <- lat2 *  pi/180
  LON2 <- long2 * pi/180
  x2 <-  -cos(LAT2) * cos(LON2)
  y2 <-  cos(LAT2) * sin(LON2)
  z2 <-  sin(LAT2)
  
  if(is.na(acos(x1*x2+y1*y2+z1*z2))){
    dis <- 9999999999
  }
  
  else
  {
    angle <- acos(x1*x2+y1*y2+z1*z2)
    dis <- R*angle+(h1+h2)/2000*angle
  }
}
#dist<-matrix(0,nrow=50,ncol=50)
#for(i in 1:50){
#  for(j in 1:50){
#    dist[i,j]<-distance_between(mydata[i,3],mydata[i,4],mydata[i,5],mydata[j,3],mydata[j,4],mydata[j,5])
#  }
#}

geo_kmeans <- function(A,cluster,times)
{
  cnum <- 3
  rnum <- nrow(A)
  npoint<-cluster
  cid<-matrix(0,nrow=npoint, ncol=cnum+1)
  dist<-matrix(0,nrow=rnum,ncol=npoint)
  rannum <- matrix(0,nrow=npoint,ncol=1)
  
  for(i in 1:npoint){
    rannum[i] <- floor(runif(1,min=1,max=(rnum+1)))
    if(i == 1){
      flag <- 1
    }
    else {
      flag <-0
    }
    while(flag == 0){
      
      for(j in 1:(i-1))
      {
        if(rannum[i] == rannum[j]){
          flag <- 0;
          rannum[i] <- floor(runif(1,min=1,max=(rnum+1)))
          break
        }
        else
        {
          flag <-1;
        }
      }
    }
    cid[i,1:cnum]=A[rannum[i],1:cnum]
  }
  Asum<-0
  Csum<-0
  for (t in 1:times){
    
    for(i in 1:rnum){
      for(j in 1:npoint){
        if((cid[j,1] == A[i,1]) && (cid[j,2] == A[i,2]) && (cid[j,3] == A[i,3]))
        {
          dist[i,j] <- 0
        }
        else
        {
          dist[i,j]<-distance_between(cid[j,1],cid[j,2],cid[j,3],A[i,1],A[i,2],A[i,3])
        }
        
      }  
      
    }
    for(i in 1:rnum)
    {
      for(j in 1:cluster)
      {
        if(dist[i,j] == min(dist[i,]))
        {
          A[i,4] <- j;
        }
      }
      #A[i,cnum+1]<-which(dist[i,]==min(dist[i,]))
    }
    Asum<-0
    for(i in 1:rnum){
      Asum<-Asum+dist[i,A[i,4]]
    }
    
    bcv<-0
    for(i in 1:(npoint-1)) {
      bcv <- bcv + distance_between(cid[i,1],cid[i,2],cid[i,3],cid[i+1,1],cid[i+1,2],cid[i+1,3])
    }
    bcv <- bcv + distance_between(cid[1,1],cid[1,2],cid[1,3],cid[npoint,1],cid[npoint,2],cid[npoint,3])
    
    Csum <- bcv/Asum
    #   
    newx <- tapply(A[,1], A[,4], mean)
    newy <- tapply(A[,2], A[,4], mean)
    newz <- tapply(A[,3], A[,4], mean)
    for(j in 1:npoint){
      cid[j,1]=newx[j]
      cid[j,2]=newy[j]
      cid[j,3]=newz[j]
      cid[j,4]<-Csum
    }
    
  }
  return(cid)
  #Write centers into XML element
  node1<-xmlNode("GeoCenters")
  for(i in 1:npoint){
    node1$children[[i]] <-xmlNode("GeoCenter",
                                 xmlNode("Longtitude",xmlText = c(cid[i,1])),
                                 xmlNode("Latitude",xmlText = c(cid[i,2])),
                                 xmlNode("Elevation",xmlText = c(cid[i,3])))
  }
  cat(saveXML(node1))
}

geo_kmedioids <- function(A,cluster,times)
{
  cnum <- 3
  rnum <- nrow(A)
  npoint<-cluster
  cid<-matrix(0,nrow=npoint, ncol=cnum+2)
  dist<-matrix(0,nrow=rnum,ncol=npoint)
  rannum <- matrix(0,nrow=npoint,ncol=1)
  
  for(i in 1:npoint){
    rannum[i] <- floor(runif(1,min=1,max=(rnum+1)))
    if(i == 1){
      flag <- 1
    }
    else {
      flag <-0
    }
    while(flag == 0){
      
      for(j in 1:(i-1))
      {
        if(rannum[i] == rannum[j]){
          flag <- 0;
          rannum[i] <- floor(runif(1,min=1,max=(rnum+1)))
          break
        }
        else
        {
          flag <-1;
        }
      }
    }
    cid[i,1:cnum]=A[rannum[i],1:cnum]
  }
  for(k in 1:npoint)
  {
    cid[k,4]<-0
    cid[k,5]<-0
  }
  Asum<-0
  Csum<-9999999999
  
  for (t in 1:times){
    
    for(i in 1:rnum){
      for(j in 1:npoint){
        if((cid[j,1] == A[i,1]) && (cid[j,2] == A[i,2]) && (cid[j,3] == A[i,3]))
        {
          dist[i,j] <- 0
        }
        else
        {
          dist[i,j]<-distance_between(cid[j,1],cid[j,2],cid[j,3],A[i,1],A[i,2],A[i,3])
        }
      }     
    }
    for(j in 1:npoint){cid[j,5]<-0}
    for(i in 1:rnum)
    {
      for(j in 1:npoint)
      {
        if(dist[i,j] == min(dist[i,]))
        {
          A[i,4] <- j
          cid[j,5]<-cid[j,5]+dist[i,j]
        }
      }
      #A[i,cnum+1]<-which(dist[i,]==min(dist[i,]))
    }
    Asum<-0
    for(i in 1:rnum){
      Asum<-Asum+dist[i,A[i,4]]
    }
    
    bcv<-0
    for(i in 1:(npoint-1)) {
      bcv <- bcv + distance_between(cid[i,1],cid[i,2],cid[i,3],cid[i+1,1],cid[i+1,2],cid[i+1,3])
    }
    bcv <- bcv + distance_between(cid[1,1],cid[1,2],cid[1,3],cid[npoint,1],cid[npoint,2],cid[npoint,3])
    
    Csum <- bcv/Asum
    #   
    for(j in 1:npoint){
      NewCenter<-cid[j,4]
      for(i in 1:rnum){
        if((A[i,4] == j) && (cid[j,4] != i))
        {
          NewSum<-0
          for(k in 1:rnum)
          {
            if(A[k,4] == j)
            {
              if(i != k) NewSum<-NewSum+ distance_between(A[i,1],A[i,2],A[i,3],A[k,1],A[k,2],A[k,3])
            }
          }
          if (NewSum<cid[j,5]) 
          {
            NewCenter<-i
          }
        }    
      }
      if(NewCenter != 0)
      {
        cid[j,4]<-NewCenter
        cid[j,1]<-A[NewCenter,1]
        cid[j,2]<-A[NewCenter,2]
        cid[j,3]<-A[NewCenter,3]
      }
    }
    
  }
  return(cid)
  #Write centers into XML element
  node2<-xmlNode("GeoCenters")
  for(i in 1:cluster){
    node2$children[[i]] <-xmlNode("GeoCenter",
                                 xmlNode("Longtitude",xmlText = c(cid[i,1])),
                                 xmlNode("Latitude",xmlText = c(cid[i,2])),
                                 xmlNode("Elevation",xmlText = c(cid[i,3])))
  }
  cat(saveXML(node2))
}

center_kmeans <- function(A,cluster,times,startcid)
{
  cnum <- 3
  rnum <- nrow(A)
  npoint<-cluster
  cid<-startcid
  dist<-matrix(0,nrow=rnum,ncol=npoint)
  Asum<-0
  Csum<-0
  for (t in 1:times){
    
    for(i in 1:rnum){
      for(j in 1:npoint){
        if((cid[j,1] == A[i,1]) && (cid[j,2] == A[i,2]) && (cid[j,3] == A[i,3]))
        {
          dist[i,j] <- 0
        }
        else
        {
          dist[i,j]<-distance_between(cid[j,1],cid[j,2],cid[j,3],A[i,1],A[i,2],A[i,3])
        }
        
      }  
      
    }
    for(i in 1:rnum)
    {
      for(j in 1:cluster)
      {
        if(dist[i,j] == min(dist[i,]))
        {
          A[i,4] <- j;
        }
      }
      #A[i,cnum+1]<-which(dist[i,]==min(dist[i,]))
    }
    Asum<-0
    for(i in 1:rnum){
      Asum<-Asum+dist[i,A[i,4]]
    }
    
    bcv<-0
    for(i in 1:(npoint-1)) {
      bcv <- bcv + distance_between(cid[i,1],cid[i,2],cid[i,3],cid[i+1,1],cid[i+1,2],cid[i+1,3])
    }
    bcv <- bcv + distance_between(cid[1,1],cid[1,2],cid[1,3],cid[npoint,1],cid[npoint,2],cid[npoint,3])
    
    Csum <- bcv/Asum
    #   
    newx <- tapply(A[,1], A[,4], mean)
    newy <- tapply(A[,2], A[,4], mean)
    newz <- tapply(A[,3], A[,4], mean)
    for(j in 1:npoint){
      cid[j,1]=newx[j]
      cid[j,2]=newy[j]
      cid[j,3]=newz[j]
      cid[j,4]<-Csum
    }
    
  }
  return(cid)
  #Write centers into XML element
  node3<-xmlNode("GeoCenters")
  for(i in 1:cluster){
    node3$children[[i]] <-xmlNode("GeoCenter",
                                 xmlNode("Longtitude",xmlText = c(cid[i,1])),
                                 xmlNode("Latitude",xmlText = c(cid[i,2])),
                                 xmlNode("Elevation",xmlText = c(cid[i,3])))
  }
  cat(saveXML(node3))
}

new_kmeans <- function(A,cluster,times,bigk)
{
  ktimes <-floor(nrow(A)/10);
  cnum<-3;
  rnum <- nrow(A)
  npoint<-cluster
  cids <- matrix(0,nrow=bigk*ktimes, ncol=cnum+1);
  dist<-matrix(0,nrow=rnum,ncol=npoint)
  for(i in 1:ktimes)
  {
    tempcid<-geo_kmeans(A,bigk,times);
    for(j in 1:bigk)
    {
      cids[((i-1)*bigk+j),1] <- tempcid[j,1];
      cids[((i-1)*bigk+j),2] <- tempcid[j,2];
      cids[((i-1)*bigk+j),3] <- tempcid[j,3];
      cids[((i-1)*bigk+j),4] <- tempcid[j,4];
    }
  }
  cid <- matrix(0,nrow=bigk, ncol=cnum+1);
  maxsse <- cids[1,4];
  bestindex <- 1;
  for(i in 1:ktimes)
  {
    if(cids[((i-1)*bigk+1),4] > maxsse)
    {
      maxsse <- cids[((i-1)*bigk+1),4];
      bestindex <- i;
    }
  }
  for(j in 1:bigk)
  {
    cid[j,1] <- cids[((bestindex-1)*bigk+j),1];
    cid[j,2] <- cids[((bestindex-1)*bigk+j),2];
    cid[j,3] <- cids[((bestindex-1)*bigk+j),3];
    cid[j,4] <- cids[((bestindex-1)*bigk+j),4];
  }
  newcid <- center_kmeans(A,bigk,times,cid);
  cid <- newcid;
  for(x in 0:(bigk-cluster-1))
  {  
    newcluster<-bigk - x;
    dist<-matrix(0,nrow=rnum,ncol=newcluster)
    for(i in 1:rnum){
      for(j in 1:newcluster){
        if((cid[j,1] == A[i,1]) && (cid[j,2] == A[i,2]) && (cid[j,3] == A[i,3]))
        {
          dist[i,j] <- 0
        }
        else
        {
          dist[i,j]<-distance_between(cid[j,1],cid[j,2],cid[j,3],A[i,1],A[i,2],A[i,3])
        }
        
      }  
      
    }
    for(i in 1:rnum)
    {
      for(j in 1:newcluster)
      {
        if(dist[i,j] == min(dist[i,]))
        {
          A[i,4] <- j;
        }
      }
      #A[i,cnum+1]<-which(dist[i,]==min(dist[i,]))
    }
    cdist<-matrix(0,nrow=newcluster,ncol=newcluster)
    cdistmin <- 99999999;
    cdmindex <- matrix(0,nrow=2,ncol=1)
    for(i in 1:newcluster)
    {
      for(j in 1:newcluster){
        if((cid[j,1] == cid[i,1]) && (cid[j,2] == cid[j,2]) && (cid[j,3] == cid[i,3]))
        {
          cdist[i,j] <- 0
        }
        else
        {
          cdist[i,j]<-distance_between(cid[j,1],cid[j,2],cid[j,3],cid[i,1],cid[i,2],cid[i,3])
          if(cdist[i,j] < cdistmin)
          {
            cdistmin <- cdist[i,j];
            cdmindex[1] <- i;
            cdmindex[2] <- j;
          }
        }
      }
    } 
    for(i in 1:rnum)
    {
      if(A[i,4] == cdmindex[2])
      {
        A[i,4] <- cdmindex[1];
      }
      else if(A[i,4] > cdmindex[2])
      {
        A[i,4] <- A[i,4] - 1;
      }
    }
    newx <- tapply(A[,1], A[,4], mean)
    newy <- tapply(A[,2], A[,4], mean)
    newz <- tapply(A[,3], A[,4], mean)
    cid<-matrix(0,nrow=(newcluster-1), ncol=4);
    for(j in 1:(newcluster-1)){
      cid[j,1]=newx[j]
      cid[j,2]=newy[j]
      cid[j,3]=newz[j]
      cid[j,4]<-0
    }
    
  }
  return(cid)
  #Write centers into XML element
  node4<-xmlNode("GeoCenters")
  for(i in 1:cluster){
    node4$children[[i]] <-xmlNode("GeoCenter",
                                 xmlNode("Longtitude",xmlText = c(cid[i,1])),
                                 xmlNode("Latitude",xmlText = c(cid[i,2])),
                                 xmlNode("Elevation",xmlText = c(cid[i,3])))
  }
  cat(saveXML(node4))
}

cluster <- 7
times<-15;
bigk <- 12;
row <- nrow(mydata)
A<-cbind(mydata$Latitude,mydata$Longitude,mydata$Elevation,matrix(0,nrow=row))
dimnames(A)<-list(1:row,c("Latitude","Longitude","Elevation","Cluster"))
newcid <-new_kmeans(A,cluster,times,bigk)
newcid
plot(A)
points(newcid,col = 1:cluster,pch = 2,cex = 4);

cidframe<-data.frame(newcid[,1],newcid[,2],newcid[,3])
names(cidframe)[1]<-"Latitude"
names(cidframe)[2]<-"Longitude"
names(cidframe)[3]<-"Elevation"

write.csv(cidframe ,file="f://CS513final//newkmeansResult.csv",row.names=FALSE )

