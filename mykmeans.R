rm(list=ls())
install.packages("libxml2", repos="http://xmlsoft.org/sources/",type="source")
install.packages("libiconv", repos="http://ftp.gnu.org/pub/gnu/libiconv/",type="source")
install.packages("XML")
#If install.packages("XML") can not install the package, can use the follow one.
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
  Csum<-9999999999
  times<-10
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
  node<-xmlNode("GeoCenters")
  for(i in 1:npoint){
    node$children[[i]] <-xmlNode("GeoCenter",
                                 xmlNode("Longtitude",xmlText = c(cid[i,1])),
                                 xmlNode("Latitude",xmlText = c(cid[i,2])),
                                 xmlNode("Elevation",xmlText = c(cid[i,3])))
  }
  cat(saveXML(node))
}

#rnum=nrow(mxyz)
cluster <- 7
row <- nrow(mydata)
A<-cbind(mydata$Latitude,mydata$Longitude,mydata$Elevation,matrix(0,nrow=row))
dimnames(A)<-list(1:row,c("Latitude","Longitude","Elevation","Cluster"))

cid <- geo_kmeans(A,cluster,10)

cid
plot(A)
points(cid,col = 1:cluster,pch = 2,cex = 4);

cidframe<-data.frame(cid[,1],cid[,2],cid[,3])
names(cidframe)[1]<-"Latitude"
names(cidframe)[2]<-"Longitude"
names(cidframe)[3]<-"Elevation"

write.csv(cidframe ,file="f://CS513final//kmeansRresult.csv",row.names=FALSE)
