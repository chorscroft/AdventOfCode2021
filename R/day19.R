# Advent of Code Day 19
## TAKES ~3 HOURS TO RUN!!! ##

## Read in the data
data<-read.table("data/day19.txt",sep=":")

# Part 1
## Make a list of scanners, where each scanner contains a matrix
## with the locations of the beacons it can see
scanners<-vector("list",1)

## Initialise count of scanners
noscanners<-0

## Create a list where each element is a single scanner
## within each element is a matrix of the beacon locations that
## that scanner can see
for (i in 1:nrow(data)){
  if (substr(data$V1[i],1,3)=="---"){
    if(noscanners>0){
      scanners[[noscanners]]<-tempVec
    }
    noscanners<-noscanners+1
    tempVec<-NULL
  } else {
    tempVec<-rbind(tempVec,as.numeric(unlist(strsplit(data$V1[i],","))))
  }
}
scanners[[noscanners]]<-tempVec

## Initialise a data frame for recording the locations of the scanners
## in relation to the first scanner, which is in position (0,0,0)
scannerLocs<-data.frame(x=rep(NA,noscanners),y=rep(NA,noscanners),z=rep(NA,noscanners))
scannerLocs[1,]<-c(0,0,0)

## Make a list that contains the x, y, and z distances between beacons for each scanner
distances<-vector("list",noscanners)
for(scan in 1:noscanners){
  ## initialise distance matrix
  xdists<-matrix(0,nrow(scanners[[scan]]),nrow(scanners[[scan]]))
  ydists<-matrix(0,nrow(scanners[[scan]]),nrow(scanners[[scan]]))
  zdists<-matrix(0,nrow(scanners[[scan]]),nrow(scanners[[scan]]))
  
  for (i in 1:nrow(scanners[[scan]])){
    for (j in 1:nrow(scanners[[scan]])){
      ## for each pair of beacons calculate the distance
      xdists[i,j]<-scanners[[scan]][i,1]-scanners[[scan]][j,1]
      ydists[i,j]<-scanners[[scan]][i,2]-scanners[[scan]][j,2]
      zdists[i,j]<-scanners[[scan]][i,3]-scanners[[scan]][j,3]
    }
  }
  distances[[scan]]<-list(xdists=xdists,ydists=ydists,zdists=zdists)
}


## checks whether points are the same distance from each other given a
## particular rotation of the scanner
check<-function(x1,x2,y1,y2,z1,z2,combo){
  switch(combo,
         return(x1==x2 && y1==y2 && z1==z2),
         return(x1==x2 && y1==z2 && z1==-y2),
         return(x1==x2 && y1==-y2 && z1==-z2),
         return(x1==x2 && y1==-z2 && z1==y2),
         
         return(x1==y2 && y1==z2 && z1==x2),
         return(x1==y2 && y1==x2 && z1==-z2),
         return(x1==y2 && y1==-z2 && z1==-x2),
         return(x1==y2 && y1==-x2 && z1==z2),
         
         return(x1==z2 && y1==x2 && z1==y2),
         return(x1==z2 && y1==y2 && z1==-x2),
         return(x1==z2 && y1==-x2 && z1==-y2),
         return(x1==z2 && y1==-y2 && z1==x2),
         
         return(x1==-x2 && y1==z2 && z1==y2),
         return(x1==-x2 && y1==y2 && z1==-z2),
         return(x1==-x2 && y1==-z2 && z1==-y2),
         return(x1==-x2 && y1==-y2 && z1==z2),
         
         return(x1==-y2 && y1==x2 && z1==z2),
         return(x1==-y2 && y1==z2 && z1==-x2),
         return(x1==-y2 && y1==-x2 && z1==-z2),
         return(x1==-y2 && y1==-z2 && z1==x2),
         
         return(x1==-z2 && y1==y2 && z1==x2),
         return(x1==-z2 && y1==x2 && z1==-y2),
         return(x1==-z2 && y1==-y2 && z1==-x2),
         return(x1==-z2 && y1==-x2 && z1==y2)
  )
}
## finds matching beacons between two scanners
getMatchingBeacons<-function(distances,scan1,scan2){
  for(combo in 1:24){
    matchingBeacons1<-NULL
    matchingBeacons2<-NULL
    for(i in 1:nrow(distances[[scan1]]$xdists)){
      for(j in 1:nrow(distances[[scan1]]$xdists)){
        for(k in 1:nrow(distances[[scan2]]$xdists)){
          for(l in 1:nrow(distances[[scan2]]$xdists)){
            if (i!=j && k!=l){
              if(check(distances[[scan1]]$xdists[i,j],distances[[scan2]]$xdists[k,l],distances[[scan1]]$ydists[i,j],distances[[scan2]]$ydists[k,l],distances[[scan1]]$zdists[i,j],distances[[scan2]]$zdists[k,l],combo)){
                matchingBeacons1<-c(matchingBeacons1,i,j)
                matchingBeacons2<-c(matchingBeacons2,k,l)
              }
            }
          }
        }
      }
    }
    if (length(unique(matchingBeacons1))>=12){
      matchingBeacons<-getMatches(matchingBeacons1,matchingBeacons2)
      return(matchingBeacons)
    }
  }
  return(NULL)
}
## sorts the matching beacons into pairs that directly correspond
getMatches<-function(matchingBeacons1,matchingBeacons2){
  uniq1<-unique(matchingBeacons1)
  uniq2<-unique(matchingBeacons2)
  matched<-matrix(T,length(uniq1),length(uniq2))
  for(i in seq(1,length(matchingBeacons1)-1,2)){
    matched[which(matchingBeacons1[i]==uniq1),which(matchingBeacons2[i] != uniq2 & matchingBeacons2[i+1] != uniq2)]<-F
    matched[which(matchingBeacons1[i+1]==uniq1),which(matchingBeacons2[i] != uniq2 & matchingBeacons2[i+1] != uniq2)]<-F
  }
  while(sum(matched)!=12){
    for(i in 1:nrow(matched)){
      if(sum(matched[i,])==1){
        matched[-i,which(matched[i,]==T)]<-F
      }
    }
    for(i in 1:ncol(matched)){
      if(sum(matched[,i])==1){
        matched[which(matched[,i]==T),-i]<-F
      }
    }
  }
  matched<-rbind(uniq1,uniq2[apply(matched,1,function(x)which(x==T))])
}
## gets the location of the scanner in relation to scanner 1. Also gets the order of
## x, y and z in relation to scanner 1, and the orientation
getScannerLoc<-function(scan1,scan2,matchingBeacons,scanners){
  x1<-scanners[[scan1]][matchingBeacons[1,],1]
  x2<-scanners[[scan2]][matchingBeacons[2,],1]
  y1<-scanners[[scan1]][matchingBeacons[1,],2]
  y2<-scanners[[scan2]][matchingBeacons[2,],2]
  z1<-scanners[[scan1]][matchingBeacons[1,],3]
  z2<-scanners[[scan2]][matchingBeacons[2,],3]
  
  xyzcheck<-cbind(x2,y2,z2)
  
  orders<-matrix(c(1,2,3,1,3,2,2,1,3,2,3,1,3,1,2,3,2,1),ncol=3,byrow = T)
  multiplys<-matrix(c(1,1,1,1,1,-1,1,-1,1,1,-1,-1,-1,1,1,-1,1,-1,-1,-1,1,-1,-1,-1),ncol=3,byrow=T)
  o<-1
  m<-1
  
  while(length(unique(cbind(x1,y1,z1)-xyzcheck[,orders[o,]]*rep(multiplys[m,],each=nrow(xyzcheck))))>3){
    m<-m+1
    if(m ==9){
      m<-1
      o<-o+1
    }
    if(o==7){
      stop()
    }
  }
  
  temp<-cbind(x1,y1,z1)-xyzcheck[,orders[o,]]*rep(multiplys[m,],each=nrow(xyzcheck))
  return(c(temp[1,1],temp[1,2],temp[1,3],orders[o,],multiplys[m,]))
}
## Finds the locations of the beacons the scanner sees in relation to scanner 1
findBeaconLocs<-function(scan2,scannerLoc,checkno,scanners){
  order<-scannerLoc[4:6]
  multiply<-scannerLoc[7:9]
  return(rep(scannerLoc[1:3],each=nrow(scanners[[scan2]]))+scanners[[scan2]][,order]*rep(multiply,each=nrow(scanners[[scan2]])))
}


## matrix to see if a pair of scanners has been checked previousl for matching beacons
checked<-matrix(F,noscanners,noscanners)

## loop until all scanners have been located in relation to scanner 1
while(sum(is.na(scannerLocs$x))>0){
  ## loop over each scanner starting with scanner 1
  for (scan1 in 1:noscanners){
    for (scan2 in 2:noscanners){
      ## only check if the first scanner has been located and the second hasn't
      if(scan1 != scan2 && checked[scan1,scan2]==F && !is.na(scannerLocs$x[scan1]) && is.na(scannerLocs$x[scan2])){
        ## record that this pair has been checked
        if(checked[scan1,scan2]==F){
          checked[scan1,scan2]<-T
        }
        print.default(c(scan1,scan2))
        
        ## find overlapping beacons
        matchingBeacons<-getMatchingBeacons(distances,scan1,scan2)
        ## if there are at least 12 overlapping find locations in relation to scanner 1
        if (!is.null(matchingBeacons)){
          ## get location of scanner in relation to scanner 1, and orientation
          scannerLoc<-getScannerLoc(scan1,scan2,matchingBeacons,scanners)
          scannerLocs[scan2,]<-scannerLoc[1:3]
          ## find locations in relation to scanner 1
          newbeacons<-findBeaconLocs(scan2,scannerLoc,checkno,scanners)
          ## overwrite scanner data with beacon locations in relation to scanner 1
          scanners[[scan2]]<-newbeacons
        }
      }
    }
  }
}

## get all the beacons
allBeacons<-scanners[[1]]
for (i in 2:length(scanners)){
  allBeacons<-rbind(allBeacons,scanners[[i]])
}
## only conider unique beacons
uniqueBeacons<-unique(allBeacons)
## output the number of beacons
nrow(uniqueBeacons)


# Part 2
## initialise the maximum Manhattan distance
maxdist<-0
## loop over each pair of scanners
for(i in 1:(noscanners-1)){
  for(j in (i+1):noscanners){
    ## get Manhattan distance
    manhat<-abs(scannerLocs[i,1]-scannerLocs[j,1])+abs(scannerLocs[i,2]-scannerLocs[j,2])+abs(scannerLocs[i,3]-scannerLocs[j,3])
    ## if it is the largest seen so far, record it
    if (manhat>maxdist){
      maxdist<-manhat
    }
  }
}

## output the largest Manhattan distance
maxdist


timestamp()



## checks whether points are the same distance from each other given a
## particular rotation of the scanner
check<-function(x1,x2,y1,y2,z1,z2,combo){
  switch(combo,
         return(x1==x2 && y1==y2 && z1==z2),
         return(x1==x2 && y1==z2 && z1==-y2),
         return(x1==x2 && y1==-y2 && z1==-z2),
         return(x1==x2 && y1==-z2 && z1==y2),
         
         return(x1==y2 && y1==z2 && z1==x2),
         return(x1==y2 && y1==x2 && z1==-z2),
         return(x1==y2 && y1==-z2 && z1==-x2),
         return(x1==y2 && y1==-x2 && z1==z2),
         
         return(x1==z2 && y1==x2 && z1==y2),
         return(x1==z2 && y1==y2 && z1==-x2),
         return(x1==z2 && y1==-x2 && z1==-y2),
         return(x1==z2 && y1==-y2 && z1==x2),
         
         return(x1==-x2 && y1==z2 && z1==y2),
         return(x1==-x2 && y1==y2 && z1==-z2),
         return(x1==-x2 && y1==-z2 && z1==-y2),
         return(x1==-x2 && y1==-y2 && z1==z2),
         
         return(x1==-y2 && y1==x2 && z1==z2),
         return(x1==-y2 && y1==z2 && z1==-x2),
         return(x1==-y2 && y1==-x2 && z1==-z2),
         return(x1==-y2 && y1==-z2 && z1==x2),
         
         return(x1==-z2 && y1==y2 && z1==x2),
         return(x1==-z2 && y1==x2 && z1==-y2),
         return(x1==-z2 && y1==-y2 && z1==-x2),
         return(x1==-z2 && y1==-x2 && z1==y2)
  )
}
## finds matching beacons between two scanners
getMatchingBeacons<-function(distances,scan1,scan2){
  for(combo in 1:24){
    matchingBeacons1<-NULL
    matchingBeacons2<-NULL
    for(i in 1:nrow(distances[[scan1]]$xdists)){
      for(j in 1:i){
        for(k in 1:nrow(distances[[scan2]]$xdists)){
          for(l in 1:k){
            if (i!=j && k!=l){
              if(check(distances[[scan1]]$xdists[i,j],distances[[scan2]]$xdists[k,l],distances[[scan1]]$ydists[i,j],distances[[scan2]]$ydists[k,l],distances[[scan1]]$zdists[i,j],distances[[scan2]]$zdists[k,l],combo)){
                matchingBeacons1<-c(matchingBeacons1,i,j)
                matchingBeacons2<-c(matchingBeacons2,k,l)
              }
            }
          }
        }
      }
    }
    if (length(unique(matchingBeacons1))>=12){
      matchingBeacons<-getMatches(matchingBeacons1,matchingBeacons2)
      return(matchingBeacons)
    }
  }
  return(NULL)
}