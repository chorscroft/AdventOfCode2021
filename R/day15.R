# Advent of Code Day 15

## Read in the data
data<-read.fwf("data/day15.txt",rep(1,100))

# Part 1
## Function to find the minimum unvisited location
findMin<-function(tentative,visited){
  temptent<-tentative
  temptent[visited==T]<-99999999
  minloc<-which(temptent==min(temptent),arr.ind=T)
  return(minloc[1,])
}

## initialise visited matrix
visited<-matrix(F,nrow(data),ncol(data))

## initialise tentative shorted distance matrix
tentative<-matrix(99999,nrow(data),ncol(data))

## start in top left corner
tentative[1,1]<-0
currentNode<-c(1,1)

## while the current node is not the bottom right corner, run
## Dijkstra's algorithm
while(!(currentNode[1]==nrow(data) & currentNode[2]==nrow(data))){
  if (currentNode[1]>1){
    if(visited[currentNode[1]-1,currentNode[2]]==F){
      if (tentative[currentNode[1],currentNode[2]]+data[currentNode[1]-1,currentNode[2]]<tentative[currentNode[1]-1,currentNode[2]]){
        tentative[currentNode[1]-1,currentNode[2]]<-tentative[currentNode[1],currentNode[2]]+data[currentNode[1]-1,currentNode[2]]
      }
    }
  }
  if (currentNode[1]<nrow(data)){
    if(visited[currentNode[1]+1,currentNode[2]]==F){
      if (tentative[currentNode[1],currentNode[2]]+data[currentNode[1]+1,currentNode[2]]<tentative[currentNode[1]+1,currentNode[2]]){
        tentative[currentNode[1]+1,currentNode[2]]<-tentative[currentNode[1],currentNode[2]]+data[currentNode[1]+1,currentNode[2]]
      }
    }
  }
  if (currentNode[2]>1){
    if(visited[currentNode[1],currentNode[2]-1]==F){
      if (tentative[currentNode[1],currentNode[2]]+data[currentNode[1],currentNode[2]-1]<tentative[currentNode[1],currentNode[2]-1]){
        tentative[currentNode[1],currentNode[2]-1]<-tentative[currentNode[1],currentNode[2]]+data[currentNode[1],currentNode[2]-1]
      }
    }
  }
  if (currentNode[2]<nrow(data)){
    if(visited[currentNode[1],currentNode[2]+1]==F){
      if (tentative[currentNode[1],currentNode[2]]+data[currentNode[1],currentNode[2]+1]<tentative[currentNode[1],currentNode[2]+1]){
        tentative[currentNode[1],currentNode[2]+1]<-tentative[currentNode[1],currentNode[2]]+data[currentNode[1],currentNode[2]+1]
      }
    }
  }
  visited[currentNode[1],currentNode[2]]<-T
  currentNode<-findMin(tentative,visited)
}

## Get result
tentative[nrow(data),ncol(data)]


# Part 2
## create new data matrix
newdata<-matrix(0,nrow(data)*5,ncol(data)*5)
for(i in 0:4){
  for(j in 0:4){
    for(k in 1:nrow(data)){
      for(l in 1:nrow(data)){
        newdata[i*nrow(data)+k,j*nrow(data)+l]<-data[k,l]+i+j
        while (newdata[i*nrow(data)+k,j*nrow(data)+l]>9){
          newdata[i*nrow(data)+k,j*nrow(data)+l]<-newdata[i*nrow(data)+k,j*nrow(data)+l]-9
        }
      }
    }
  }
}

## Run Dijkstra's algorithm again with the new data
data<-newdata

visited<-matrix(F,nrow(data),ncol(data))
tentative<-matrix(9999999,nrow(data),ncol(data))
tentative[1,1]<-0
currentNode<-c(1,1)
while(!(currentNode[1]==nrow(data) & currentNode[2]==nrow(data))){
  if (currentNode[1]>1){
    if(visited[currentNode[1]-1,currentNode[2]]==F){
      if (tentative[currentNode[1],currentNode[2]]+data[currentNode[1]-1,currentNode[2]]<tentative[currentNode[1]-1,currentNode[2]]){
        tentative[currentNode[1]-1,currentNode[2]]<-tentative[currentNode[1],currentNode[2]]+data[currentNode[1]-1,currentNode[2]]
      }
    }
  }
  if (currentNode[1]<nrow(data)){
    if(visited[currentNode[1]+1,currentNode[2]]==F){
      if (tentative[currentNode[1],currentNode[2]]+data[currentNode[1]+1,currentNode[2]]<tentative[currentNode[1]+1,currentNode[2]]){
        tentative[currentNode[1]+1,currentNode[2]]<-tentative[currentNode[1],currentNode[2]]+data[currentNode[1]+1,currentNode[2]]
      }
    }
  }
  if (currentNode[2]>1){
    if(visited[currentNode[1],currentNode[2]-1]==F){
      if (tentative[currentNode[1],currentNode[2]]+data[currentNode[1],currentNode[2]-1]<tentative[currentNode[1],currentNode[2]-1]){
        tentative[currentNode[1],currentNode[2]-1]<-tentative[currentNode[1],currentNode[2]]+data[currentNode[1],currentNode[2]-1]
      }
    }
  }
  if (currentNode[2]<nrow(data)){
    if(visited[currentNode[1],currentNode[2]+1]==F){
      if (tentative[currentNode[1],currentNode[2]]+data[currentNode[1],currentNode[2]+1]<tentative[currentNode[1],currentNode[2]+1]){
        tentative[currentNode[1],currentNode[2]+1]<-tentative[currentNode[1],currentNode[2]]+data[currentNode[1],currentNode[2]+1]
      }
    }
  }
  visited[currentNode[1],currentNode[2]]<-T
  currentNode<-findMin(tentative,visited)
}

## Get result
tentative[nrow(data),ncol(data)]

