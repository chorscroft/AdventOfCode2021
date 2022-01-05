# Advent of Code Day 23

## DO NOT RUN THIS IT TAKES HOURS
## This will not work if a pod starts already at home as it assumes each pod will
## move once to the hallway then once to their home

#############
#...........#
###B#B#D#D###
  #C#C#A#A#
  #########

## Read in the data
pattern<-read.table("data/day23.txt",header=F,skip=2,nrow=2,comment.char = "@")

## Pull out the letters in each row
row1<-NULL
row2<-NULL
for(i in 1:nchar(pattern$V1[1])){
  if(substr(pattern$V1[1],i,i)!="#"){
    row1<-c(row1,substr(pattern$V1[1],i,i))
  }
}
for(i in 1:nchar(pattern$V1[2])){
  if(substr(pattern$V1[2],i,i)!="#"){
    row2<-c(row2,substr(pattern$V1[2],i,i))
  }
}

# Part 1
## Create a data frame with all relevant information about each pod:
## Its letter, where it starts, where it is trying to go (min and max y locations), how much it costs to move, how many moves it has made
data<-data.frame(pod=c(row1,row2),startx=c(4,6,8,10,4,6,8,10),starty=c(3,3,3,3,4,4,4,4),endx=NA,endy=3,endy2=4,move=NA,moves_taken=0)
for(i in 1:nrow(data)){
  if(data$pod[i]=="A"){
    data$endx[i]<-4
    data$move[i]<-1
  } else if(data$pod[i]=="B"){
    data$endx[i]<-6
    data$move[i]<-10
  } else if(data$pod[i]=="C"){
    data$endx[i]<-8
    data$move[i]<-100
  } else if(data$pod[i]=="D"){
    data$endx[i]<-10
    data$move[i]<-1000
  }
}

## Add columns with the pods' current locations
data$current_locx<-data$startx
data$current_locy<-data$starty

## number of pods
npods<-nrow(data)

## Locations of potential hallway stops
hallway<-matrix(c(2,2,
                  3,2,
                  5,2,
                  7,2,
                  9,2,
                  11,2,
                  12,2),byrow=T,ncol=2)

## Check if all pods are home
checkwin<-function(podrecord,npods){
  if(length(podrecord)==2*npods){
    return(T)
  } else {
    return(F)
  }
}

## Check if there is a clear path from the pod's current location to next location
clearPath<-function(x,y,x2,y2){
  if(x==x2 && y == y2){
    return(F)
  }
  ## move up
  while(y2<y){
    y<-y-1
    for (i in 1:npods){
      if(data$current_locy[i]==y && data$current_locx[i]==x){
        return(F)
      }
    }
  }
  ## move right
  while(x<x2){
    x<-x+1
    for (i in 1:npods){
      if(data$current_locy[i]==y && data$current_locx[i]==x){
        return(F)
      }
    }
  }
  ## move left
  while(x>x2){
    x<-x-1
    for (i in 1:npods){
      if(data$current_locy[i]==y && data$current_locx[i]==x){
        return(F)
      }
    }
  }
  ## move down
  while(y2>y){
    y<-y+1
    for (i in 1:npods){
      if(data$current_locy[i]==y && data$current_locx[i]==x){
        return(F)
      }
    }
  }
  return(T)
}

## Check if pod is home
podhome<-function(data,pod){
  if(length(podrecord)==0){
    return(F)
  } else {
    if(sum(podrecord==pod)==2){
      return(T)
    } else {
      return(F)
    }
  }
}

## Count how mnay pods of the same letter are already home
samepodshome<-function(data,pod,podrecord){
  temp<-which(data$pod==data$pod[pod])
  otherpod<-temp[which(temp!=pod)]
  counthome<-0
  for(i in 1:length(otherpod)){
    if(podhome(podrecord,otherpod[i])){
      counthome<-counthome+1
    }
  }
  return(counthome)
}

## Initialise record of pod moves, and the location they moved to
## Pods are recorded by row number in the data frame
## Locations are either the hallway number (position in hallway vector) or 0 if they are home
podrecord<-NULL
locationrecord<-NULL

## Initialise current pod and location to try
pod<-1
location<-1

## Initialise the record of the lowest cost move
lowestwin<-Inf

## Try moves until the lowest cost has been found
repeat{
  ## Assume pod hasn't moved
  moved<-F
  if (podhome(podrecord,pod)==F){
    ## If pod is not at home already, see if it has moved to the hallway yet
    if(data$moves_taken[pod]==0){
      ## If it hasn't moved at all, see if it can be moved into the hallway (to a particular hallway location)
      if(clearPath(data$current_locx[pod],data$current_locy[pod],hallway[location,1],hallway[location,2])){
        ## If there is a clear path to the current location being check, move it there
        ## Record number of steps to move to location
        data$moves_taken[pod]<-data$moves_taken[pod]+abs(data$current_locx[pod]-hallway[location,1])+abs(data$current_locy[pod]-hallway[location,2])
        ## Update current location
        data$current_locx[pod]<-hallway[location,1]
        data$current_locy[pod]<-hallway[location,2]
        ## Update the records
        podrecord<-c(podrecord,pod)
        locationrecord<-c(locationrecord,location)
        ## Record that pod has moved
        moved<-T
      }
    } else {
      ## If the pod is in the hallway, see if it can go home
      ## Count the number of pods with the same letter that are already home
      othershome<-samepodshome(data,pod,podrecord)
      if(clearPath(data$current_locx[pod],data$current_locy[pod],data$endx[pod],data$endy2[pod]-othershome)){
        ## If there is a clear path home, move it home
        ## Record number of steps to move to home
        data$moves_taken[pod]<-data$moves_taken[pod]+abs(data$current_locx[pod]-data$endx[pod])+abs(data$current_locy[pod]-(data$endy2[pod]-othershome))
        ## Update current location
        data$current_locx[pod]<-data$endx[pod]
        data$current_locy[pod]<-data$endy2[pod]-othershome
        ## Update the records
        podrecord<-c(podrecord,pod)
        locationrecord<-c(locationrecord,0)
        ## Record that pod has moved
        moved<-T
      }
    }
  }
  
  if(moved==F){
    ## If pod didn't move, try the next possible location
    location<-location+1
    if(location>7 || data$moves_taken[pod]>0){
      ## If there is no other location to try, or the pod cannot get home,
      ## try moving the next pod
      location<-1
      pod<-pod+1
      while(pod>npods){
        ## If you've run out of pods to try, undo last moves
        if(length(podrecord)==0){
          ## If you've searched all combinations, end the algorithm
          stop("finished search")
        }
        ## Get last moved pod and its location
        pod<-podrecord[length(podrecord)]
        location<-locationrecord[length(locationrecord)]
        if(location==0){
          ## If it was at home, put it back in the corridor
          temp<-which(podrecord==pod)
          oldloc<-locationrecord[temp[1]]
          data$moves_taken[pod]<-abs(data$startx[pod]-hallway[oldloc,1])+abs(data$starty[pod]-hallway[oldloc,2])
          data$current_locx[pod]<-hallway[oldloc,1]
          data$current_locy[pod]<-hallway[oldloc,2]
          ## Try next pod
          pod<-pod+1
          location<-1
        } else {
          ## If it was in the corridor, move it back to the start
          data$current_locx[pod]<-data$startx[pod]
          data$current_locy[pod]<-data$starty[pod]
          data$moves_taken[pod]<-0
          ## Try next location
          location<-location+1
          if(location>7){
            ## If you have tried all locations, go to next pod
            ## This section will loop if you have run out of pods to try too
            location<-1
            pod<-pod+1
          }
        }
        ## Update the records
        podrecord<-podrecord[-length(podrecord)]
        locationrecord<-locationrecord[-length(locationrecord)]
      }
    }
  } else {
    ## If pod moved, assess the cost so far
    cost<-sum(data$move*data$moves_taken)
    
    if (checkwin(podrecord,npods)==T | cost>lowestwin){
      ## Check if the pods are all home, or if the current moves is more than the
      ## lowest cost scenario found so far
      if (lowestwin>cost){
        ## If the pods are all home and the cost is the lowest found so far, record the new cost
        lowestwin<-cost
        ## Print the cost and the records
        print.default(lowestwin)
        print.default(podrecord)
        print.default(locationrecord)
        #if (cost==46721){
        #  stop("win")
        #}
      }
      ## remove last step in either case (win or cost has become too high to bother continuing)
      ## make pod greater than the number of pods to force while loop to run at least once
      pod<-npods+1
      while(pod>npods){
        ## undo last moves
        if(length(podrecord)==0){
          ## If you've searched all combinations, end the algorithm
          stop("finished search")
        }
        ## Get last moved pod and its location
        pod<-podrecord[length(podrecord)]
        location<-locationrecord[length(locationrecord)]
        if(location==0){
          ## If it was at home, put it back in the corridor
          temp<-which(podrecord==pod)
          oldloc<-locationrecord[temp[1]]
          data$moves_taken[pod]<-abs(data$startx[pod]-hallway[oldloc,1])+abs(data$starty[pod]-hallway[oldloc,2])
          data$current_locx[pod]<-hallway[oldloc,1]
          data$current_locy[pod]<-hallway[oldloc,2]
          ## Try next pod
          pod<-pod+1
          location<-1
        } else {
          ## If it was in the corridor, move it back to the start
          data$current_locx[pod]<-data$startx[pod]
          data$current_locy[pod]<-data$starty[pod]
          data$moves_taken[pod]<-0
          ## Try next location
          location<-location+1
          if(location>7){
            ## If you have tried all locations, go to next pod
            ## This section will loop if you have run out of pods to try too
            location<-1
            pod<-pod+1
          }
        }
        ## Update the records
        podrecord<-podrecord[-length(podrecord)]
        locationrecord<-locationrecord[-length(locationrecord)]
      }
      
    } else {
      ## If you haven't won yet and the current cost is still acceptable, 
      ## attempt to make a new move starting with pod 1
      pod<-1
      location<-1
    }
  }
}

# Part 2

#############
#...........#
###B#B#D#D###
  #D#C#B#A#
  #D#B#A#C#
  #C#C#A#A#
  #########

## Recreate data frame, and add extra rows for the new pods
data<-data.frame(pod=c(row1,row2),startx=c(4,6,8,10,4,6,8,10),starty=c(3,3,3,3,6,6,6,6),endx=NA,endy=3,endy2=6,move=NA,moves_taken=0)
data<-rbind(data,data.frame(pod="D",startx=4,starty=4,endx=NA,endy=3,endy2=6,move=NA,moves_taken=0))
data<-rbind(data,data.frame(pod="C",startx=6,starty=4,endx=NA,endy=3,endy2=6,move=NA,moves_taken=0))
data<-rbind(data,data.frame(pod="B",startx=8,starty=4,endx=NA,endy=3,endy2=6,move=NA,moves_taken=0))
data<-rbind(data,data.frame(pod="A",startx=10,starty=4,endx=NA,endy=3,endy2=6,move=NA,moves_taken=0))
data<-rbind(data,data.frame(pod="D",startx=4,starty=5,endx=NA,endy=3,endy2=6,move=NA,moves_taken=0))
data<-rbind(data,data.frame(pod="B",startx=6,starty=5,endx=NA,endy=3,endy2=6,move=NA,moves_taken=0))
data<-rbind(data,data.frame(pod="A",startx=8,starty=5,endx=NA,endy=3,endy2=6,move=NA,moves_taken=0))
data<-rbind(data,data.frame(pod="C",startx=10,starty=5,endx=NA,endy=3,endy2=6,move=NA,moves_taken=0))

for(i in 1:nrow(data)){
  if(data$pod[i]=="A"){
    data$endx[i]<-4
    data$move[i]<-1
  } else if(data$pod[i]=="B"){
    data$endx[i]<-6
    data$move[i]<-10
  } else if(data$pod[i]=="C"){
    data$endx[i]<-8
    data$move[i]<-100
  } else if(data$pod[i]=="D"){
    data$endx[i]<-10
    data$move[i]<-1000
  }
}

## Add columns with the pods' current locations
data$current_locx<-data$startx
data$current_locy<-data$starty

## number of pods
npods<-nrow(data)

## Initialise record of pod moves, and the location they moved to
## Pods are recorded by row number in the data frame
## Locations are either the hallway number (position in hallway vector) or 0 if they are home
podrecord<-NULL
locationrecord<-NULL

## Initialise current pod and location to try
pod<-1
location<-1

## Initialise the record of the lowest cost move
lowestwin<-Inf

## Try moves until the lowest cost has been found
repeat{
  ## Assume pod hasn't moved
  moved<-F
  if (podhome(podrecord,pod)==F){
    ## If pod is not at home already, see if it has moved to the hallway yet
    if(data$moves_taken[pod]==0){
      ## If it hasn't moved at all, see if it can be moved into the hallway (to a particular hallway location)
      if(clearPath(data$current_locx[pod],data$current_locy[pod],hallway[location,1],hallway[location,2])){
        ## If there is a clear path to the current location being check, move it there
        ## Record number of steps to move to location
        data$moves_taken[pod]<-data$moves_taken[pod]+abs(data$current_locx[pod]-hallway[location,1])+abs(data$current_locy[pod]-hallway[location,2])
        ## Update current location
        data$current_locx[pod]<-hallway[location,1]
        data$current_locy[pod]<-hallway[location,2]
        ## Update the records
        podrecord<-c(podrecord,pod)
        locationrecord<-c(locationrecord,location)
        ## Record that pod has moved
        moved<-T
      }
    } else {
      ## If the pod is in the hallway, see if it can go home
      ## Count the number of pods with the same letter that are already home
      othershome<-samepodshome(data,pod,podrecord)
      if(clearPath(data$current_locx[pod],data$current_locy[pod],data$endx[pod],data$endy2[pod]-othershome)){
        ## If there is a clear path home, move it home
        ## Record number of steps to move to home
        data$moves_taken[pod]<-data$moves_taken[pod]+abs(data$current_locx[pod]-data$endx[pod])+abs(data$current_locy[pod]-(data$endy2[pod]-othershome))
        ## Update current location
        data$current_locx[pod]<-data$endx[pod]
        data$current_locy[pod]<-data$endy2[pod]-othershome
        ## Update the records
        podrecord<-c(podrecord,pod)
        locationrecord<-c(locationrecord,0)
        ## Record that pod has moved
        moved<-T
      }
    }
  }
  
  if(moved==F){
    ## If pod didn't move, try the next possible location
    location<-location+1
    if(location>7 || data$moves_taken[pod]>0){
      ## If there is no other location to try, or the pod cannot get home,
      ## try moving the next pod
      location<-1
      pod<-pod+1
      while(pod>npods){
        ## If you've run out of pods to try, undo last moves
        if(length(podrecord)==0){
          ## If you've searched all combinations, end the algorithm
          stop("finished search")
        }
        ## Get last moved pod and its location
        pod<-podrecord[length(podrecord)]
        location<-locationrecord[length(locationrecord)]
        if(location==0){
          ## If it was at home, put it back in the corridor
          temp<-which(podrecord==pod)
          oldloc<-locationrecord[temp[1]]
          data$moves_taken[pod]<-abs(data$startx[pod]-hallway[oldloc,1])+abs(data$starty[pod]-hallway[oldloc,2])
          data$current_locx[pod]<-hallway[oldloc,1]
          data$current_locy[pod]<-hallway[oldloc,2]
          ## Try next pod
          pod<-pod+1
          location<-1
        } else {
          ## If it was in the corridor, move it back to the start
          data$current_locx[pod]<-data$startx[pod]
          data$current_locy[pod]<-data$starty[pod]
          data$moves_taken[pod]<-0
          ## Try next location
          location<-location+1
          if(location>7){
            ## If you have tried all locations, go to next pod
            ## This section will loop if you have run out of pods to try too
            location<-1
            pod<-pod+1
          }
        }
        ## Update the records
        podrecord<-podrecord[-length(podrecord)]
        locationrecord<-locationrecord[-length(locationrecord)]
      }
    }
  } else {
    ## If pod moved, assess the cost so far
    cost<-sum(data$move*data$moves_taken)
    
    if (checkwin(podrecord,npods)==T | cost>lowestwin){
      ## Check if the pods are all home, or if the current moves is more than the
      ## lowest cost scenario found so far
      if (lowestwin>cost){
        ## If the pods are all home and the cost is the lowest found so far, record the new cost
        lowestwin<-cost
        ## Print the cost and the records
        print.default(lowestwin)
        print.default(podrecord)
        print.default(locationrecord)
        #if (cost==46721){
        #  stop("win")
        #}
      }
      ## remove last step in either case (win or cost has become too high to bother continuing)
      ## make pod greater than the number of pods to force while loop to run at least once
      pod<-npods+1
      while(pod>npods){
        ## undo last moves
        if(length(podrecord)==0){
          ## If you've searched all combinations, end the algorithm
          stop("finished search")
        }
        ## Get last moved pod and its location
        pod<-podrecord[length(podrecord)]
        location<-locationrecord[length(locationrecord)]
        if(location==0){
          ## If it was at home, put it back in the corridor
          temp<-which(podrecord==pod)
          oldloc<-locationrecord[temp[1]]
          data$moves_taken[pod]<-abs(data$startx[pod]-hallway[oldloc,1])+abs(data$starty[pod]-hallway[oldloc,2])
          data$current_locx[pod]<-hallway[oldloc,1]
          data$current_locy[pod]<-hallway[oldloc,2]
          ## Try next pod
          pod<-pod+1
          location<-1
        } else {
          ## If it was in the corridor, move it back to the start
          data$current_locx[pod]<-data$startx[pod]
          data$current_locy[pod]<-data$starty[pod]
          data$moves_taken[pod]<-0
          ## Try next location
          location<-location+1
          if(location>7){
            ## If you have tried all locations, go to next pod
            ## This section will loop if you have run out of pods to try too
            location<-1
            pod<-pod+1
          }
        }
        ## Update the records
        podrecord<-podrecord[-length(podrecord)]
        locationrecord<-locationrecord[-length(locationrecord)]
      }
      
    } else {
      ## If you haven't won yet and the current cost is still acceptable, 
      ## attempt to make a new move starting with pod 1
      pod<-1
      location<-1
    }
  }
}






### IGNORE THIS ###
#pod<-11
#location<-3
#data<-read.table("data/day23.txt",header=T)
#data<-data[16:1,]
#podrecord<-c(15,6,10,7,11,7,6,8,8,1,2,11,10,5,5,13,14,9,9,2,1,16,3,3,12,12,4,4,16,15,14,13)
#podrecord<-17-podrecord
#locationrecord<-c(7,5,2,4,3,0,0,4,0,6,5,0,0,3,0,1,2,4,0,0,0,6,5,0,5,0,5,0,0,0,0,0)
#locationrecord[locationrecord>0]<-8-locationrecord[locationrecord>0]
