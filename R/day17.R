# Advent of Code Day 17

## Read in the data
data<-read.table("data/day17.txt")

## Extract x and y limits of the target area
targetx<-c(as.numeric(substr(data$V3,3,which(unlist(strsplit(data$V3,""))==".")[1]-1)),
           as.numeric(substr(data$V3,which(unlist(strsplit(data$V3,""))==".")[2]+1,which(unlist(strsplit(data$V3,""))==",")[1]-1)))
targety<-c(as.numeric(substr(data$V4,3,which(unlist(strsplit(data$V4,""))==".")[1]-1)),
           as.numeric(substr(data$V4,which(unlist(strsplit(data$V4,""))==".")[2]+1,nchar(data$V4))))

# Part 1
## initialise maximum y position
totmaxy<-0

## loop over a range of possible velocities
for (i in 1:targetx[2]){
  for (j in c(1:1000)){
    ## initialise position
    pos<-c(0,0)
    ## initialise velocity
    velocity<-c(i,j)
    ## assume it isn't going to hit the target
    win<-F
    ## initialise record of maximum height
    maxy<-0
    ## launch the probe and calculate each step until it hits the target or
    ## it definitely will not hit the target
    repeat{
      pos<-pos+velocity
      
      ## record maximum y position
      if (pos[2]>maxy){
        maxy<-pos[2]
      }
      
      ## If it hits the target then record this as a win and stop looping
      if (pos[1]>=targetx[1] && pos[1]<=targetx[2] && pos[2]>=targety[1] && pos[2]<=targety[2]){
        win<-T
        break
      ## If it definitely will not hit the target then stop looping
      } else if (pos[2]< targety[1] || pos[1]>targetx[2] || (pos[1]<targetx[1] & velocity[1]<=0)){
        break
      }
      
      ## change the x velocity for the next step
      if(velocity[1]>0){
        velocity[1]<-velocity[1]-1
      } else if (velocity[1]<0){
        velocity[1]<-velocity[1]+1
      }
      
      ## change the y velocity for the next step
      velocity[2]<-velocity[2]-1
    }
    ## If the target was hit, record the max y if it is the highest seen so far
    if (win && maxy > totmaxy){
      totmaxy<-maxy
    }
  }
}

## Return the maximum y possible
totmaxy

# Part 2
## Initialise the count of possible velocities that hit the target
countwins<-0

## Loop over a range of possible velocities
for (i in 1:targetx[2]){
  for (j in c(targety[1]:1000)){
    ## initialise position
    pos<-c(0,0)
    ## initialise velocity
    velocity<-c(i,j)
    ## assume it isn't going to hit the target
    win<-F
    ## launch the probe and calculate each step until it hits the target or
    ## it definitely will not hit the target
    repeat{
      pos<-pos+velocity
      
      ## If it hits the target then record this as a win and stop looping
      if (pos[1]>=targetx[1] && pos[1]<=targetx[2] && pos[2]>=targety[1] && pos[2]<=targety[2]){
        win<-T
        break
        ## If it definitely will not hit the target then stop looping
      } else if (pos[2]< targety[1] || pos[1]>targetx[2] || (pos[1]<targetx[1] & velocity[1]<=0)){
        break
      }
      
      ## change the x velocity for the next step
      if(velocity[1]>0){
        velocity[1]<-velocity[1]-1
      } else if (velocity[1]<0){
        velocity[1]<-velocity[1]+1
      }
      
      ## change the y velocity for the next step
      velocity[2]<-velocity[2]-1
    }
    ## If the target was hit then count this as a win
    if (win){
      countwins<-countwins+1
    }
  }
}

## Return the number of times the target was hit
countwins
