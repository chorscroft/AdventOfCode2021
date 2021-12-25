# Advent of Code Day 25

## Read in the data
data<-read.fwf("data/day25.txt",rep(1,139),colClasses="character")

# Part 1
## Initialise step count
countsteps<-0

## create new data frame for overwriting
newdata<-data

## do steps until no moves are made
repeat{
  ## initialise move count
  moves<-0
  
  ## east moving sea cucumbers
  ## initialise matrix to record if the sea cucumber will move
  checkifmove<-matrix(F,nrow(newdata),ncol(newdata))
  ## for each location, check if a ">" is there
  for(i in 1:nrow(newdata)){
    for(j in 1:ncol(newdata)){
      if(newdata[i,j]==">"){
        ## if there is a space to the right, record it
        if (j<ncol(newdata)){
          if(newdata[i,j+1]=="."){
            checkifmove[i,j]<-T
          } 
        } else {
          if(newdata[i,1]=="."){
            checkifmove[i,j]<-T
          } 
        }
      }
    }
  }
  
  ## move all the east sea cucumbers
  for(i in 1:nrow(newdata)){
    for(j in 1:ncol(newdata)){
      if(checkifmove[i,j]){
        if (j<ncol(newdata)){
            newdata[i,j+1]<-newdata[i,j]
            newdata[i,j]<-"."
        } else {
            newdata[i,1]<-newdata[i,j]
            newdata[i,j]<-"."
        }
      }
    }
  }  
  ## record how many moves were made
  moves<-moves+sum(checkifmove)
  
  
  ## south moving sea cucumbers
  ## initialise matrix to record if the sea cucumber will move
  checkifmove<-matrix(F,nrow(newdata),ncol(newdata))
  ## for each location, check if a "v" is there
  for(i in 1:nrow(newdata)){
    for(j in 1:ncol(newdata)){
      if(newdata[i,j]=="v"){
        ## if there is a space below, record it
        if (i<nrow(newdata)){
          if(newdata[i+1,j]=="."){
            checkifmove[i,j]<-T
          } 
        } else {
          if(newdata[1,j]=="."){
            checkifmove[i,j]<-T
          } 
        }
      }
    }
  }
  
  ## move all the south sea cucumbers
  for(i in 1:nrow(newdata)){
    for(j in 1:ncol(newdata)){
      if(checkifmove[i,j]){
        if (i<nrow(newdata)){
            newdata[i+1,j]<-newdata[i,j]
            newdata[i,j]<-"."
        } else {
            newdata[1,j]<-newdata[i,j]
            newdata[i,j]<-"."
        }
      }
    }
  }
  ## record how many moves were made
  moves<-moves+sum(checkifmove)
  
  ## iterate count of steps 
  countsteps<-countsteps+1
  
  ## if no moves were made this step, end the loop
  if (moves==0){
    break
  }

}

## Output the number of steps
countsteps

# Part 2
## Merry Christmas!!!
