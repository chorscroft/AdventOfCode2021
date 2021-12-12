# Advent of Code Day 12
## NOTE: This code is very inefficient and takes ages to run!!

## Read in the data
data<-read.table("data/day12.txt",sep="-")

# Part 1
## Check if step is valid
isValidStep<-function(path,route){
  ## does next step start from current location
  if(path[length(path)]==route[1]){
    cave<-route[2]
  } else if(path[length(path)]==route[2]){
    cave<-route[1]
  } else {
    return(FALSE)
  }
  
  ## if next location is in capitals, is "end"
  ## or is a lower case location that hasn't been visitied
  ## then step is valid
  if (substr(cave,1,1) %in% LETTERS){
    return(TRUE)
  } else if (cave=="end"){
    return(TRUE)
  } else if (!(cave %in% path)){
    return(TRUE)
  }
  return(FALSE)
}

## recursive function to find paths
getPath<-function(path){
  for (i in 1:nrow(data)){
    if (isValidStep(path,data[i,])){
      ## if next step is valid, add to end of the path
      if(path[length(path)]==data$V1[i]){
        path<-c(path,data$V2[i])
      } else {
        path<-c(path,data$V1[i])
      }
      ## if path has reached the end, count it 
      ## and then remove the end of the path
      if(path[length(path)]=="end"){
        countpaths<<-countpaths+1
        path<-path[-length(path)]
      } else {
        ## otherwise continue along path
        path<-getPath(path)
      }
    }
  }
  ## once all cave connections have been checked, remove end of path
  path<-path[-length(path)]
  return(path)
}

## initialise count of paths
countpaths<-0

## run algorithm
getPath("start")

## output number of paths
countpaths

# Part 2
## Check if step is valid
isValidStep2<-function(path,route){
  ## does next step start from current location
  if(path[length(path)]==route[1]){
    cave<-route[2]
  } else if(path[length(path)]==route[2]){
    cave<-route[1]
  } else {
    return(FALSE)
  }
  
  ## if next location is in capitals or is "end" then it is valid.
  ## going back to "start" is not a valid step.
  ## lower case location is only valid if it has not been visited before,
  ## or if no small cave has been visited twice
  if (substr(cave,1,1) %in% LETTERS){
    return(TRUE)
  } else if (cave=="end"){
    return(TRUE)
  } else if (cave=="start"){
    return(FALSE)
  } else if (twiceVisited(path)==TRUE & !(cave %in% path)){
    return(TRUE)
  } else if (twiceVisited(path)==FALSE){
    return(TRUE)
  }
  return(FALSE)
}

## check if a small cave has been visited twice on the current path
twiceVisited<-function(path){
  uni<-unique(path)
  for(i in 1:length(uni)){
    if (uni[i] != "start" & uni[i] !="end" & substr(uni[i],1,1) %in% letters){
      if(sum(path==uni[i])==2){
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

## recursive function to find paths
getPath2<-function(path){
  for (i in 1:nrow(data)){
    if (isValidStep2(path,data[i,])){
      ## if next step is valid, add to end of the path
      if(path[length(path)]==data$V1[i]){
        path<-c(path,data$V2[i])
      } else {
        path<-c(path,data$V1[i])
      }
      ## if path has reached the end, count it 
      ## and then remove the end of the path
      if(path[length(path)]=="end"){
        countpaths<<-countpaths+1
        path<-path[-length(path)]
      } else {
        ## otherwise continue along path
        path<-getPath2(path)
      }
    }
  }
  ## once all cave connections have been checked, remove end of path
  path<-path[-length(path)]
  return(path)
}

## initialise count of paths
countpaths<-0

## run algorithm
getPath2("start")

## output number of paths
countpaths
