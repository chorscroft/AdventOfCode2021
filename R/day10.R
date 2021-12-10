# Advent of Code Day 10

## Read in the data
data<-read.table("data/day10.txt")

# Part 1
## Initialise vector of corrupt symbols
corrupt<-NULL

## Check if each line is corrupt
for (i in 1:nrow(data)){
  ## Split line into individual characters
  myLine<-unlist(strsplit(data$V1[i],""))
  ## assume line is not corrupt
  uncorrupt<-TRUE
  ## initialise vector tracking what brackets are open
  tempvec<-NULL
  ## loop over each character until the end of the line or corruption is found
  j<-1
  while (uncorrupt==TRUE && j<= length(myLine)){
    ## if it is an open bracket, store in tempvec
    if(myLine[j] %in% c("(","{","[","<")){
      tempvec<-c(tempvec,myLine[j])
    } else {
      ## if it is a close bracket, check it is valid or corrupt
      if (myLine[j] %in% c(")","}","]",">") && length(tempvec)==0){
        uncorrupt<-FALSE
        corrupt<-c(corrupt,myLine[j])
      } else if (myLine[j]==">" && tempvec[length(tempvec)]!="<"){
        uncorrupt<-FALSE
        corrupt<-c(corrupt,myLine[j])
      } else if(myLine[j]==")" && tempvec[length(tempvec)]!="("){
        uncorrupt<-FALSE
        corrupt<-c(corrupt,myLine[j])
      } else if(myLine[j]=="}" && tempvec[length(tempvec)]!="{"){
        uncorrupt<-FALSE
        corrupt<-c(corrupt,myLine[j])
      } else if (myLine[j]=="]" && tempvec[length(tempvec)]!="["){
        uncorrupt<-FALSE
        corrupt<-c(corrupt,myLine[j])
      } else {
        ## if it is valid, remove open bracket from vector
        tempvec<-tempvec[-length(tempvec)]
      }
    }
    j<-j+1
  }
}

## Point system:
## ): 3 points
## ]: 57 points
## }: 1197 points
## >: 25137 points

## initialise sum for syntax error score
mysum<-0

## Add score for each corrupt symbol
for(i in 1:length(corrupt)){
  if (corrupt[i]==")"){
    mysum<-mysum+3
  } else if (corrupt[i]=="]"){
    mysum<-mysum+57
  } else if (corrupt[i]=="}"){
    mysum<-mysum+1197
  }else if (corrupt[i]==">"){
    mysum<-mysum+25137
  }
}

## output final value
mysum


# Part 2
## initialise list of incomplete lines
incomplete<-list()

## Check each line for corruption or incompleteness
for (i in 1:nrow(data)){
  ## Split line into individual characters
  myLine<-unlist(strsplit(data$V1[i],""))
  ## assume line is not corrupt
  uncorrupt<-TRUE
  ## initialise vector tracking what brackets are open
  tempvec<-NULL
  ## loop over each character until the end of the line or corruption is found
  j<-1
  while (uncorrupt==TRUE && j<= length(myLine)){
    ## if it is an open bracket, store in tempvec
    if(myLine[j] %in% c("(","{","[","<")){
      tempvec<-c(tempvec,myLine[j])
    } else {
      ## if it is a close bracket, check it is valid or corrupt
      if (myLine[j] %in% c(")","}","]",">") && length(tempvec)==0){
        uncorrupt<-FALSE
        corrupt<-c(corrupt,myLine[j])
      } else if (myLine[j]==">" && tempvec[length(tempvec)]!="<"){
        uncorrupt<-FALSE
        corrupt<-c(corrupt,myLine[j])
      } else if(myLine[j]==")" && tempvec[length(tempvec)]!="("){
        uncorrupt<-FALSE
        corrupt<-c(corrupt,myLine[j])
      } else if(myLine[j]=="}" && tempvec[length(tempvec)]!="{"){
        uncorrupt<-FALSE
        corrupt<-c(corrupt,myLine[j])
      } else if (myLine[j]=="]" && tempvec[length(tempvec)]!="["){
        uncorrupt<-FALSE
        corrupt<-c(corrupt,myLine[j])
      } else {
        ## if it is valid, remove open bracket from vector
        tempvec<-tempvec[-length(tempvec)]
      }
    }
    j<-j+1
  }
  ## if it is incomplete, store the left over open bracket in a list
  if (uncorrupt==TRUE && length(tempvec)>0){
    incomplete[[length(incomplete)+1]]<-tempvec
  }
  
}

## Point system:
## ): 1 point
## ]: 2 points
## }: 3 points
## >: 4 points

## initialise final sum
sums<-rep(0,length(incomplete))
## for each incomplete line, calculate the score
for(i in 1:length(incomplete)){
  tempsum<-0
  ## calcualte score from the last open bracket to the first
  for(j in length(incomplete[[i]]):1){
    tempsum<-tempsum*5
    if(incomplete[[i]][j]=="("){
      tempsum<-tempsum+1
    } else if(incomplete[[i]][j]=="["){
      tempsum<-tempsum+2
    } else if(incomplete[[i]][j]=="{"){
      tempsum<-tempsum+3
    } else if(incomplete[[i]][j]=="<"){
      tempsum<-tempsum+4
    }
  }
  sums[i]<-tempsum
}

## output final value (stop R using scientific notation)
options(scipen=999)
sort(sums)
