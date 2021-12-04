# Advent of Code Day 3

## Read in the data
data<-read.table("data/day04.txt",skip=2)
numbers<-read.table("data/day04.txt",nrows=1,sep=",")

# Part 1
## Function to check if a bingo board has been completed
isComplete<-function(x){
  for (i in 1:100){
    if (sum(x[i*5-4,])==5|sum(x[i*5-3,])==5|sum(x[i*5-2,])==5|sum(x[i*5-1,])==5|sum(x[i*5,])==5){
      return(i)
    } else if (sum(x[(i*5-4):(i*5),1])==5|sum(x[(i*5-4):(i*5),2])==5|sum(x[(i*5-4):(i*5),3])==5|sum(x[(i*5-4):(i*5),4])==5|sum(x[(i*5-4):(i*5),5])==5){
      return(i)
    }
  }
  return(NA)
}

## Initialise matrix of blank bingo boards
checked<-matrix(F,nrow=500,ncol=5)

## Call numbers until a board is complete
i<-0
while (is.na(isComplete(checked))){
  i<-i+1
  checked[data==numbers[1,i]]<-T
}
board<-isComplete(checked)

## Find the result
sum(data[(board*5-4):(board*5),1:5][checked[(board*5-4):(board*5),1:5]==F])*numbers[1,i]

# Part 2
## Function to check if all but one bingo boards have been completed
isAllButOneComplete<-function(x){
  complete<-rep(F,100)
  for (i in 1:100){
    if (sum(x[i*5-4,])==5|sum(x[i*5-3,])==5|sum(x[i*5-2,])==5|sum(x[i*5-1,])==5|sum(x[i*5,])==5){
      complete[i]<-T
    } else if (sum(x[(i*5-4):(i*5),1])==5|sum(x[(i*5-4):(i*5),2])==5|sum(x[(i*5-4):(i*5),3])==5|sum(x[(i*5-4):(i*5),4])==5|sum(x[(i*5-4):(i*5),5])==5){
      complete[i]<-T
    } 
  }
  if (sum(complete)==99){
    return(which(complete==F))
  }
  return(NA)
}

## Function to check if all bingo boards have been completed
isAllComplete<-function(x){
  complete<-rep(F,100)
  for (i in 1:100){
    if (sum(x[i*5-4,])==5|sum(x[i*5-3,])==5|sum(x[i*5-2,])==5|sum(x[i*5-1,])==5|sum(x[i*5,])==5){
      complete[i]<-T
    } else if (sum(x[(i*5-4):(i*5),1])==5|sum(x[(i*5-4):(i*5),2])==5|sum(x[(i*5-4):(i*5),3])==5|sum(x[(i*5-4):(i*5),4])==5|sum(x[(i*5-4):(i*5),5])==5){
      complete[i]<-T
    } 
  }
  if (sum(complete)==100){
    return(T)
  }
  return(NA)
}

## Initialise matrix of blank bingo boards
checked<-matrix(F,nrow=500,ncol=5)

## Call numbers until all but one board is complete
i<-0
while (is.na(isAllButOneComplete(checked))){
  i<-i+1
  checked[data==numbers[1,i]]<-T
}

## record number of last board
board<-isAllButOneComplete(checked)

## Continue calling numbers until all boards are complete
while (is.na(isAllComplete(checked))){
  i<-i+1
  checked[data==numbers[1,i]]<-T
}

## Find the result
sum(data[(board*5-4):(board*5),1:5][checked[(board*5-4):(board*5),1:5]==F])*numbers[1,i]
