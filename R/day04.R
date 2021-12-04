# Advent of Code Day 4

## Read in the data
data<-read.table("data/day04.txt",skip=2)
numbers<-read.table("data/day04.txt",nrows=1,sep=",")

# Part 1
## Get size of bingo board
bb_size<-ncol(data)

## Get number of bingo boards
n_bb<-nrow(data)/bb_size

## Function to check if a bingo board has been completed
isComplete<-function(x){
  for (i in 1:n_bb){
    for (j in 1:bb_size){
      if (sum(x[i*bb_size+1-j,])==bb_size | sum(x[((i-1)*bb_size+1):(i*bb_size),j])==bb_size){
        return(i)
      }
    }
  }
  return(NA)
}

## Initialise matrix of blank bingo boards
checked<-matrix(F,nrow=n_bb*bb_size,ncol=bb_size)

## Call numbers until a board is complete
i<-0
while (is.na(isComplete(checked))){
  i<-i+1
  checked[data==numbers[1,i]]<-T
}
board<-isComplete(checked)

## Find the result
sum(data[((board-1)*bb_size+1):(board*bb_size),1:bb_size][checked[((board-1)*bb_size+1):(board*bb_size),1:bb_size]==F])*numbers[1,i]

# Part 2
## Function to check if all but one bingo boards have been completed
isAllButOneComplete<-function(x){
  complete<-rep(F,n_bb)
  for (i in 1:n_bb){
    for (j in 1:bb_size){
      if (sum(x[i*bb_size+1-j,])==bb_size | sum(x[((i-1)*bb_size+1):(i*bb_size),j])==bb_size){
        complete[i]<-T
      }
    }
  }
  if (sum(complete)==n_bb-1){
    return(which(complete==F))
  }
  return(NA)
}

## Function to check if all bingo boards have been completed
isAllComplete<-function(x){
  complete<-rep(F,n_bb)
  for (i in 1:n_bb){
    for (j in 1:bb_size){
      if (sum(x[i*bb_size+1-j,])==bb_size | sum(x[((i-1)*bb_size+1):(i*bb_size),j])==bb_size){
        complete[i]<-T
      }
    }
  }
  if (sum(complete)==n_bb){
    return(T)
  }
  return(NA)
}

## Initialise matrix of blank bingo boards
checked<-matrix(F,nrow=n_bb*bb_size,ncol=bb_size)

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
sum(data[((board-1)*bb_size+1):(board*bb_size),1:bb_size][checked[((board-1)*bb_size+1):(board*bb_size),1:bb_size]==F])*numbers[1,i]
