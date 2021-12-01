# Advent of Code Day 1

## Read in the data
data<-read.table("data/day01.txt",col.names = "Depth")

# Part 1
## Initialise count
countIncrease<-0

## Count the number of increases
for (i in 2:nrow(data)){
  if (data$Depth[i]>data$Depth[i-1]){
    countIncrease<-countIncrease+1
  }
}

## Print result
countIncrease

#Part 2
## Initialise count
countIncrease<-0

## Set group size
groupSize<-3

## Count the number of increases
for (i in (1+groupSize):nrow(data)){
  if (sum(data$Depth[(i-groupSize+1):i])>sum(data$Depth[(i-groupSize):(i-1)])){
    countIncrease<-countIncrease+1
  }
}

## Print result
countIncrease