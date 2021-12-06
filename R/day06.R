# Advent of Code Day 6

## Read in the data
data<-read.table("data/day06.txt",sep=",")

# Part 1
## Get the initial counts for each age of fish
init_counts<-as.numeric(sapply(0:8,function(x)sum(data==x)))

## Define function for interating x days
## Each day, the age of fish gets one smaller
## Fish who were on 0 reset to 6 days (index 7)
## New fish are generated at 8 days (index 9)
iterateFish<-function(counts,days){
  for (i in 1:days){
    newFish<-counts[1]
    counts[1:8]<-counts[2:9]
    counts[9]<-newFish
    counts[7]<-counts[7]+newFish
  }
  return(sum(counts))
}

## Use function for 80 days (stop R using scientific notation)
options(scipen = 999)
iterateFish(init_counts,80)

# Part 2
## Use function for 256 days
iterateFish(init_counts,256)
