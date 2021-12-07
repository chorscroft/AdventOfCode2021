# Advent of Code Day 7

## Read in the data
data<-read.table("data/day07.txt",sep=",")

# Part 1
## initialise the fuel usage
minfuel<-999999999999999

## loop for each horizontal position in the data set
## record the fuel usage if it is smaller than the previous calculation
## if the fuel usage starts increasing again, stop
for(i in min(data):max(data)){
  temp<-sum(abs(data-i))
  if(temp<minfuel){
    minfuel<-temp
  } else if (i > min(data) & temp > minfuel){
    break
  }
}

## Return result
minfuel

# Part 2
## initialise the fuel usage
minfuel<-999999999999999

## Cost of fuel given the distance needed to travel
fuelcost<-function(x){
  cost<-sum(1:x)
  return(cost)
}

## loop for each horizontal position in the data set
## record the fuel usage if it is smaller than the previous calculation
## if the fuel usage starts increasing again, stop
for(i in min(data):max(data)){
  temp<-sum(sapply(abs(data-i),fuelcost))
  if(temp<minfuel){
    minfuel<-temp
  } else if (i > min(data) & temp > minfuel){
    break
  }
}

## Return result
minfuel