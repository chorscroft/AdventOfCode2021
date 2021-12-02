# Advent of Code Day 2

## Read in the data
data<-read.table("data/day02.txt",col.names = c("Command","X"))

# Part 1
## Initialise horizontal position and depth
horiz<-0
depth<-0

## For each command adjust horizontal position and depth
for(i in 1:nrow(data)){
  if (data$Command[i]=="forward"){
    horiz<-horiz+data$X[i]
  }
  else if (data$Command[i]=="down"){
    depth<-depth+data$X[i]
  }
  else if (data$Command[i]=="up"){
    depth<-depth-data$X[i]
  }
}
depth*horiz

# Part 2
## Initialise horizontal position, depth, and aim
horiz<-0
depth<-0
aim<-0

## For each command adjust horizontal position, depth, and aim
for(i in 1:nrow(data)){
  if (data$Command[i]=="forward"){
    horiz<-horiz+data$X[i]
    depth<-depth+aim*data$X[i]
  }
  else if (data$Command[i]=="down"){
    aim<-aim+data$X[i]
  }
  else if (data$Command[i]=="up"){
    aim<-aim-data$X[i]
  }
}
depth*horiz
