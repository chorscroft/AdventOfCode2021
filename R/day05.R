# Advent of Code Day 5

## Read in the data
data<-read.table("data/day05.txt")

## Convert into two matrices for each point
point1<-matrix(as.numeric(unlist(strsplit(data$V1,","))),ncol=2,byrow=T)
point2<-matrix(as.numeric(unlist(strsplit(data$V3,","))),ncol=2,byrow=T)

# Part 1
## Initialise region
region<-matrix(0,max(c(point1,point2)),max(c(point1,point2)))

## Increase the count at each point in the region if a straight line crosses it
for(i in 1:nrow(data)){
  if(point1[i,1]==point2[i,1]){
    region[point1[i,1],point1[i,2]:point2[i,2]]<-region[point1[i,1],point1[i,2]:point2[i,2]]+1
  } else if (point1[i,2]==point2[i,2]) {
    region[point1[i,1]:point2[i,1],point1[i,2]]<-region[point1[i,1]:point2[i,1],point1[i,2]]+1
  } 
}

## Return the number of points crossed by at least two lines
sum(region>=2)

# Part 2
## Initialise region
region<-matrix(0,max(c(point1,point2)),max(c(point1,point2)))

## Increase the count at each point in the region if a straight line crosses it
for(i in 1:nrow(data)){
  if(point1[i,1]==point2[i,1]){
    region[point1[i,1],point1[i,2]:point2[i,2]]<-region[point1[i,1],point1[i,2]:point2[i,2]]+1
  } else if (point1[i,2]==point2[i,2]) {
    region[point1[i,1]:point2[i,1],point1[i,2]]<-region[point1[i,1]:point2[i,1],point1[i,2]]+1
  } else {
    x<-point1[i,1]:point2[i,1]
    y<-point1[i,2]:point2[i,2]
    for (j in 1:length(x)){
      region[x[j],y[j]]<-region[x[j],y[j]]+1
    }
  }
}

## Return the number of points crossed by at least two lines
sum(region>=2)