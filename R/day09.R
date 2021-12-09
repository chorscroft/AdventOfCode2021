# Advent of Code Day 9

## Read in the data
data<-read.fwf("data/day09.txt",rep(1,100))

## Add a border of 9s around the data
data<-cbind(rep(9,100),data,rep(9,100))
data<-rbind(rep(9,102),data,rep(9,102))

# Part 1
## Find the low points and record their heights
heights<-NULL
for(i in 2:101){
  for (j in 2:101){
      if (data[i,j]< data[i-1,j] & data[i,j]< data[i,j-1] & data[i,j]< data[i+1,j] & data[i,j]< data[i,j+1] ){
        heights<-c(heights,data[i,j])
      }
    }
}

## Add one to the heights and sum
heights<-heights+1
sum(heights)

# Part 2
## Define basin data frame and set all 9s to 999
basin<-matrix(0,102,102)
basin[data==9]<-999

## Function to find adjacent locations and number them
numberBasin<-function(i,j,number,basin){
  if (basin[i+1,j]==0){
    basin[i+1,j]<-number
    basin<-numberBasin(i+1,j,number,basin)
  }
  if (basin[i-1,j]==0){
    basin[i-1,j]<-number
    basin<-numberBasin(i-1,j,number,basin)
  }
  if (basin[i,j+1]==0){
    basin[i,j+1]<-number
    basin<-numberBasin(i,j+1,number,basin)
  }
  if (basin[i,j-1]==0){
    basin[i,j-1]<-number
    basin<-numberBasin(i,j-1,number,basin)
  }
  return(basin)
}

## Number each basin seperately. Call recursive function to find 
## all points in the basin.
number<-1
for(i in 1:102){
  for (j in 1:102){
    ## If lcation isn't numbered, assign it to the next basin and find all 
    ## other locations in the basin
    if (basin[i,j]==0){
      basin[i,j]<-number
      basin<-numberBasin(i,j,number,basin)
      number<-number+1
    }
  }
}

## Get the size of the basins
basin_size<-table(basin)

## Remove basin 999
basin_size<-basin_size[-length(basin_size)]

## Sort the basins by size (largest first)
basin_size<-sort(basin_size,decreasing = T)

## Multiply the three biggest basin sizes together
basin_size[1]*basin_size[2]*basin_size[3]
