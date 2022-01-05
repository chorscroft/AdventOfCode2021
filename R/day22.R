# Advent of Code Day 22

## Read in the data
data<-read.table("data/day22.txt")

# Part 1
## Initialise grid
df <- array(0, dim=c(101, 101, 101))

## Only consider the first 20 cuboids
for(d in 1:20){
  ## Pull out the x, y, and z ranges
  split<-unlist(strsplit(data$V2[d],"="))
  split<-unlist(strsplit(split[2],","))
  split<-unlist(strsplit(split[1],"\\."))
  x<-as.numeric(c(split[c(1,3)]))
  split<-unlist(strsplit(data$V2[d],"="))
  split<-unlist(strsplit(split[3],","))
  split<-unlist(strsplit(split[1],"\\."))
  y<-as.numeric(c(split[c(1,3)]))
  split<-unlist(strsplit(data$V2[d],"="))
  split<-unlist(strsplit(split[4],"\\."))
  z<-as.numeric(c(split[c(1,3)]))
  ## If it is going on, turn every cube in the cuboid on
  ## otherwise, turn it off
  if(data$V1[d]=="on"){
    df[(x[1]+51):(x[2]+51),(y[1]+51):(y[2]+51),(z[1]+51):(z[2]+51)]<-1
  } else {
    df[(x[1]+51):(x[2]+51),(y[1]+51):(y[2]+51),(z[1]+51):(z[2]+51)]<-0
  }
}

## Return the number of lit cubes
sum(df)

# Part 2

## The idea was to read in each cuboid, and then remove any sections of it that overlap 
## with future cuboids (on or off). In practice this means turning the cuboid into multiple
## other, smaller, cuboids. Eventually you'll be left with a list of non-overlapping cuboids,
## and you can sum the cubes within them.

## Read in the data
data<-read.table("data/day22.txt")

## Initialise instructions
instruct<-data.frame(x1=rep(0,nrow(data)),x2=rep(0,nrow(data)),y1=rep(0,nrow(data)),y2=rep(0,nrow(data)),z1=rep(0,nrow(data)),z2=rep(0,nrow(data)),on=rep(0,nrow(data)))
## Get x, y, z, and on/off status for each line of the instructions
for(d in 1:nrow(data)){
  split<-unlist(strsplit(data$V2[d],"="))
  split<-unlist(strsplit(split[2],","))
  split<-unlist(strsplit(split[1],"\\."))
  instruct[d,1:2]<-as.numeric(c(split[c(1,3)]))
  split<-unlist(strsplit(data$V2[d],"="))
  split<-unlist(strsplit(split[3],","))
  split<-unlist(strsplit(split[1],"\\."))
  instruct[d,3:4]<-as.numeric(c(split[c(1,3)]))
  split<-unlist(strsplit(data$V2[d],"="))
  split<-unlist(strsplit(split[4],"\\."))
  instruct[d,5:6]<-as.numeric(c(split[c(1,3)]))
  if (data$V1[d]=="on"){
    instruct[d,7]<-1
  }
}

## Initialise instructions - these will be added to as cuboids are split
## into smaller cuboids
newinstruct<-instruct

## start with cuboid 1
d<-1
## Loop until you have considered each line of the instructions
while(d <= (nrow(newinstruct)-1)){
  if (newinstruct$on[d]==1){
    ## if the cuboid is on then compare it to each future cuboid on the list (e)
    e<-d+1
    while(e<=nrow(newinstruct)){
      ## if there is an overlap then see what it is
      if(!(newinstruct$x1[e]>newinstruct$x2[d] || newinstruct$x2[e]<newinstruct$x1[d] || newinstruct$y1[e]>newinstruct$y2[d] || newinstruct$y2[e]<newinstruct$y1[d] || newinstruct$z1[e]>newinstruct$z2[d] || newinstruct$z2[e]<newinstruct$z1[d])){
        ## completely ovelaps
        if(newinstruct$x1[d]>=newinstruct$x1[e] && newinstruct$x2[d]<=newinstruct$x2[e] && newinstruct$y1[d]>=newinstruct$y1[e] && newinstruct$y2[d]<=newinstruct$y2[e] && newinstruct$z1[d]>=newinstruct$z1[e] && newinstruct$z2[d]<=newinstruct$z2[e]){
          newinstruct<-newinstruct[-d,]
          ## do not iterate d but stop this loop
          d<-d-1
          e<-e-1
          break
          
          ## Split in two halves
        } else if (newinstruct$x1[e]<=newinstruct$x1[d] && newinstruct$x2[d]<=newinstruct$x2[e] && newinstruct$y1[e]<=newinstruct$y1[d] && newinstruct$y2[d]<=newinstruct$y2[e] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z2[e]<newinstruct$z2[d]){
          ## through z
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$z2[d]<-newinstruct$z1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newinstruct[(d+1):nrow(newinstruct),])         
        } else if (newinstruct$x1[e]<=newinstruct$x1[d] && newinstruct$x2[d]<=newinstruct$x2[e] && newinstruct$z1[e]<=newinstruct$z1[d] && newinstruct$z2[d]<=newinstruct$z2[e] && newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y2[e]<newinstruct$y2[d]){
          ## through y
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$y2[d]<-newinstruct$y1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newinstruct[(d+1):nrow(newinstruct),])         
        } else if (newinstruct$y1[e]<=newinstruct$y1[d] && newinstruct$y2[d]<=newinstruct$y2[e] && newinstruct$z1[e]<=newinstruct$z1[d] && newinstruct$z2[d]<=newinstruct$z2[e] && newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x2[e]<newinstruct$x2[d]){
          ## through x
          newline<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$x2[d]<-newinstruct$x1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newinstruct[(d+1):nrow(newinstruct),])    
          
          ## leave a single slice behind
        } else if (newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x1[e]<=newinstruct$x2[d] && newinstruct$y1[d]>=newinstruct$y1[e] && newinstruct$y2[d]<=newinstruct$y2[e] && newinstruct$z1[d]>=newinstruct$z1[e] && newinstruct$z2[d]<=newinstruct$z2[e]){
          ## slices through upper x
          newinstruct$x2[d]<-newinstruct$x1[e]-1
        } else if (newinstruct$x1[d]<=newinstruct$x2[e] && newinstruct$x2[e]<newinstruct$x2[d] && newinstruct$y1[d]>=newinstruct$y1[e] && newinstruct$y2[d]<=newinstruct$y2[e] && newinstruct$z1[d]>=newinstruct$z1[e] && newinstruct$z2[d]<=newinstruct$z2[e]){
          ## slices through lower x
          newinstruct$x1[d]<-newinstruct$x2[e]+1
        } else if (newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y1[e]<=newinstruct$y2[d] && newinstruct$x1[d]>=newinstruct$x1[e] && newinstruct$x2[d]<=newinstruct$x2[e] && newinstruct$z1[d]>=newinstruct$z1[e] && newinstruct$z2[d]<=newinstruct$z2[e]){
          ## slices through upper y
          newinstruct$y2[d]<-newinstruct$y1[e]-1
        } else if (newinstruct$y1[d]<=newinstruct$y2[e] && newinstruct$y2[e]<newinstruct$y2[d] && newinstruct$x1[d]>=newinstruct$x1[e] && newinstruct$x2[d]<=newinstruct$x2[e] && newinstruct$z1[d]>=newinstruct$z1[e] && newinstruct$z2[d]<=newinstruct$z2[e]){
          ## slices through lower y
          newinstruct$y1[d]<-newinstruct$y2[e]+1
        } else if (newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z1[e]<=newinstruct$z2[d] && newinstruct$x1[d]>=newinstruct$x1[e] && newinstruct$x2[d]<=newinstruct$x2[e] && newinstruct$y1[d]>=newinstruct$y1[e] && newinstruct$y2[d]<=newinstruct$y2[e]){
          ## slices through upper z
          newinstruct$z2[d]<-newinstruct$z1[e]-1
        } else if (newinstruct$z1[d]<=newinstruct$z2[e] && newinstruct$z2[e]<newinstruct$z2[d] && newinstruct$x1[d]>=newinstruct$x1[e] && newinstruct$x2[d]<=newinstruct$x2[e] && newinstruct$y1[d]>=newinstruct$y1[e] && newinstruct$y2[d]<=newinstruct$y2[e]){
          ## slices through lower z
          newinstruct$z1[d]<-newinstruct$z2[e]+1
          
          ## internal cube
        } else if (newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x2[e]<newinstruct$x2[d] && newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y2[e]<newinstruct$y2[d] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z2[e]<newinstruct$z2[d]){ 
          ## through z
          newline<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[d],newinstruct$z1[e]-1,1)
          newline3<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          newline4<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y1[d],newinstruct$y1[e]-1,newinstruct$z1[e],newinstruct$z2[e],1)
          newline5<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[e],newinstruct$z2[e],1)
          ## treat old line like a slice
          newinstruct$x2[d]<-newinstruct$x1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newline3,newline4,newline5,newinstruct[(d+1):nrow(newinstruct),])             
          
          ## pipe
        } else if (newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x2[e]<newinstruct$x2[d] && newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y2[e]<newinstruct$y2[d] && newinstruct$z1[e]<=newinstruct$z1[d] && newinstruct$z2[d]<=newinstruct$z2[e]){      
          ## through z
          newline<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y1[d],newinstruct$y1[e]-1,newinstruct$z1[d],newinstruct$z2[d],1)
          newline3<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$x2[d]<-newinstruct$x1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newline3,newinstruct[(d+1):nrow(newinstruct),])   
        } else if (newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x2[e]<newinstruct$x2[d] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z2[e]<newinstruct$z2[d] && newinstruct$y1[e]<=newinstruct$y1[d] && newinstruct$y2[d]<=newinstruct$y2[e]){      
          ## through y
          newline<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[d],newinstruct$z1[e]-1,1)
          newline3<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$x2[d]<-newinstruct$x1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newline3,newinstruct[(d+1):nrow(newinstruct),])           
        } else if (newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y2[e]<newinstruct$y2[d] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z2[e]<newinstruct$z2[d] && newinstruct$x1[e]<=newinstruct$x1[d] && newinstruct$x2[d]<=newinstruct$x2[e]){      
          ## through x
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[e],newinstruct$y2[e],newinstruct$z1[d],newinstruct$z1[e]-1,1)
          newline3<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[e],newinstruct$y2[e],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$y2[d]<-newinstruct$y1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newline3,newinstruct[(d+1):nrow(newinstruct),])  
          
          ## U HOLES
        } else if (newinstruct$x1[e]<=newinstruct$x1[d] && newinstruct$x2[d]<=newinstruct$x2[e] && newinstruct$y1[d] <= newinstruct$y2[e] && newinstruct$y2[e]<newinstruct$y2[d] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z2[e]<newinstruct$z2[d]){
          ## towards upper y through z
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[e],newinstruct$z2[e],1)
          ## treat old line like a slice
          newinstruct$z2[d]<-newinstruct$z1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newinstruct[(d+1):nrow(newinstruct),])  
        } else if (newinstruct$z1[e]<=newinstruct$z1[d] && newinstruct$z2[d]<=newinstruct$z2[e] && newinstruct$y1[d] <= newinstruct$y2[e] && newinstruct$y2[e]<newinstruct$y2[d] && newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x2[e]<newinstruct$x2[d]){
          ## towards upper y through x
          newline<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$x2[d]<-newinstruct$x1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newinstruct[(d+1):nrow(newinstruct),])        
        } else if (newinstruct$y1[e]<=newinstruct$y1[d] && newinstruct$y2[d]<=newinstruct$y2[e] && newinstruct$x1[d] <= newinstruct$x2[e] && newinstruct$x2[e]<newinstruct$x2[d] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z2[e]<newinstruct$z2[d]){
          ## towards upper x through z
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          newline2<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[e],newinstruct$z2[e],1)
          ## treat old line like a slice
          newinstruct$z2[d]<-newinstruct$z1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newinstruct[(d+1):nrow(newinstruct),])          
        } else if (newinstruct$z1[e]<=newinstruct$z1[d] && newinstruct$z2[d]<=newinstruct$z2[e] && newinstruct$x1[d] <= newinstruct$x2[e] && newinstruct$x2[e]<newinstruct$x2[d] && newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y2[e]<newinstruct$y2[d]){
          ## towards upper x through y
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[e],newinstruct$y2[e],newinstruct$z1[d],newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$y2[d]<-newinstruct$y1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newinstruct[(d+1):nrow(newinstruct),])        
        } else if (newinstruct$x1[e]<=newinstruct$x1[d] && newinstruct$x2[d]<=newinstruct$x2[e] && newinstruct$z1[d] <= newinstruct$z2[e] && newinstruct$z2[e]<newinstruct$z2[d] && newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y2[e]<newinstruct$y2[d]){
          ## towards upper z through y
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[e],newinstruct$y2[e],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$y2[d]<-newinstruct$y1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newinstruct[(d+1):nrow(newinstruct),])            
        } else if (newinstruct$y1[e]<=newinstruct$y1[d] && newinstruct$y2[d]<=newinstruct$y2[e] && newinstruct$z1[d] <= newinstruct$z2[e] && newinstruct$z2[e]<newinstruct$z2[d] && newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x2[e]<newinstruct$x2[d]){
          ## towards upper z through x
          newline<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$x2[d]<-newinstruct$x1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newinstruct[(d+1):nrow(newinstruct),])              
        } else if (newinstruct$x1[e]<=newinstruct$x1[d] && newinstruct$x2[d]<=newinstruct$x2[e] && newinstruct$y1[d] < newinstruct$y1[e] && newinstruct$y1[e]<=newinstruct$y2[d] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z2[e]<newinstruct$z2[d]){
          ## towards lower y through z
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[d],newinstruct$y1[e]-1,newinstruct$z1[e],newinstruct$z2[e],1)
          ## treat old line like a slice
          newinstruct$z2[d]<-newinstruct$z1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newinstruct[(d+1):nrow(newinstruct),])         
        } else if (newinstruct$z1[e]<=newinstruct$z1[d] && newinstruct$z2[d]<=newinstruct$z2[e] && newinstruct$y1[d] < newinstruct$y1[e] && newinstruct$y1[e]<=newinstruct$y2[d] && newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x2[e]<newinstruct$x2[d]){
          ## towards lower y through x
          newline<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y1[d],newinstruct$y1[e]-1,newinstruct$z1[d],newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$x2[d]<-newinstruct$x1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newinstruct[(d+1):nrow(newinstruct),])            
        } else if (newinstruct$y1[e]<=newinstruct$y1[d] && newinstruct$y2[d]<=newinstruct$y2[e] && newinstruct$x1[d] < newinstruct$x1[e] && newinstruct$x1[e]<=newinstruct$x2[d] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z2[e]<newinstruct$z2[d]){
          ## towards lower x through z
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[d],newinstruct$x1[e]-1,newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[e],newinstruct$z2[e],1)
          ## treat old line like a slice
          newinstruct$z2[d]<-newinstruct$z1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newinstruct[(d+1):nrow(newinstruct),])            
        } else if (newinstruct$z1[e]<=newinstruct$z1[d] && newinstruct$z2[d]<=newinstruct$z2[e] && newinstruct$x1[d] < newinstruct$x1[e] && newinstruct$x1[e]<=newinstruct$x2[d] && newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y2[e]<newinstruct$y2[d]){
          ## towards lower x through y
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[d],newinstruct$x1[e]-1,newinstruct$y1[e],newinstruct$y2[e],newinstruct$z1[d],newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$y2[d]<-newinstruct$y1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newinstruct[(d+1):nrow(newinstruct),])                
        } else if (newinstruct$x1[e]<=newinstruct$x1[d] && newinstruct$x2[d]<=newinstruct$x2[e] && newinstruct$z1[d] < newinstruct$z1[e] && newinstruct$z1[e]<=newinstruct$z2[d] && newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y2[e]<newinstruct$y2[d]){
          ## towards lower z through y
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[e],newinstruct$y2[e],newinstruct$z1[d],newinstruct$z1[e]-1,1)
          ## treat old line like a slice
          newinstruct$y2[d]<-newinstruct$y1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newinstruct[(d+1):nrow(newinstruct),])              
        } else if (newinstruct$y1[e]<=newinstruct$y1[d] && newinstruct$y2[d]<=newinstruct$y2[e] && newinstruct$z1[d] < newinstruct$z1[e] && newinstruct$z1[e]<=newinstruct$z2[d] && newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x2[e]<newinstruct$x2[d]){
          ## towards lower z through x
          newline<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[d],newinstruct$z1[e]-1,1)
          ## treat old line like a slice
          newinstruct$x2[d]<-newinstruct$x1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newinstruct[(d+1):nrow(newinstruct),])           
        
          ## whole edges
        } else if (newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x1[e]<=newinstruct$x2[d] && newinstruct$x2[e]>=newinstruct$x2[d] && newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y1[e]<=newinstruct$y2[d] && newinstruct$y2[e]>=newinstruct$y2[d] && newinstruct$z1[d]>=newinstruct$z1[e] && newinstruct$z2[d]<=newinstruct$z2[e]){
          ## 1cut a whole edge: upper x1 upper y1
          ## new line to insert
          newline<-c(newinstruct$x1[e],newinstruct$x2[d],newinstruct$y1[d],newinstruct$y1[e]-1,newinstruct$z1[d],newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$x2[d]<-newinstruct$x1[e]-1
          ## insert new line
          newinstruct<-rbind(newinstruct[1:d,],newline,newinstruct[(d+1):nrow(newinstruct),])
        } else if (newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x1[e]<=newinstruct$x2[d] && newinstruct$x2[e]>=newinstruct$x2[d] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z1[e]<=newinstruct$z2[d] && newinstruct$z2[e]>=newinstruct$z2[d] && newinstruct$y1[d]>=newinstruct$y1[e] && newinstruct$y2[d]<=newinstruct$y2[e]){
          ## 2cut a whole edge: upper x1 upper z1
          ## new line to insert
          newline<-c(newinstruct$x1[e],newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[d],newinstruct$z1[e]-1,1)
          ## treat old line like a slice
          newinstruct$x2[d]<-newinstruct$x1[e]-1
          ## insert new line
          newinstruct<-rbind(newinstruct[1:d,],newline,newinstruct[(d+1):nrow(newinstruct),])
        } else if (newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z1[e]<=newinstruct$z2[d] && newinstruct$z2[e]>=newinstruct$z2[d] && newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y1[e]<=newinstruct$y2[d] && newinstruct$y2[e]>=newinstruct$y2[d] && newinstruct$x1[d]>=newinstruct$x1[e] && newinstruct$x2[d]<=newinstruct$x2[e]){
          ## 3cut a whole edge: upper z1 upper y1
          ## new line to insert
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[d],newinstruct$y1[e]-1,newinstruct$z1[e],newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$z2[d]<-newinstruct$z1[e]-1
          ## insert new line
          newinstruct<-rbind(newinstruct[1:d,],newline,newinstruct[(d+1):nrow(newinstruct),])  
        } else if (newinstruct$x1[d]<=newinstruct$x2[e] && newinstruct$x2[e]<=newinstruct$x2[d] && newinstruct$x1[e]<=newinstruct$x1[d] && newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y1[e]<=newinstruct$y2[d] && newinstruct$y2[e]>=newinstruct$y2[d] && newinstruct$z1[d]>=newinstruct$z1[e] && newinstruct$z2[d]<=newinstruct$z2[e]){
          ## 4cut a whole edge: lower x1 upper y1
          ## new line to insert
          newline<-c(newinstruct$x1[d],newinstruct$x2[e],newinstruct$y1[d],newinstruct$y1[e]-1,newinstruct$z1[d],newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$x1[d]<-newinstruct$x2[e]+1
          ## insert new line
          newinstruct<-rbind(newinstruct[1:d,],newline,newinstruct[(d+1):nrow(newinstruct),])   
        } else if (newinstruct$x1[d]<=newinstruct$x2[e] && newinstruct$x2[e]<=newinstruct$x2[d] && newinstruct$x1[e]<=newinstruct$x1[d] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z1[e]<=newinstruct$z2[d] && newinstruct$z2[e]>=newinstruct$z2[d] && newinstruct$y1[d]>=newinstruct$y1[e] && newinstruct$y2[d]<=newinstruct$y2[e]){
          ## 5cut a whole edge: lower x1 upper z1
          ## new line to insert
          newline<-c(newinstruct$x1[d],newinstruct$x2[e],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[d],newinstruct$z1[e]-1,1)
          ## treat old line like a slice
          newinstruct$x1[d]<-newinstruct$x2[e]+1
          ## insert new line
          newinstruct<-rbind(newinstruct[1:d,],newline,newinstruct[(d+1):nrow(newinstruct),])
        } else if (newinstruct$y1[d]<=newinstruct$y2[e] && newinstruct$y2[e]<=newinstruct$y2[d] && newinstruct$y1[e]<=newinstruct$y1[d] && newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x1[e]<=newinstruct$x2[d] && newinstruct$x2[e]>=newinstruct$x2[d] && newinstruct$z1[d]>=newinstruct$z1[e] && newinstruct$z2[d]<=newinstruct$z2[e]){
          ## 6cut a whole edge: lower y1 upper x1
          ## new line to insert
          newline<-c(newinstruct$x1[d],newinstruct$x1[e]-1,newinstruct$y1[d],newinstruct$y2[e],newinstruct$z1[d],newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$y1[d]<-newinstruct$y2[e]+1
          ## insert new line
          newinstruct<-rbind(newinstruct[1:d,],newline,newinstruct[(d+1):nrow(newinstruct),])  
        } else if (newinstruct$y1[d]<=newinstruct$y2[e] && newinstruct$y2[e]<=newinstruct$y2[d] && newinstruct$y1[e]<=newinstruct$y1[d] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z1[e]<=newinstruct$z2[d] && newinstruct$z2[e]>=newinstruct$z2[d] && newinstruct$x1[d]>=newinstruct$x1[e] && newinstruct$x2[d]<=newinstruct$x2[e]){
          ## 7cut a whole edge: lower y1 upper z1
          ## new line to insert
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[e],newinstruct$z1[d],newinstruct$z1[e]-1,1)
          ## treat old line like a slice
          newinstruct$y1[d]<-newinstruct$y2[e]+1
          ## insert new line
          newinstruct<-rbind(newinstruct[1:d,],newline,newinstruct[(d+1):nrow(newinstruct),])  
        } else if (newinstruct$z1[d]<=newinstruct$z2[e] && newinstruct$z2[e]<=newinstruct$z2[d] && newinstruct$z1[e]<=newinstruct$z1[d] && newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x1[e]<=newinstruct$x2[d] && newinstruct$x2[e]>=newinstruct$x2[d] && newinstruct$y1[d]>=newinstruct$y1[e] && newinstruct$y2[d]<=newinstruct$y2[e]){
          ## 8cut a whole edge: lower z1 upper x1
          ## new line to insert
          newline<-c(newinstruct$x1[d],newinstruct$x1[e]-1,newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[e],1)
          ## treat old line like a slice
          newinstruct$z1[d]<-newinstruct$z2[e]+1
          ## insert new line
          newinstruct<-rbind(newinstruct[1:d,],newline,newinstruct[(d+1):nrow(newinstruct),])            
        } else if (newinstruct$z1[d]<=newinstruct$z2[e] && newinstruct$z2[e]<=newinstruct$z2[d] && newinstruct$z1[e]<=newinstruct$z1[d] && newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y1[e]<=newinstruct$y2[d] && newinstruct$y2[e]>=newinstruct$y2[d] && newinstruct$x1[d]>=newinstruct$x1[e] && newinstruct$x2[d]<=newinstruct$x2[e]){
          ## 9cut a whole edge: lower z1 upper y1
          ## new line to insert
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[d],newinstruct$y1[e]-1,newinstruct$z1[d],newinstruct$z2[e],1)
          ## treat old line like a slice
          newinstruct$z1[d]<-newinstruct$z2[e]+1
          ## insert new line
          newinstruct<-rbind(newinstruct[1:d,],newline,newinstruct[(d+1):nrow(newinstruct),])    
        } else if (newinstruct$x1[d]<=newinstruct$x2[e] && newinstruct$x2[e]<=newinstruct$x2[d] && newinstruct$x1[e]<=newinstruct$x1[d] && newinstruct$y1[d]<=newinstruct$y2[e] && newinstruct$y2[e]<=newinstruct$y2[d] && newinstruct$y1[e]<=newinstruct$y1[d] && newinstruct$z1[d]>=newinstruct$z1[e] && newinstruct$z2[d]<=newinstruct$z2[e]){
          ## 10cut a whole edge: lower x1 lower y1
          ## new line to insert
          newline<-c(newinstruct$x1[d],newinstruct$x2[e],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$x1[d]<-newinstruct$x2[e]+1
          ## insert new line
          newinstruct<-rbind(newinstruct[1:d,],newline,newinstruct[(d+1):nrow(newinstruct),])          
        } else if (newinstruct$x1[d]<=newinstruct$x2[e] && newinstruct$x2[e]<=newinstruct$x2[d] && newinstruct$x1[e]<=newinstruct$x1[d] && newinstruct$z1[d]<=newinstruct$z2[e] && newinstruct$z2[e]<=newinstruct$z2[d] && newinstruct$z1[e]<=newinstruct$z1[d] && newinstruct$y1[d]>=newinstruct$y1[e] && newinstruct$y2[d]<=newinstruct$y2[e]){
          ## 11cut a whole edge: lower x1 lower z1
          ## new line to insert
          newline<-c(newinstruct$x1[d],newinstruct$x2[e],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$x1[d]<-newinstruct$x2[e]+1
          ## insert new line
          newinstruct<-rbind(newinstruct[1:d,],newline,newinstruct[(d+1):nrow(newinstruct),])  
        } else if (newinstruct$z1[d]<=newinstruct$z2[e] && newinstruct$z2[e]<=newinstruct$z2[d] && newinstruct$z1[e]<=newinstruct$z1[d] && newinstruct$y1[d]<=newinstruct$y2[e] && newinstruct$y2[e]<=newinstruct$y2[d] && newinstruct$y1[e]<=newinstruct$y1[d] && newinstruct$x1[d]>=newinstruct$x1[e] && newinstruct$x2[d]<=newinstruct$x2[e]){
          ## 12cut a whole edge: lower z1 lower y1
          ## new line to insert
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[e],1)
          ## treat old line like a slice
          newinstruct$z1[d]<-newinstruct$z2[e]+1
          ## insert new line
          newinstruct<-rbind(newinstruct[1:d,],newline,newinstruct[(d+1):nrow(newinstruct),])             
        
          ## face poke
        } else if (newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x2[e]<newinstruct$x2[d] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z2[e]<newinstruct$z2[d] && newinstruct$y1[e]<=newinstruct$y1[d] && newinstruct$y1[d]<newinstruct$y2[e] && newinstruct$y2[e]<newinstruct$y2[d]){        
          # lower y
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[d],newinstruct$x1[e]-1,newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[e],newinstruct$z2[e],1)
          newline3<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[e],newinstruct$z2[e],1)
          newline4<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[e],newinstruct$z2[e],1)
          ## treat old line like a slice
          newinstruct$z2[d]<-newinstruct$z1[e]-1
          ## insert new line
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newline3,newline4,newinstruct[(d+1):nrow(newinstruct),])   
        } else if (newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x2[e]<newinstruct$x2[d] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z2[e]<newinstruct$z2[d] && newinstruct$y2[e]>=newinstruct$y2[d] && newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y1[e]<newinstruct$y2[d]){        
          # upper y
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[d],newinstruct$x1[e]-1,newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[e],newinstruct$z2[e],1)
          newline3<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y1[d],newinstruct$y1[e]-1,newinstruct$z1[e],newinstruct$z2[e],1)
          newline4<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[e],newinstruct$z2[e],1)
          ## treat old line like a slice
          newinstruct$z2[d]<-newinstruct$z1[e]-1
          ## insert new line
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newline3,newline4,newinstruct[(d+1):nrow(newinstruct),])           
        } else if (newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y2[e]<newinstruct$y2[d] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z2[e]<newinstruct$z2[d] && newinstruct$x1[e]<=newinstruct$x1[d] && newinstruct$x1[d]<newinstruct$x2[e] && newinstruct$x2[e]<newinstruct$x2[d]){        
          # lower x
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[d],newinstruct$y1[e]-1,newinstruct$z1[e],newinstruct$z2[e],1)
          newline3<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[e],newinstruct$y2[e],newinstruct$z1[e],newinstruct$z2[e],1)
          newline4<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[e],newinstruct$z2[e],1)
          ## treat old line like a slice
          newinstruct$z2[d]<-newinstruct$z1[e]-1
          ## insert new line
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newline3,newline4,newinstruct[(d+1):nrow(newinstruct),])   
        } else if (newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y2[e]<newinstruct$y2[d] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z2[e]<newinstruct$z2[d] && newinstruct$x2[e]>=newinstruct$x2[d] && newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x1[e]<newinstruct$x2[d]){        
          # upper x
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[d],newinstruct$y1[e]-1,newinstruct$z1[e],newinstruct$z2[e],1)
          newline3<-c(newinstruct$x1[d],newinstruct$x1[e]-1,newinstruct$y1[e],newinstruct$y2[e],newinstruct$z1[e],newinstruct$z2[e],1)
          newline4<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[e],newinstruct$z2[e],1)
          ## treat old line like a slice
          newinstruct$z2[d]<-newinstruct$z1[e]-1
          ## insert new line
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newline3,newline4,newinstruct[(d+1):nrow(newinstruct),])           
          } else if (newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y2[e]<newinstruct$y2[d] && newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x2[e]<newinstruct$x2[d] && newinstruct$z1[e]<=newinstruct$z1[d] && newinstruct$z1[d]<newinstruct$z2[e] && newinstruct$z2[e]<newinstruct$z2[d]){        
          # lower z
          newline<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y1[d],newinstruct$y1[e]-1,newinstruct$z1[d],newinstruct$z2[d],1)
          newline3<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y1[e],newinstruct$y2[e],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          newline4<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$x2[d]<-newinstruct$x1[e]-1
          ## insert new line
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newline3,newline4,newinstruct[(d+1):nrow(newinstruct),])   
        } else if (newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y2[e]<newinstruct$y2[d] && newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x2[e]<newinstruct$x2[d] && newinstruct$z2[e]>=newinstruct$z2[d] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z1[e]<newinstruct$z2[d]){        
          # upper z
          newline<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y1[d],newinstruct$y1[e]-1,newinstruct$z1[d],newinstruct$z2[d],1)
          newline3<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y1[e],newinstruct$y2[e],newinstruct$z1[d],newinstruct$z1[e]-1,1)
          newline4<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$x2[d]<-newinstruct$x1[e]-1
          ## insert new line
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newline3,newline4,newinstruct[(d+1):nrow(newinstruct),])              
          
          ## edge poke
        } else if (newinstruct$x1[d]>=newinstruct$x1[e] && newinstruct$x1[d]<=newinstruct$x2[e] && newinstruct$x2[e]<newinstruct$x2[d] && newinstruct$y1[e]<=newinstruct$y1[d] && newinstruct$y1[d]<=newinstruct$y2[e] && newinstruct$y2[e]<newinstruct$y2[d] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z2[e]<newinstruct$z2[d]){   
          ## 1edgepoke: lower x lower y
          ## new line to insert
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          newline2<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[e],newinstruct$z2[e],1)
          newline3<-c(newinstruct$x1[d],newinstruct$x2[e],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[e],newinstruct$z2[e],1)
          ## treat old line like a slice
          newinstruct$z2[d]<-newinstruct$z1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newline3,newinstruct[(d+1):nrow(newinstruct),]) 
        } else if (newinstruct$x1[d]>=newinstruct$x1[e] && newinstruct$x1[d]<=newinstruct$x2[e] && newinstruct$x2[e]<newinstruct$x2[d] && newinstruct$z1[e]<=newinstruct$z1[d] && newinstruct$z1[d]<=newinstruct$z2[e] && newinstruct$z2[e]<newinstruct$z2[d] && newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y2[e]<newinstruct$y2[d]){   
          ## 2edgepoke: lower x lower z
          ## new line to insert
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[e],newinstruct$y2[e],newinstruct$z1[d],newinstruct$z2[d],1)
          newline3<-c(newinstruct$x1[d],newinstruct$x2[e],newinstruct$y1[e],newinstruct$y2[e],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$y2[d]<-newinstruct$y1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newline3,newinstruct[(d+1):nrow(newinstruct),])         
        } else if (newinstruct$y1[d]>=newinstruct$y1[e] && newinstruct$y1[d]<=newinstruct$y2[e] && newinstruct$y2[e]<newinstruct$y2[d] && newinstruct$z1[e]<=newinstruct$z1[d] && newinstruct$z1[d]<=newinstruct$z2[e] && newinstruct$z2[e]<newinstruct$z2[d] && newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x2[e]<newinstruct$x2[d]){   
          ## 3edgepoke: lower y lower z
          ## new line to insert
          newline<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          newline3<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y1[d],newinstruct$y2[e],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$x2[d]<-newinstruct$x1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newline3,newinstruct[(d+1):nrow(newinstruct),])                 
        } else if (newinstruct$x2[d]<=newinstruct$x2[e] && newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x1[e]<newinstruct$x2[d] && newinstruct$y1[e]<=newinstruct$y1[d] && newinstruct$y1[d]<=newinstruct$y2[e] && newinstruct$y2[e]<newinstruct$y2[d] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z2[e]<newinstruct$z2[d]){   
          ## 4edgepoke: upper x lower y
          ## new line to insert
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[d],newinstruct$x1[e]-1,newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[e],newinstruct$z2[e],1)
          newline3<-c(newinstruct$x1[e],newinstruct$x2[d],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[e],newinstruct$z2[e],1)
          ## treat old line like a slice
          newinstruct$z2[d]<-newinstruct$z1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newline3,newinstruct[(d+1):nrow(newinstruct),])         
        } else if (newinstruct$x2[d]<=newinstruct$x2[e] && newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x1[e]<newinstruct$x2[d] && newinstruct$z1[e]<=newinstruct$z1[d] && newinstruct$z1[d]<=newinstruct$z2[e] && newinstruct$z2[e]<newinstruct$z2[d] && newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y2[e]<newinstruct$y2[d]){   
          ## 5edgepoke: upper x lower z
          ## new line to insert
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[d],newinstruct$x1[e]-1,newinstruct$y1[e],newinstruct$y2[e],newinstruct$z1[d],newinstruct$z2[d],1)
          newline3<-c(newinstruct$x1[e],newinstruct$x2[d],newinstruct$y1[e],newinstruct$y2[e],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$y2[d]<-newinstruct$y1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newline3,newinstruct[(d+1):nrow(newinstruct),])           
        } else if (newinstruct$y2[d]<=newinstruct$y2[e] && newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y1[e]<newinstruct$y2[d] && newinstruct$z1[e]<=newinstruct$z1[d] && newinstruct$z1[d]<=newinstruct$z2[e] && newinstruct$z2[e]<newinstruct$z2[d] && newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x2[e]<newinstruct$x2[d]){   
          ## 6edgepoke: upper y lower z
          ## new line to insert
          newline<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y1[d],newinstruct$y1[e]-1,newinstruct$z1[d],newinstruct$z2[d],1)
          newline3<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y1[e],newinstruct$y2[d],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$x2[d]<-newinstruct$x1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newline3,newinstruct[(d+1):nrow(newinstruct),])           
        } else if (newinstruct$y2[d]<=newinstruct$y2[e] && newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y1[e]<newinstruct$y2[d] && newinstruct$x1[e]<=newinstruct$x1[d] && newinstruct$x1[d]<=newinstruct$x2[e] && newinstruct$x2[e]<newinstruct$x2[d] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z2[e]<newinstruct$z2[d]){   
          ## 7edgepoke: upper y lower x
          ## new line to insert
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[d],newinstruct$y1[e]-1,newinstruct$z1[e],newinstruct$z2[e],1)
          newline3<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[e],newinstruct$y2[d],newinstruct$z1[e],newinstruct$z2[e],1)
          ## treat old line like a slice
          newinstruct$z2[d]<-newinstruct$z1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newline3,newinstruct[(d+1):nrow(newinstruct),])           
        } else if (newinstruct$z2[d]<=newinstruct$z2[e] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z1[e]<newinstruct$z2[d] && newinstruct$x1[e]<=newinstruct$x1[d] && newinstruct$x1[d]<=newinstruct$x2[e] && newinstruct$x2[e]<newinstruct$x2[d] && newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y2[e]<newinstruct$y2[d]){   
          ## 8edgepoke: upper z lower x
          ## new line to insert
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[e],newinstruct$y2[e],newinstruct$z1[d],newinstruct$z1[e]-1,1)
          newline3<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[e],newinstruct$y2[e],newinstruct$z1[e],newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$y2[d]<-newinstruct$y1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newline3,newinstruct[(d+1):nrow(newinstruct),])                
        } else if (newinstruct$z2[d]<=newinstruct$z2[e] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z1[e]<newinstruct$z2[d] && newinstruct$y1[e]<=newinstruct$y1[d] && newinstruct$y1[d]<=newinstruct$y2[e] && newinstruct$y2[e]<newinstruct$y2[d] && newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x2[e]<newinstruct$x2[d]){   
          ## 9edgepoke: upper z lower y
          ## new line to insert
          newline<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[d],newinstruct$z1[e]-1,1)
          newline3<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[e],newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$x2[d]<-newinstruct$x1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newline3,newinstruct[(d+1):nrow(newinstruct),])         
        } else if (newinstruct$x2[d]<=newinstruct$x2[e] && newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x1[e]<newinstruct$x2[d] && newinstruct$y2[e]>=newinstruct$y2[d] && newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y1[e]<newinstruct$y2[d] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z2[e]<newinstruct$z2[d]){         
          ## 10edgepoke: upper x upper y
          ## new line to insert
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[d],newinstruct$x1[e]-1,newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[e],newinstruct$z2[e],1)
          newline3<-c(newinstruct$x1[e],newinstruct$x2[d],newinstruct$y1[d],newinstruct$y1[e]-1,newinstruct$z1[e],newinstruct$z2[e],1)
          ## treat old line like a slice
          newinstruct$z2[d]<-newinstruct$z1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newline3,newinstruct[(d+1):nrow(newinstruct),])       
        } else if (newinstruct$x2[d]<=newinstruct$x2[e] && newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x1[e]<newinstruct$x2[d] && newinstruct$z2[e]>=newinstruct$z2[d] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z1[e]<newinstruct$z2[d] && newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y2[e]<newinstruct$y2[d]){         
          ## 11edgepoke: upper x upper z
          ## new line to insert
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[d],newinstruct$x1[e]-1,newinstruct$y1[e],newinstruct$y2[e],newinstruct$z1[d],newinstruct$z2[d],1)
          newline3<-c(newinstruct$x1[e],newinstruct$x2[d],newinstruct$y1[e],newinstruct$y2[e],newinstruct$z1[d],newinstruct$z1[e]-1,1)
          ## treat old line like a slice
          newinstruct$y2[d]<-newinstruct$y1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newline3,newinstruct[(d+1):nrow(newinstruct),])           
        } else if (newinstruct$y2[d]<=newinstruct$y2[e] && newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y1[e]<newinstruct$y2[d] && newinstruct$z2[e]>=newinstruct$z2[d] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z1[e]<newinstruct$z2[d] && newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x2[e]<newinstruct$x2[d]){         
          ## 12edgepoke: upper y upper z
          ## new line to insert
          newline<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y1[d],newinstruct$y1[e]-1,newinstruct$z1[d],newinstruct$z2[d],1)
          newline3<-c(newinstruct$x1[e],newinstruct$x2[e],newinstruct$y1[e],newinstruct$y2[d],newinstruct$z1[d],newinstruct$z1[e]-1,1)
          ## treat old line like a slice
          newinstruct$x2[d]<-newinstruct$x1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newline3,newinstruct[(d+1):nrow(newinstruct),])          
          
        ## corners NEED MORE DEFINITION (unless dp face poke/edge poke first)
        } else if (newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x1[e]<=newinstruct$x2[d] && newinstruct$y1[d]<=newinstruct$y2[e] && newinstruct$y2[e]<newinstruct$y2[d] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z1[e]<=newinstruct$z2[d]){
          ## 1cut a whole corner: upper x1 lower y1 upper z
          ## new line to insert
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[e],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[d],newinstruct$x1[e]-1,newinstruct$y1[d],newinstruct$y2[e],newinstruct$z1[e],newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$z2[d]<-newinstruct$z1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newinstruct[(d+1):nrow(newinstruct),])
        } else if (newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y1[e]<=newinstruct$y2[d] && newinstruct$x1[d]<=newinstruct$x2[e] && newinstruct$x2[e]<newinstruct$x2[d] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z1[e]<=newinstruct$z2[d]){
          ## 2cut a whole corner: lower x1 upper y1 upper z
          ## new line to insert
          newline<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[d],newinstruct$z1[e],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[d],newinstruct$x2[e],newinstruct$y1[d],newinstruct$y1[e]-1,newinstruct$z1[e],newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$z2[d]<-newinstruct$z1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newinstruct[(d+1):nrow(newinstruct),])  
        } else if (newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x1[e]<=newinstruct$x2[d] && newinstruct$z1[d]<=newinstruct$z2[e] && newinstruct$z2[e]<newinstruct$z2[d] && newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y1[e]<=newinstruct$y2[d]){
          ## 3cut a whole corner: upper x1 upper y1 lower z
          ## new line to insert
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[e],newinstruct$y2[d],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[d],newinstruct$x1[e]-1,newinstruct$y1[e],newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[e],1)
          ## treat old line like a slice
          newinstruct$y2[d]<-newinstruct$y1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newinstruct[(d+1):nrow(newinstruct),])
        } else if (newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x1[e]<=newinstruct$x2[d] && newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y1[e]<=newinstruct$y2[d] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z1[e]<=newinstruct$z2[d]){
          ## 4cut a whole corner: upper x1 upper y1 upper z
          ## new line to insert
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[d],newinstruct$y1[e]-1,newinstruct$z1[e],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[d],newinstruct$x1[e]-1,newinstruct$y1[e],newinstruct$y2[d],newinstruct$z1[e],newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$z2[d]<-newinstruct$z1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newinstruct[(d+1):nrow(newinstruct),])        
        } else if (newinstruct$x1[d]<=newinstruct$x2[e] && newinstruct$x2[e]<newinstruct$x2[d] && newinstruct$y1[d]<=newinstruct$y2[e] && newinstruct$y2[e]<newinstruct$y2[d] && newinstruct$z1[d]<newinstruct$z1[e] && newinstruct$z1[e]<=newinstruct$z2[d]){
          ## 5cut a whole corner: lower x1 lower y1 upper z
          ## new line to insert
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[e],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[e],newinstruct$z1[e],newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$z2[d]<-newinstruct$z1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newinstruct[(d+1):nrow(newinstruct),])       
        } else if (newinstruct$x1[d]<=newinstruct$x2[e] && newinstruct$x2[e]<newinstruct$x2[d] && newinstruct$z1[d]<=newinstruct$z2[e] && newinstruct$z2[e]<newinstruct$z2[d] && newinstruct$y1[d]<newinstruct$y1[e] && newinstruct$y1[e]<=newinstruct$y2[d]){
          ## 6cut a whole corner: lower x1 upper y1 lower z
          ## new line to insert
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y1[e],newinstruct$y2[d],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          newline2<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[e],newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[e],1)
          ## treat old line like a slice
          newinstruct$y2[d]<-newinstruct$y1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newinstruct[(d+1):nrow(newinstruct),])        
        } else if (newinstruct$z1[d]<=newinstruct$z2[e] && newinstruct$z2[e]<newinstruct$z2[d] && newinstruct$y1[d]<=newinstruct$y2[e] && newinstruct$y2[e]<newinstruct$y2[d] && newinstruct$x1[d]<newinstruct$x1[e] && newinstruct$x1[e]<=newinstruct$x2[d]){
          ## 7cut a whole corner: upper x1 lower y1 lower z
          ## new line to insert
          newline<-c(newinstruct$x1[e],newinstruct$x2[d],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[d],1)
          newline2<-c(newinstruct$x1[e],newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[e],newinstruct$z2[e]+1,newinstruct$z2[d],1)
          ## treat old line like a slice
          newinstruct$x2[d]<-newinstruct$x1[e]-1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newinstruct[(d+1):nrow(newinstruct),])        
        } else if (newinstruct$x1[d]<=newinstruct$x2[e] && newinstruct$x2[e]<newinstruct$x2[d] && newinstruct$y1[d]<=newinstruct$y2[e] && newinstruct$y2[e]<newinstruct$y2[d] && newinstruct$z1[d]<=newinstruct$z2[e] && newinstruct$z2[e]<newinstruct$z2[d]){
          ## 8cut a whole corner: lower x1 lower y1 lower z
          ## new line to insert
          newline<-c(newinstruct$x1[d],newinstruct$x2[d],newinstruct$y2[e]+1,newinstruct$y2[d],newinstruct$z1[d],newinstruct$z2[e],1)
          newline2<-c(newinstruct$x2[e]+1,newinstruct$x2[d],newinstruct$y1[d],newinstruct$y2[e],newinstruct$z1[d],newinstruct$z2[e],1)
          ## treat old line like a slice
          newinstruct$z1[d]<-newinstruct$z2[e]+1
          ## insert new lines
          newinstruct<-rbind(newinstruct[1:d,],newline,newline2,newinstruct[(d+1):nrow(newinstruct),])                 
        }  
      }
      ## Iterate e
      e<-e+1
    }
    ## Iterate d
    d<-d+1
  } else{
    ## remove if this cuboid is "off". Do not iterate d.
    newinstruct<-newinstruct[-d,]
  }
}

## If final instruction is "off", remove it
if(newinstruct$on[nrow(newinstruct)]==0){
  newinstruct<-newinstruct[-nrow(newinstruct),]
}

## Get the sum of the volumes of each cuboid in the final data frame
cubeson<-0
for (i in 1:nrow(newinstruct)){
  cubeson<-cubeson+(newinstruct$x2[i]-newinstruct$x1[i]+1)*(newinstruct$y2[i]-newinstruct$y1[i]+1)*(newinstruct$z2[i]-newinstruct$z1[i]+1)
}
options(scipen=999)
cubeson
