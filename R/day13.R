# Advent of Code Day 13

## Read in the data
data<-read.table("data/day13.txt",nrows=959,sep=",")
instruct<-read.table("data/day13.txt",skip=960,sep="=")

## Edit data for base 1 arrays
data<-data+1
instruct$V2<-instruct$V2+1

# Part 1
## get dot matrix
dots<-matrix(0,nrow=max(data[,2]),ncol=max(data[,1])+1)
for (i in 1:nrow(data)){
  dots[data[i,2],data[i,1]]<-1
}

## get fold line
fold<-instruct[1,2]

## do a fold
dots<-dots[,ncol(dots):(fold+1)]+dots[,1:(fold-1)]

## get number of dots
sum(dots>0)


# Part 2
## get dot matrix
dots<-matrix(0,nrow=max(data[,2]),ncol=max(data[,1])+1)
for (i in 1:nrow(data)){
  dots[data[i,2],data[i,1]]<-1
}

## for each instruction fold and combine dots
for (i in 1:nrow(instruct)){
  fold<-instruct[i,2]
  if (instruct[i,1]=="fold along x"){
    dots<-dots[,ncol(dots):(fold+1)]+dots[,1:(fold-1)]
  } else if (instruct[i,1]=="fold along y"){
    dots<-dots[nrow(dots):(fold+1),]+dots[1:(fold-1),]
  }
}

## Visually inspect result to get code
toplot<-dots
toplot[toplot>0]<-1
image(t(toplot[nrow(toplot):1,]))
