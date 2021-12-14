# Advent of Code Day 14

## Read in the data
instruct<-read.table("data/day14.txt",skip=2,sep=" ")
data<-read.fwf("data/day14.txt",rep(1,20),nrows=1,colClasses="character")

# Part 1
## Get polymer template as a character vector
newData<-as.character(data[1,])

## Find unique letters in template and insertion rules
uni<-unique(c(newData,instruct$V3))

## Initialise a matrix that counts the number of pairs XY
mat<-matrix(0,length(uni),length(uni))

## Fill matrix with existing pair counts from initial polymer template
for(i in 1:(length(newData)-1)){
  mat[which(newData[i]==uni),which(newData[i+1]==uni)]<-mat[which(newData[i]==uni),which(newData[i+1]==uni)]+1
}

## loop 10 times
for(loops in 1:10){
  ## Create temporary changes matrix
  changes<-matrix(0,length(uni),length(uni))
  for(i in 1:nrow(instruct)){
    ## for each instruction, extract the pairs
    pair<-unlist(strsplit(instruct$V1[i],""))
    ## remove the count of all these pairs in the current polymer template
    changes[which(pair[1]==uni),which(pair[2]==uni)]<-changes[which(pair[1]==uni),which(pair[2]==uni)]-mat[which(pair[1]==uni),which(pair[2]==uni)]
    ## add a count of all the new pairs 
    changes[which(pair[1]==uni),which(instruct$V3[i]==uni)]<-changes[which(pair[1]==uni),which(instruct$V3[i]==uni)]+mat[which(pair[1]==uni),which(pair[2]==uni)]
    changes[which(instruct$V3[i]==uni),which(pair[2]==uni)]<-changes[which(instruct$V3[i]==uni),which(pair[2]==uni)]+mat[which(pair[1]==uni),which(pair[2]==uni)]
  }
  ## apply all the instructions simultaneously
  mat<-mat+changes
}

## Output the difference between the most and least common letters
## Use colSums and rowSums as the first and last letters will be off by one in each
## (Stop R using scientific notation)
options(scipen=999)
max(c(rowSums(mat),colSums(mat)))-min(c(rowSums(mat),colSums(mat)))

# Part 2
## Initialise a matrix that counts the number of pairs XY
mat<-matrix(0,length(uni),length(uni))

## Fill matrix with existing pair counts from initial polymer template
for(i in 1:(length(newData)-1)){
  mat[which(newData[i]==uni),which(newData[i+1]==uni)]<-mat[which(newData[i]==uni),which(newData[i+1]==uni)]+1
}

## loop 40 times
for(loops in 1:40){
  ## Create temporary changes matrix
  changes<-matrix(0,length(uni),length(uni))
  for(i in 1:nrow(instruct)){
    ## for each instruction, extract the pairs
    pair<-unlist(strsplit(instruct$V1[i],""))
    ## remove the count of all these pairs in the current polymer template
    changes[which(pair[1]==uni),which(pair[2]==uni)]<-changes[which(pair[1]==uni),which(pair[2]==uni)]-mat[which(pair[1]==uni),which(pair[2]==uni)]
    ## add a count of all the new pairs 
    changes[which(pair[1]==uni),which(instruct$V3[i]==uni)]<-changes[which(pair[1]==uni),which(instruct$V3[i]==uni)]+mat[which(pair[1]==uni),which(pair[2]==uni)]
    changes[which(instruct$V3[i]==uni),which(pair[2]==uni)]<-changes[which(instruct$V3[i]==uni),which(pair[2]==uni)]+mat[which(pair[1]==uni),which(pair[2]==uni)]
  }
  ## apply all the instructions simultaneously
  mat<-mat+changes
}

## Output the difference between the most and least common letters
## Use colSums and rowSums as the first and last letters will be off by one in each
## (Stop R using scientific notation)
options(scipen=999)
max(c(rowSums(mat),colSums(mat)))-min(c(rowSums(mat),colSums(mat)))







####
# Original part 1 code
####

# Part 1
newData<-as.character(data[1,])
for( loops in 1:10){
  origLoc<-1:length(newData)
  tempData<-newData
  for (i in 1:nrow(instruct)){
    pair<-unlist(strsplit(instruct$V1[i],""))
    for (j in 1:(length(tempData)-1)){
      if(tempData[j]==pair[1] & tempData[j+1]==pair[2]){
        k<-which(origLoc==j)
        newData<-c(newData[1:k],instruct$V3[i],newData[(k+1):length(newData)])
        origLoc<-c(origLoc[1:k],F,origLoc[(k+1):length(newData)])
      }
    }
  }
}

tab<-table(newData)
tab<-sort(tab,decreasing = T)
tab[1]-tab[length(tab)]