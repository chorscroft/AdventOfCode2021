# Advent of Code Day 8

## Read in the data
data<-read.table("data/day08.txt")

# Part 1
## Get the code lengths for each of the result digits
lens<-apply(data[,12:15],c(1,2),nchar)

## Add all of the digits which must be a 1, 4, 7 or 8,
## which have lengths 2, 4, 3, and 7 respectively
sum(lens==2)+sum(lens==4)+sum(lens==3)+sum(lens==7)

# Part 2
## Numeric positions on display:
##   111
##  2   3
##  2   3
##   444
##  5   6
##  5   6
##   777

## Function to map the letters to numeric positions
## based on the 10 signal values given
decodeCodes<- function(todecode){
  positions<-matrix(T,7,7)
  # Find mappings that cannot be true
  for(i in 1:length(todecode)){
    temp_vec<-unlist(strsplit(todecode[,i],""))
    temp_vec_num<-sapply(temp_vec,function(x)which(letters==x))
    if(length(temp_vec)==2){
      positions[temp_vec_num,c(1,2,4,5,7)]<-F
    } else if(length(temp_vec)==4){
      positions[temp_vec_num,c(1,5,7)]<-F
    } else if(length(temp_vec)==3){
      positions[temp_vec_num,c(2,4,5,7)]<-F
    } else if(length(temp_vec)==6){
      notin<-which((c(1:7) %in% temp_vec_num)==F)
      positions[notin,c(1,2,6,7)]<-F
    } else if(length(temp_vec)==5){
      notin<-which((c(1:7) %in% temp_vec_num)==F)
      positions[notin,c(1,4,7)]<-F
    }
  }
  # From the matrix given, logic out the final mapping
  while(sum(positions)>7){
    for (i in 1:7){
      if (sum(positions[i,])==1){
        positions[-i,which(positions[i,]==T)]<-F
      }
      if (sum(positions[,i])==1){
        positions[which(positions[,i]==T),-i]<-F
      }
    }
  }
  # Vector of mappings to use for decoding
  locs<-unlist(apply(positions,1,function(x)which(x==T)))
  return(locs)
}

## Returns the output value given the four output values and
## the mapping of letters to digit positions
getResult<-function(locs,result){
  code<-""
  for(i in 1:4){
    temp_vec<-unlist(strsplit(result[,i],""))
    temp_vec_num<-sapply(temp_vec,function(x)which(letters==x))
    if(length(temp_vec_num)==2){
      code<-paste0(code,1)
    } else if (length(temp_vec_num)==4){
      code<-paste0(code,4)
    } else if (length(temp_vec_num)==3){
      code<-paste0(code,7)
    } else if (length(temp_vec_num)==7){
      code<-paste0(code,8)
    } else if (length(temp_vec_num)==6){
      notin<-which((c(1:7) %in% temp_vec_num)==FALSE)
      if(locs[notin]==5){
        code<-paste0(code,9)
      } else if(locs[notin]==4){
        code<-paste0(code,0)
      } else if(locs[notin]==3){
        code<-paste0(code,6)
      }
    } else if (length(temp_vec_num)==5){
      notin<-which((c(1:7) %in% temp_vec_num)==FALSE)
      if(sum(sort(locs[notin]) == c(2,6))==2){
        code<-paste0(code,2)
      } else if(sum(sort(locs[notin]) == c(2,5))==2){
        code<-paste0(code,3)
      } else if(sum(sort(locs[notin]) == c(3,5))==2){
        code<-paste0(code,5)
      }
    }
  }
  return(as.numeric(code))
}

## Initialise summing variable
finalresult<-0

## For each line, determine the output values and add them together
for (i in 1:nrow(data)){
  # Get signal patterns
  todecode<-data[i,1:10]
  # Get output digits
  result<-data[i,12:15]
  
  # Work out mapping of letters to positions
  locs<-decodeCodes(todecode)
  # Work out output value
  toadd<-getResult(locs,result)
  # Add output value to final sum
  finalresult<-finalresult+toadd
}
finalresult

  