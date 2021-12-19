# Advent of Code Day 18

## Read in the data
data<-read.table("data/day18.txt")

# Part 1
## Checks if a sum is going to explode and returns the
## location of the "[" bracket triggering the explosion
## or 0 if there is no explosion
checkexplode<-function(mySum){
  ## assume no explosion
  explode<-F
  ## count number of nested sums
  count<-0
  for (j in 1:nchar(mySum)){
    ## increase count
    if (substr(mySum,j,j)=="["){
      count<-count+1
    ## closed bracket, so decrease count
    } else if (substr(mySum,j,j)=="]"){
      count<-count-1
    }
    ## if 5 nested sums then explosion happens
    if (count == 5){
      explode<-T
      break
    }
  }
  ## return location of explosion
  if (explode==T){
    return(j)
  } else {
    return(0)
  }
}

## Does the explosion given the sum and the location of the 
## explosion. Returns the new sum.
doexplode<-function(mySum,j){
  ## Find values from the exploding pair
  leftValue<-""
  k<-1
  while(substr(mySum,j+k,j+k) != ","){
    leftValue<-paste0(leftValue,substr(mySum,j+k,j+k))
    k<-k+1
  }
  leftValue<-as.numeric(leftValue)
  rightValue<-""
  k<-k+1
  while(substr(mySum,j+k,j+k) != "]"){
    rightValue<-paste0(rightValue,substr(mySum,j+k,j+k))
    k<-k+1
  }  
  rightValue<-as.numeric(rightValue)
  ## replace exploding pair with 0
  mySum<-paste0(substr(mySum,1,j-1),"0",substr(mySum,j+k+1,nchar(mySum)))
  ## add the right value to the nearest value to the right
  for(i in (j+1):nchar(mySum)){
    if (substr(mySum,i,i) %in% c(0:9)){
      newVal<-substr(mySum,i,i)
      k<-1
      while(substr(mySum,i+k,i+k) %in% c(0:9)){
        newVal<-paste0(newVal,substr(mySum,i+k,i+k))
        k<-k+1
      }
      newVal<-as.numeric(newVal)
      mySum<-paste0(substr(mySum,1,i-1),newVal+rightValue,substr(mySum,i+k,nchar(mySum)))
      break
    }
  }  
  # add the left value to the nearest value to the left
  for(i in (j-1):1){
    if (substr(mySum,i,i) %in% c(0:9)){
      newVal<-substr(mySum,i,i)
      k<-1
      while(substr(mySum,i-k,i-k) %in% c(0:9)){
        newVal<-paste0(substr(mySum,i-k,i-k),newVal)
        k<-k+1
      }
      newVal<-as.numeric(newVal)
      mySum<-paste0(substr(mySum,1,i-k),newVal+leftValue,substr(mySum,i+1,nchar(mySum)))
      break
    }
  }
  return(mySum)
}
## Check if there is a spli that needs to be done
## returns the location if there is, or a 0 if not
checksplit<-function(mySum){
  ## assume no split
  split<-F
  ## initialise count of consecutive digits
  count<-0
  ## look along the sum for consecutive digits
  for (j in 1:nchar(mySum)){
    if (substr(mySum,j,j) %in% c(0:9)){
      count<-count+1
    } else {
      count<-0
    }
    if (count == 2){
      split<-T
      break
    }
  }
  ## if there were consecutive digits, return the location
  ## otherwise return 0
  if (split==T){
    return(j)
  } else {
    return(0)
  }
}

## Do the split
dosplit<-function(mySum,j){
  ## Get the number to split
  myNumber<-as.numeric(substr(mySum,j-1,j))
  ## split the number in two and insert into the sum
  mySum<-paste0(substr(mySum,1,j-2),"[",floor(myNumber/2),",",ceiling(myNumber/2),"]",substr(mySum,j+1,nchar(mySum)))
  return(mySum)
}

## Calculate the magnitude of the sum
getMagnitude<-function(mySum){
  ## split the sum into a vector for each character
  myVec<-unlist(strsplit(mySum,""))
  ## loop until you only have a single numbr remaininh 
  repeat{
    ## find a close bracket
    j<-which(myVec=="]")[1]
    ## if it is the last close bracket, assess the magnitude and exit loop
    if (j == length(myVec)){
      return(3*as.numeric(myVec[2])+2*as.numeric(myVec[4]))
      ## assess magnitude of single pair and remove brackets either side
    } else {
      myVec<-c(myVec[1:(j-5)],3*as.numeric(myVec[j-3])+2*as.numeric(myVec[j-1]),myVec[(j+1):length(myVec)])
    }
  }
}

## Get the line of the sum 
mySum<-data$V1[1]

## Add each line on and explode/split until no more can be done, then
## add the next line
for (i in 2:nrow(data)){
  ## add next line to sum
  mySum<-paste0("[",mySum,",",data$V1[i],"]")
  ## do explodes and splits
  repeat{
    ## assume this is final loop
    finish<-T
    ## Check for explosion
    temp<-checkexplode(mySum)
    ## do explosion if check was positive
    if (temp>0){
      mySum<-doexplode(mySum,temp)
      finish<-F
    }
    
    ## if an explosion didn't happen, check for split
    if (finish==T){
      ## check for split
      temp<-checksplit(mySum)
      ## do split if check was positive
      if (temp>0){
        mySum<-dosplit(mySum,temp)
        finish<-F
      }
    }
    ## if no explosions or splits, exit loop
    if (finish==T){
      break
    }
  }
}

## Calculate final magnitude
getMagnitude(mySum)


# Part 2
## Initialise the maximum magnitude as 0
maxmag<-0

## Assess magnitude for each pair of sums
for (i in 1:100){
  for (j in 1:100){
    if (i != j){
      ## get the sum of pairs of lines
      mySum<-paste0("[",data$V1[i],",",data$V1[j],"]")
      ## do explodes and splits
      repeat{
        finish<-T
        ## Explode
        temp<-checkexplode(mySum)
        if (temp>0){
          mySum<-doexplode(mySum,temp)
          finish<-F
        }
        
        if (finish==T){
          ## Split
          temp<-checksplit(mySum)
          if (temp>0){
            mySum<-dosplit(mySum,temp)
            finish<-F
          }
        }
        if (finish==T){
          break
        }
      }
    }
    ## get magnitude
    mag<-getMagnitude(mySum)
    ## if magnitue is the biggest seen so far, record it
    if (mag>maxmag){
      maxmag<-mag
    }
  }
}

## output the largest magnitude
maxmag