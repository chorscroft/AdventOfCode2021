# Advent of Code Day 11

## Read in the data
data<-read.fwf("data/day11.txt",rep(1,10))

# Part 1
## initialise flash count
flashes<-0

## loop for 100 steps
for (step in 1:100){
  ## increase each octopus by 1
  data<-data+1
  ## initialise flashed matrix with False for each octopus
  flashed<-matrix(F,10,10)
  ## initialise indicator of change
  nochange<-F
  ## loop until no changes occur
  while(nochange==F){
    ## assume no changes will occur
    nochange<-T
    ## for each octopus, see if it flashes
    for(i in 1:10){
      for(j in 1:10){
        ## if it hasn't flashed yet and has reached the threshold, flash
        if (flashed[i,j]==F & data[i,j]>=10){
          ## record it has flashed
          flashed[i,j]<-T
          ## record a change has occured
          nochange<-F
          ## increase each adjacent octopus
          if (i>1 & j>1){
            data[i-1,j-1]<-data[i-1,j-1]+1
          }
          if (i>1){
            data[i-1,j]<-data[i-1,j]+1
          }
          if (j>1){
            data[i,j-1]<-data[i,j-1]+1
          }
          if (i>1 & j<10){
            data[i-1,j+1]<-data[i-1,j+1]+1
          }
          if (i<10 & j<10){
            data[i+1,j+1]<-data[i+1,j+1]+1
          }
          if (i<10){
            data[i+1,j]<-data[i+1,j]+1
          }
          if (j<10){
            data[i,j+1]<-data[i,j+1]+1
          }
          if (i<10 & j>1){
            data[i+1,j-1]<-data[i+1,j-1]+1
          }
        }
      }
    }
  }
  ## count the number of flashes
  flashes<-flashes+sum(flashed)
  ## reset each octopus that flashed to 0
  data[flashed]<-0
}

## Return result
flashes


# Part 2
## Read in the data
data<-read.fwf("data/day11.txt",rep(1,10))

## initialise flash count
flashes<-0

## initialise step count
step<-0
## loop until simultaneous flash
while(T){
  ## increase step count
  step<-step+1
  ## increase each octopus by 1
  data<-data+1
  ## initialise flashed matrix with False for each octopus
  flashed<-matrix(F,10,10)
  ## initialise indicator of change
  nochange<-F
  ## loop until no changes occur
  while(nochange==F){
    ## assume no changes will occur
    nochange<-T
    ## for each octopus, see if it flashes
    for(i in 1:10){
      for(j in 1:10){
        ## if it hasn't flashed yet and has reached the threshold, flash
        if (flashed[i,j]==F & data[i,j]>=10){
          ## record it has flashed
          flashed[i,j]<-T
          ## record a change has occured
          nochange<-F
          ## increase each adjacent octopus
          if (i>1 & j>1){
            data[i-1,j-1]<-data[i-1,j-1]+1
          }
          if (i>1){
            data[i-1,j]<-data[i-1,j]+1
          }
          if (j>1){
            data[i,j-1]<-data[i,j-1]+1
          }
          if (i>1 & j<10){
            data[i-1,j+1]<-data[i-1,j+1]+1
          }
          if (i<10 & j<10){
            data[i+1,j+1]<-data[i+1,j+1]+1
          }
          if (i<10){
            data[i+1,j]<-data[i+1,j]+1
          }
          if (j<10){
            data[i,j+1]<-data[i,j+1]+1
          }
          if (i<10 & j>1){
            data[i+1,j-1]<-data[i+1,j-1]+1
          }
        }
      }
    }
  }
  ## If each octopus flashed, print the step number and stop loop
  if(sum(flashed)==100){
    print.default(step)
    break
  }
  ## reset each octopus that flashed to 0
  data[flashed]<-0
}