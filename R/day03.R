# Advent of Code Day 3

## Read in the data
data<-read.fwf("data/day03.txt",rep(1,12))

# Part 1
## Initialise gamma and epsilon
gamma<-rep(0,12)
epsilon<-rep(0,12)

## for each position find the most common bit, then
## define gamma and epsilon based on that
for (pos in 1:12){
  if (sum(data[,pos]==1)>=nrow(data)/2){
    gamma[pos]<-1
    epsilon[pos]<-0
  }  else{
    gamma[pos]<-0
    epsilon[pos]<-1
  }
}

## Convert a binary vector to decimal
converttobin<-function(x){
  len<-length(x)
  dec<-0
  for (i in 1:len){
    dec<-dec+x[len-i+1]*2^(i-1)
  }
  return(dec)
}

## Convert gamma and epsilon to decimal
deceps<-converttobin(epsilon)
decgam<-converttobin(gamma)

## Get result
deceps*decgam

# Part 2
## Initialise table for oxygen generator rating
oxy_tab<-data

## Iterate over positions until one row is left
pos<-1
while (nrow(oxy_tab)>1){
  if (sum(oxy_tab[,pos]=="1")>=nrow(oxy_tab)/2){
    oxy_tab<-oxy_tab[oxy_tab[,pos]=="1",]
  } else{
    oxy_tab<-oxy_tab[oxy_tab[,pos]=="0",]
  }
  pos<-pos+1
}

## Convert to decimal
dec_oxy<-converttobin(as.numeric(oxy_tab))

## Initialise table for CO2 scrubber rating
co2_tab<-data

## Iterate over positions until one row is left
pos<-1
while (nrow(co2_tab)>1){
  if (sum(co2_tab[,pos]=="1")>=nrow(co2_tab)/2){
    co2_tab<-co2_tab[co2_tab[,pos]=="0",]
  } else{
    co2_tab<-co2_tab[co2_tab[,pos]=="1",]
  }
  pos<-pos+1
}

## Convert to decimal
dec_co2<-converttobin(as.numeric(co2_tab))

## Get result
dec_co2*dec_oxy

