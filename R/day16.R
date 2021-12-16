# Advent of Code Day 16

## Read in the data
data<-read.table("data/day16.txt")

## Get data in a vector
datavec<-unlist(strsplit(data$V1,""))

# Part 1
## Convert hex vector to a binary vector
convertToBinary<-function(x){
  if(x==0){
    return(c(0,0,0,0))
  } else if(x==1){
    return(c(0,0,0,1))
  } else if(x==2){
    return(c(0,0,1,0))
  } else if(x==3){
    return(c(0,0,1,1))
  } else if(x==4){
    return(c(0,1,0,0))
  } else if(x==5){
    return(c(0,1,0,1))
  } else if(x==6){
    return(c(0,1,1,0))
  } else if(x==7){
    return(c(0,1,1,1))
  } else if(x==8){
    return(c(1,0,0,0))
  } else if(x==9){
    return(c(1,0,0,1))
  } else if(x=="A"){
    return(c(1,0,1,0))
  } else if(x=="B"){
    return(c(1,0,1,1))
  } else if(x=="C"){
    return(c(1,1,0,0))
  } else if(x=="D"){
    return(c(1,1,0,1))
  } else if(x=="E"){
    return(c(1,1,1,0))
  } else if(x=="F"){
    return(c(1,1,1,1))
  }
}

## convert binary vector to decimal
convertfrombin<-function(x){
  len<-length(x)
  dec<-0
  for (i in 1:len){
    dec<-dec+x[len-i+1]*2^(i-1)
  }
  return(dec)
}

## Get the hex data vector as a long binary vector
binvec<-NULL
for (i in 1:length(datavec)){
  binvec<-c(binvec,convertToBinary(datavec[i]))
}

## initialise a temporary version of the vector that will
## shrink as the space is explored
tempvec<-binvec

## initialise list of packets
packets<-list()

## initialise count of packets
nopackets<-0

## function to get packets, given binary vector and packet currently in
getPackets<-function(tempvec,inpacket){
  ## initialise index of sub packets and counts
  countsub<-NULL
  subnumber<-NULL
  ## do until the temporary vector is empty or just contains trailing 0s
  while(length(tempvec)>0){
    if(sum(tempvec==0)==length(tempvec)){
      break
    }
    ## increase count of packets
    nopackets<<-nopackets+1
    ## record packet number
    packets[[nopackets]]<<-nopackets
    ## record the number of the packet this packet is inside
    if(length(countsub)>0){
      packets[[nopackets]]$inpacket<<-subnumber[length(subnumber)]
      countsub[length(countsub)]<-countsub[length(countsub)]-1
      if(countsub[length(countsub)]==0){
        countsub<-countsub[-length(countsub)]
        subnumber<-subnumber[-length(subnumber)]
      }
    } else {
      packets[[nopackets]]$inpacket<<-inpacket
    }
    ## Record version and type ID of the packet
    packets[[nopackets]]$version<<-convertfrombin(tempvec[1:3])
    packets[[nopackets]]$typeID<<-convertfrombin(tempvec[4:6])
    if (packets[[nopackets]]$typeID==4){
      ## If type ID = 4 then record the binary string to decode later
      i<-0
      while(tempvec[7+i*5]==1){
        i<-i+1
      }
      packets[[nopackets]]$binnumber<<-tempvec[7:(11+i*5)]
      ## remove packet from binary vector
      tempvec<-tempvec[-c(1:(11+i*5))]
    } else {
      ## Record length type ID
      packets[[nopackets]]$lengthtypeID<<-tempvec[7]
      
      if(packets[[nopackets]]$lengthtypeID==0){
        ## Record packet length
        packets[[nopackets]]$packetlength<<-convertfrombin(tempvec[8:22])
        ## Record binary string containing subpackets
        packets[[nopackets]]$subpackets<<-tempvec[23:(22+packets[[nopackets]]$packetlength)]
        ## run recursively to get subpackets using binary sting above
        templength<-22+packets[[nopackets]]$packetlength
        newtemp<-packets[[nopackets]]$subpackets
        packetno<-unname(unlist(packets[[nopackets]][1]))
        getPackets(newtemp,packetno)
        ## remove packet and sub packets from binary vector
        tempvec<-tempvec[-c(1:templength)]
      } else {
        ## record number of subpackets
        packets[[nopackets]]$nosubpackets<<-convertfrombin(tempvec[8:18])
        ## remove packet from vector
        tempvec<-tempvec[-c(1:18)]
        ## continue on to the next packet, which is actually a sub
        ## packet of this one. Record this by storing this packet's number
        ## and the count of sub packets to come
        countsub<-c(countsub,packets[[nopackets]]$nosubpackets)
        subnumber<-c(subnumber,unname(unlist(packets[[nopackets]][1])))
      }
    }
  }
}

## Get the packets
getPackets(tempvec,0)

## Get the sum of version numbers
result<-sum(unlist(lapply(packets,function(x)x$version)))
result


# Part 2
## Convert type ID 4 packet binary strings to values
converttype4<-function(x){
  remove<-length(x)/5
  for(i in remove:1){
    x<-x[-c(5*i-4)]
  }
  return(convertfrombin(x))
}

## Find the values for all type ID 4 packets
for(i in 1:length(packets)){
  if (packets[[i]]$typeID==4){
    packets[[i]]$value<-converttype4(packets[[i]]$binnumber)
  }
}

## Evalute packet values. Keep going until packet 1 has a value
while(is.null(packets[[1]]$value)){
  ## for each packet attempt to find its value. If a subpacket doesn't have a value,
  ## skip this packet for now
  for (i in 1:length(packets)){
    if (is.null(packets[[i]]$value)){
      if (packets[[i]]$typeID==0){
        #Packets with type ID 0 are sum packets - their value is the sum of the values of their sub-packets. If they only have a single sub-packet, their value is the value of the sub-packet.      su
        sumpackets<-0
        for(j in i:length(packets)){
          if(packets[[j]]$inpacket==i){
            if(is.null(packets[[j]]$value)){
              sumpackets<-NA
              break
            } else {
              sumpackets<-sumpackets+packets[[j]]$value
            }
          }
        }
        if (is.na(sumpackets)==F){
          packets[[i]]$value<-sumpackets
        }
      }
      if (packets[[i]]$typeID==1){
        #Packets with type ID 1 are product packets - their value is the result of multiplying together the values of their sub-packets. If they only have a single sub-packet, their value is the value of the sub-packet.
        prodpackets<-1
        for(j in i:length(packets)){
          if(packets[[j]]$inpacket==i){
            if(is.null(packets[[j]]$value)){
              prodpackets<-NA
              break
            } else {
              prodpackets<-prodpackets*packets[[j]]$value
            }
          }
        }
        if (is.na(prodpackets)==F){
          packets[[i]]$value<-prodpackets
        }
      }    
      
      if (packets[[i]]$typeID==2){
        minval<-Inf
        #Packets with type ID 2 are minimum packets - their value is the minimum of the values of their sub-packets.
        for(j in i:length(packets)){
          if(packets[[j]]$inpacket==i){
            if(is.null(packets[[j]]$value)){
              minval<-NA
              break
            } else {
              if (packets[[j]]$value<minval){
                minval<-packets[[j]]$value
              }
            }
          }
        }
        if (is.na(minval)==F){
          packets[[i]]$value<-minval
        }
      } 
      if (packets[[i]]$typeID==3){
        #Packets with type ID 3 are maximum packets - their value is the maximum of the values of their sub-packets.
        maxval<-0
        for(j in i:length(packets)){
          if(packets[[j]]$inpacket==i){
            if(is.null(packets[[j]]$value)){
              maxval<-NA
              break
            } else {
              if (packets[[j]]$value>maxval){
                maxval<-packets[[j]]$value
              }
            }
          }
        }
        if (is.na(maxval)==F){
          packets[[i]]$value<-maxval
        }
      }   
      if (packets[[i]]$typeID==5){
        #Packets with type ID 5 are greater than packets - their value is 1 if the value of the first sub-packet is greater than the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.
        j<-i+2
        while(packets[[j]]$inpacket!=i){
          j<-j+1
        }
        if(is.null(packets[[i+1]]$value)==F & is.null(packets[[j]]$value)==F){
          if (packets[[i+1]]$value>packets[[j]]$value){
            packets[[i]]$value<-1
          } else {
            packets[[i]]$value<-0
          }
        }
      }     
      if (packets[[i]]$typeID==6){
        #Packets with type ID 6 are less than packets - their value is 1 if the value of the first sub-packet is less than the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.
        j<-i+2
        while(packets[[j]]$inpacket!=i){
          j<-j+1
        }
        if(is.null(packets[[i+1]]$value)==F & is.null(packets[[j]]$value)==F){
          if (packets[[i+1]]$value<packets[[j]]$value){
            packets[[i]]$value<-1
          } else {
            packets[[i]]$value<-0
          }
        }
      }   
      if (packets[[i]]$typeID==7){
        #Packets with type ID 7 are equal to packets - their value is 1 if the value of the first sub-packet is equal to the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.
        j<-i+2
        while(packets[[j]]$inpacket!=i){
          j<-j+1
        }
        if(is.null(packets[[i+1]]$value)==F & is.null(packets[[j]]$value)==F){
          if (packets[[i+1]]$value==packets[[j]]$value){
            packets[[i]]$value<-1
          } else {
            packets[[i]]$value<-0
          }
        }
      }    
    }
  }
}

## Output packet 1's value (tell R not to use scientific notation)
options(scipen=999)
packets[[1]]$value





















###############################################################################

