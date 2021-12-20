# Advent of Code Day 20

## Read in the data
data<-read.fwf("data/day20.txt",rep(1,100),skip=2,comment.char="")
img_enhance<-read.fwf("data/day20.txt",rep(1,512),nrows=1,comment.char="")

## Convert data to 0s and 1s
data[data=="#"]<-1
data[data=="."]<-0
img_enhance[img_enhance=="#"]<-1
img_enhance[img_enhance=="."]<-0

data<-data.frame(lapply(data,as.numeric))
img_enhance<-as.numeric(img_enhance)

# Part 1
## convert binary vector to decimal
convertfrombin<-function(x){
  len<-length(x)
  dec<-0
  for (i in 1:len){
    dec<-dec+x[len-i+1]*2^(i-1)
  }
  return(dec)
}

## Run the enhancement n_loops times on data given the image enhancement algorithm
doEnhancement<-function(data,img_enhance,n_loops){
  ## Run enhancement n_loops number of times
  for (loops in 1:n_loops){
    ## pad with 5 0s or 1s alternately
    if (loops %% 2 == 1){
      data<-cbind(rep(0,nrow(data)),rep(0,nrow(data)),rep(0,nrow(data)),rep(0,nrow(data)),rep(0,nrow(data)),data,rep(0,nrow(data)),rep(0,nrow(data)),rep(0,nrow(data)),rep(0,nrow(data)),rep(0,nrow(data)))
      data<-rbind(rep(0,ncol(data)),rep(0,ncol(data)),rep(0,ncol(data)),rep(0,ncol(data)),rep(0,ncol(data)),data,rep(0,ncol(data)),rep(0,ncol(data)),rep(0,ncol(data)),rep(0,ncol(data)),rep(0,ncol(data)))
    } else {
      data<-cbind(rep(1,nrow(data)),rep(1,nrow(data)),rep(1,nrow(data)),rep(1,nrow(data)),rep(1,nrow(data)),data,rep(1,nrow(data)),rep(1,nrow(data)),rep(1,nrow(data)),rep(1,nrow(data)),rep(1,nrow(data)))
      data<-rbind(rep(1,ncol(data)),rep(1,ncol(data)),rep(1,ncol(data)),rep(1,ncol(data)),rep(1,ncol(data)),data,rep(1,ncol(data)),rep(1,ncol(data)),rep(1,ncol(data)),rep(1,ncol(data)),rep(1,ncol(data)))
    }
   
    ## initialise the new Image as all 0s
    newImage<-matrix(0,ncol(data),ncol(data))
    
    ## for each point, calcualte its new state
    for (i in 1:(ncol(data))){
      for (j in 1:(ncol(data))){
        ## if it is an edge point, reverse the state it is in
        if (i == 1 | i == ncol(data) | j ==1 | j == ncol(data)){
          newImage[i,j]<-1-data[i,j]
        } else {
          ## use the 9 data points in the square around the point in question
          ## to find the relevent position in the image enhancement algorithm
          ## then look up the new point state in the algorithm and change it in 
          ## the new image
          myBinary<-c(data[i-1,j-1],data[i-1,j],data[i-1,j+1],data[i,j-1],data[i,j],data[i,j+1],data[i+1,j-1],data[i+1,j],data[i+1,j+1])
          dec<-convertfrombin(myBinary)
          newImage[i,j]<-as.numeric(img_enhance[dec+1])
        }
      }
    }
    ## once all points have been updated, replace the data with the new image
    data<-newImage
  }
  return(data)
}

## Return the number of lit points after looping twice
new_data<-doEnhancement(data,img_enhance,2)
sum(new_data)

# Part 2
## Return the number of lit points after looping 50 times
new_data<-doEnhancement(data,img_enhance,50)
sum(new_data)