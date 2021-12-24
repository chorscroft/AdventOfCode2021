# Advent of Code Day 24

## Read in the data
data<-read.table("data/day24.txt",sep="#")

# Part 1
## Function to convert wxyz to 1234
convertLtoN<-function(L){
  if (L=="w"){
    return(1)
  } else if (L=="x"){
    return(2)
  } else if (L=="y"){
    return(3)
  } else if (L=="z"){
    return(4)
  }
  return[NA]
}

## Initialise list of the 14 data input secctions
data_inps<-vector("list",14)

## initialise count of input sections
inp_count<-0

## assign each row of the data to input sections
for(i in 1:nrow(data)){
  line<-unlist(strsplit(data$V1[i]," "))
  ## if it is in input line, begin next section
  if(line[1]=="inp"){
    inp_count<-inp_count+1
    ## each section contains a data frame with the instruction and the two inputs
    data_inps[[inp_count]]<-data.frame(inst=NULL,a=NULL,b=NULL)
  } else {
    ## add line to data frame
    data_inps[[inp_count]]<-rbind(data_inps[[inp_count]],line)
  }
}

## initialise model number as all 9s
model_no<-rep(9,14)

## initialise store of wxyz values at the end of each input section
vec_wxyz_store<-matrix(0,nrow=14,ncol=4)

## start at input section 1
inp_min<-1

## initilaise w, x, y and z values
vec_wxyz<-c(0,0,0,0)

## repeat until a valid model number is found
repeat{
  ## start at the minimum input section
  inp_count<-inp_min
  ## loop over all input section from minimum upwards
  while (inp_count<=14){
    ## do the instructions for an input section (maybe repeat if the section fails)
    repeat{
      ## initialse w value from the model number
      vec_wxyz[1]<-model_no[inp_count]
      ##execute each line of the instructions for this section
      for(i in 1:nrow(data_inps[[inp_count]])){
        ## get line
        line<-data_inps[[inp_count]][i,]
        ## do addition
        if(line[1]=="add"){
          L<-convertLtoN(line[2])
          if(line[3] %in% c("w","x","y","z")){
            L2<-convertLtoN(line[3])
            b<-vec_wxyz[L2]
          } else {
            b<-as.numeric(line[3])
          }
          vec_wxyz[L]<-vec_wxyz[L]+b    
          
          ## do multiplication
        }else if(line[1]=="mul"){
          L<-convertLtoN(line[2])
          if(line[3] %in% c("w","x","y","z")){
            L2<-convertLtoN(line[3])
            b<-vec_wxyz[L2]
          } else {
            b<-as.numeric(line[3])
          }
          vec_wxyz[L]<-vec_wxyz[L]*b
          
          ## do division
        } else if(line[1]=="div"){
          L<-convertLtoN(line[2])
          if(line[3] %in% c("w","x","y","z")){
            L2<-convertLtoN(line[3])
            b<-vec_wxyz[L2]
          } else {
            b<-as.numeric(line[3])
          }
          vec_wxyz[L]<-floor(vec_wxyz[L]/b)
          
          ## find modulus
        } else if(line[1]=="mod"){
          
          L<-convertLtoN(line[2])
          if(line[3] %in% c("w","x","y","z")){
            L2<-convertLtoN(line[3])
            b<-vec_wxyz[L2]
          } else {
            b<-as.numeric(line[3])
          }
          vec_wxyz[L]<-vec_wxyz[L] %% b
          
          ## assess equality
        } else if(line[1]=="eql"){
          L<-convertLtoN(line[2])
          if(line[3] %in% c("w","x","y","z")){
            L2<-convertLtoN(line[3])
            b<-vec_wxyz[L2]
          } else {
            b<-as.numeric(line[3])
          }
          if(vec_wxyz[L]==b){
            vec_wxyz[L]<-1
          } else {
            vec_wxyz[L]<-0
          }
        }
      }
      
      ## sections 4, 6, 7, 11, 12, 13 and 14 MUST rescult in x = 0
      if (inp_count %in% c(4,6,7,11,12,13,14) && vec_wxyz[2]!=0){
        ## if it doesn't, reduce the model number
        repeat{
          ## check if reached all 1s in model number
          if (sum(model_no==rep(1,14))==14){
            stop("fail")
          } else {
            ## reduce model number
            model_no[inp_count]<-model_no[inp_count]-1
            ## if model number reaches 0 then go to previous input section and repeat
            if(model_no[inp_count]==0){
              model_no[inp_count]<-9
              inp_count<-inp_count-1
            } else {
              ## start the input count iteration at the new input section
              inp_min<-inp_count
              ## if it isn't the first section, initialise with the stored w,x,y,z values
              if(inp_count >1){
                vec_wxyz<-vec_wxyz_store[inp_count-1,]
                ## otherwise initialise with 0s
              } else {
                vec_wxyz<-c(0,0,0,0)
              }
              break
            }
          }
        }
        
      } else {
        ## store final w, x, y, and z values for this input section
        vec_wxyz_store[inp_count,]<-vec_wxyz
        break
      }
    }
    ## go to next input section
    inp_count<-inp_count+1
  }
  
  ## if the final z value is 0 then finish the algorithm
  if(vec_wxyz_store[14,4]==0){
    break
  } else {
    ## otherwise, starting at the end of the model number, reduce the model number
    i<-14
    repeat{
      ## if model number is all 1s, stop
      if (sum(model_no==rep(1,14))==14){
        stop("fail")
      } else {
        ## decrease model number
        model_no[i]<-model_no[i]-1
        ## if the model number is too low, go to the previous model number
        if(model_no[i]==0){
          model_no[i]<-9
          i<-i-1
        } else {
          ## update new section to start at
          inp_min<-i
          ## if it isn't the first section, initialise with the stored w,x,y,z values
          if(i >1){
            vec_wxyz<-vec_wxyz_store[i-1,]
            ## otherwise initialise with 0s
          } else {
            vec_wxyz<-c(0,0,0,0)
          }
          break
        }
      }
    }
  }
  
}

## output the final model number
finalResult<-""
for(i in 1:14){
  finalResult<-paste0(finalResult,model_no[i])
}
finalResult


# Part 2
## same as before but reverse the model number to increment rather than reduce
model_no<-rep(1,14)
vec_wxyz_store<-matrix(0,nrow=14,ncol=4)

inp_min<-1
vec_wxyz<-c(0,0,0,0)
repeat{
  inp_count<-inp_min
  while (inp_count<=14){
    repeat{
      vec_wxyz[1]<-model_no[inp_count]
      for(i in 1:nrow(data_inps[[inp_count]])){
        line<-data_inps[[inp_count]][i,]
        if(line[1]=="add"){
          L<-convertLtoN(line[2])
          if(line[3] %in% c("w","x","y","z")){
            L2<-convertLtoN(line[3])
            b<-vec_wxyz[L2]
          } else {
            b<-as.numeric(line[3])
          }
          vec_wxyz[L]<-vec_wxyz[L]+b    
          
          
        }else if(line[1]=="mul"){
          L<-convertLtoN(line[2])
          if(line[3] %in% c("w","x","y","z")){
            L2<-convertLtoN(line[3])
            b<-vec_wxyz[L2]
          } else {
            b<-as.numeric(line[3])
          }
          vec_wxyz[L]<-vec_wxyz[L]*b
          
          
        } else if(line[1]=="div"){
          L<-convertLtoN(line[2])
          if(line[3] %in% c("w","x","y","z")){
            L2<-convertLtoN(line[3])
            b<-vec_wxyz[L2]
          } else {
            b<-as.numeric(line[3])
          }
          vec_wxyz[L]<-floor(vec_wxyz[L]/b)
          
          
        } else if(line[1]=="mod"){
          L<-convertLtoN(line[2])
          if(line[3] %in% c("w","x","y","z")){
            L2<-convertLtoN(line[3])
            b<-vec_wxyz[L2]
          } else {
            b<-as.numeric(line[3])
          }
          vec_wxyz[L]<-vec_wxyz[L] %% b
          
        } else if(line[1]=="eql"){
          L<-convertLtoN(line[2])
          if(line[3] %in% c("w","x","y","z")){
            L2<-convertLtoN(line[3])
            b<-vec_wxyz[L2]
          } else {
            b<-as.numeric(line[3])
          }
          if(vec_wxyz[L]==b){
            vec_wxyz[L]<-1
          } else {
            vec_wxyz[L]<-0
          }
        }
      }
      
      if (inp_count %in% c(4,6,7,11,12,13,14) && vec_wxyz[2]!=0){
        repeat{
          if (sum(model_no==rep(9,14))==14){
            stop("fail")
          } else {
            model_no[inp_count]<-model_no[inp_count]+1
            if(model_no[inp_count]==10){
              model_no[inp_count]<-1
              inp_count<-inp_count-1
            } else {
              inp_min<-inp_count
              if(inp_count >1){
                vec_wxyz<-vec_wxyz_store[inp_count-1,]
              } else {
                vec_wxyz<-c(0,0,0,0)
              }
              break
            }
          }
        }
        
      } else {
        vec_wxyz_store[inp_count,]<-vec_wxyz
        break
      }
    }
    inp_count<-inp_count+1
  }
  
  
  if(vec_wxyz_store[14,4]==0){
    break
  } else {
    i<-14
    repeat{
      if (sum(model_no==rep(9,14))==14){
        stop("fail")
      } else {
        model_no[i]<-model_no[i]+1
        if(model_no[i]==10){
          model_no[i]<-1
          i<-i-1
        } else {
          inp_min<-i
          if(i >1){
            vec_wxyz<-vec_wxyz_store[i-1,]
          } else {
            vec_wxyz<-c(0,0,0,0)
          }
          break
        }
      }
    }
  }
  
}

finalResult<-""
for(i in 1:14){
  finalResult<-paste0(finalResult,model_no[i])
}
finalResult
