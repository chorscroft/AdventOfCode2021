# Advent of Code Day 21

## Read in the data
data<-read.table("data/day21.txt")

# Part 1
## Get the starting positions for the players
p1pos<-data$V5[1]
p2pos<-data$V5[2]

## initialise the scores at 0
score1<-0
score2<-0

## initialise the dice
dice<-0

## start with player 1
pturn<-1

## Calculate the new position given the start position and
## the sum of the dice rolls
newpos<-function(pos,dice){
  temp<-pos+dice
  while(temp>10){
    temp<-temp-10
  }
  return(temp)
}

## Play until someone gets over 1000 points
while (score1 <1000 & score2 <1000){
  ## if it's player 1's turn
  if(pturn==1){
    ## get new position
    p1pos<-newpos(p1pos,dice+1+dice+2+dice+3)
    ## get new score
    score1<-score1+p1pos
    ## record the state of the dice
    dice<-dice+3
    ## switch to player 2's turn
    pturn<-2
  } else {
    ## get new position
    p2pos<-newpos(p2pos,dice+1+dice+2+dice+3)
    ## get new score
    score2<-score2+p2pos
    ## record the state of the dice
    dice<-dice+3
    ## switch to player 2's turn
    pturn<-1
  }
}

## Return the product of the final dice value and the loser's score
if (score2>=1000){
  score1*dice
} else {
  score2*dice
}


# Part 2
## Get the starting positions for the players
p1pos<-data$V5[1]
p2pos<-data$V5[2]

## initialise list of turns for the players
## Each turn will contain a matrix containing the number of universes
## that the player ends the turn in a particular position with a specific score
turns1<-list()
turns2<-list()

## Initialise count of wins
p1<-0
p2<-0

## For player 1:
## Initialise the position / score matrix for turn 1
turns1[[1]]<-matrix(0,10,21)
## loop for each dice combination in turn 1
for(i in 1:3){
  for(j in 1:3){
    for(k in 1:3){
      ## find the new position
      temp<-newpos(p1pos,i+j+k)
      ## find the new score
      score<-temp
      ## if score is more than 21 count it as 21
      if(temp>21){
        temp<-21
      }
      ## record the final position and score 
      turns1[[1]][temp,score]<-turns1[[1]][temp,score]+1
    }
  }
}

## For player 2:
## Initialise the position / score matrix for turn 1
turns2[[1]]<-matrix(0,10,21)
## loop for each dice combination in turn 1
for(i in 1:3){
  for(j in 1:3){
    for(k in 1:3){
      ## find the new position
      temp<-newpos(p2pos,i+j+k)
      ## find the new score
      score<-temp
      ## if score is more than 21 count it as 21
      if(temp>21){
        temp<-21
      }
      ## record the final position and score 
      turns2[[1]][temp,score]<-turns2[[1]][temp,score]+1
    }
  }
}

## initialise turns at turn 2
t<-2

## keep taking turns
repeat{
  ## player 1 turn as before
  turns1[[t]]<-matrix(0,10,21)
  for(score in 1:20){
    for(pos in 1:10){
      if(turns1[[t-1]][pos,score]>0){
        for(i in 1:3){
          for(j in 1:3){
            for(k in 1:3){
              temppos<-newpos(pos,i+j+k)
              tempscore<-score+temppos
              if(tempscore>21){
                tempscore<-21
              }
              turns1[[t]][temppos,tempscore]<-turns1[[t]][temppos,tempscore]+turns1[[t-1]][pos,score]
            }
          }
        }
      }
    }
  }
  
  ## player 2 turn as before
  turns2[[t]]<-matrix(0,10,21)
  for(score in 1:20){
    for(pos in 1:10){
      if(turns2[[t-1]][pos,score]>0){
        for(i in 1:3){
          for(j in 1:3){
            for(k in 1:3){
              temppos<-newpos(pos,i+j+k)
              tempscore<-score+temppos
              if(tempscore>21){
                tempscore<-21
              }
              turns2[[t]][temppos,tempscore]<-turns2[[t]][temppos,tempscore]+turns2[[t-1]][pos,score]
            }
          }
        }
      }
    }
  }
  
  ## Count times that player 1 won this turn multiplied by the count of universes
  ## where player 2 hadn't won by the turn before
  p1<-p1+sum(turns1[[t]][,21])*sum(turns2[[t-1]][,1:20])
  ## Count times that player 2 won this turn multiplied by the count of universes
  ## where player 1 hadn't won by this turn
  p2<-p2+sum(turns2[[t]][,21])*sum(turns1[[t]][,1:20])
  
  ## if all possible universes have been counted, stop
  if (sum(turns1[[t]])==sum(turns1[[t]][,21])){
    break
  }
  ## next turn
  t<-t+1
}

## output the number of universes in which the player with the most wins wins
options(scipen=999)
if (p1>p2){
  p1
} else {
  p2
}