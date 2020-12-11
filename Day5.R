# Day 5
library(tidyverse)

# read in data
day5 <- tibble(x = read_lines('data/day5Data.txt'))

figureOutHalf <- function(numVec, position){
  
  temp <- length(numVec)
  
  if (temp == 1){
    
    return(numVec)
    
  }
  
  split <- temp/2
  
  if (position == "F" || position == "L"){
    
    return(numVec[1:split])
    
  } else if (position == "B" || position == "R"){
    
    return(numVec[(split+1):temp])
    
  } else {
    
    stop("No correct position")
    
  }
}

figureOutLocation <- function(rowString, numPos){

	pos <- 0:numPos

	for(i in 1:nchar(rowString)){
	  
		pos <- figureOutHalf(pos, str_sub(rowString, i, i))
		
	}
	
	return(pos)

}

# part 1
day5Part1 <- day5 %>%
	rowwise() %>%
	mutate(row = figureOutLocation(str_sub(x, start = 1, end = 7), 127),
		seat = figureOutLocation(str_sub(x, start= 8, end =10), 7),
		seatID = (row*8) + seat)

max(day5Part1$seatID)

# part 2
day5Part2 <- day5Part1 %>%
	arrange(seatID) %>%
	ungroup() %>%
	mutate(different = seatID - lag(seatID)) %>%
	filter(different > 1)

mySeat <- (day5Part2$seatID - 1) 

print(mySeat)
