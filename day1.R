library(tidyverse)

# read in the data
day1 <- read_lines('day1Data.txt') %>%
  as.numeric()

# part 1
for(i in 1:length(day1)) {
  
  needed <- (2020 - day1[[i]])

  for(j in i:length(day1)) {
    if(needed == day1[[j]]) {
      print(day1[[i]])
      print(day1[[j]])
      print(day1[[i]]*day1[[j]])
      break()
    }
  }
}

# part 2
for(i in 1:length(day1)) {
  
  needed1 <- (2020 - day1[[i]])
  
  for(j in 1:length(day1)) {
    needed2 <- needed1 - day1[[j]]
    for(k in j:length(day1)) {
      if(needed2 == day1[[k]]){
        print(day1[[i]])
        print(day1[[j]])
        print(day1[[k]])
        print(day1[[i]]*day1[[j]]*day1[[k]])
      }
    }
  }
}