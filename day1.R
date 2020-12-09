library(tidyverse)

day1 <- read_lines('day1Data.txt') %>%
  as.numeric()

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
