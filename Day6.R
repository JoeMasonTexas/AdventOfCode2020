# day 6
library(tidyverse)

# read in given data
day6 <- read_lines('data/day6Data.txt')

groupCount <- 1
groups <- c()

for(i in 1:length(day6)) {

	if(str_detect(day6[[i]], "")) {
	  
		groups <- c(groups, groupCount)
		
	} else {
	  
		groups <- c(groups, NA)
		groupCount <- groupCount + 1
		
	}

}

# part 1
day6Part1 <- tibble(x = day6, groups = groups) %>%
	filter(!is.na(groups)) %>%
	group_by(groups) %>%
	summarize(text = paste(x, collapse = ""),
		answers = length(unique(str_split(text[[1]],"")[[1]])))

sum(day6Part1$answers)

# part 2
day6Part2 <- day6Part1 <- tibble(x = day6, groups = groups) %>%
  filter(!is.na(groups)) %>%
  group_by(groups) %>%
  pivot_wider(names_from = groups, values_from = x)

countOfLetters <- c()

for(i in 1:NCOL(day6Part2)){
  
  letterCount <- 0
  for(j in 1:length(letters)){
    
    searchLetter <- letters[[j]]
    temp <- c()
    for(k in 1:length(day6Part2[,i][[1]][[1]])) {
      
      there <- str_detect(day6Part2[,i][[1]][[1]][[k]], searchLetter)
      temp <- c(temp, there)
      
    }
    if(all(temp)){
      
      letterCount <- letterCount +1
      
    }
    
  }
  countOfLetters <- c(countOfLetters, letterCount)
  
}

sum(countOfLetters)
