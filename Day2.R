# Day 2 
library(tidyverse)

# read in data
day2 <- tibble(x = read_lines('data/day2Data.txt'))

# clean the data up
day2Clean <- day2 %>%
	separate(col = x, into = c('num', 'key', 'password'), sep = " ") %>%
	separate(col = num, into = c('min', 'max')) %>%
	mutate(min = as.numeric(min),
		max = as.numeric(max),
		key = str_remove(key, ":"))

# part 1
day2Part1 <- day2Clean %>%
	mutate(keyInPass = str_count(password, patter = key),
		valid = if_else(min <= keyInPass & keyInPass <= max, TRUE, FALSE))

sum(day2Part1$valid)

# part 2

day2Part2 <- day2Clean %>%
	mutate(pos1 = str_sub(password, start = min, end = min),
		pos2 = str_sub(password, star = max, end = max),
		contains1 = if_else(pos1 == key | pos2 == key, TRUE, FALSE),
		contains2 = if_else(pos1 == key & pos2 == key, TRUE, FALSE),
		valid = if_else(contains1 == TRUE & contains2 == FALSE, TRUE, FALSE))

# answer
sum(day2Part2$valid)
