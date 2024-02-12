# Exercise 4 - Mock WORDLE
library("tidyverse")
## Step 1

### Load data with custom dictionary reader function
scrabble_url <- "https://raw.githubusercontent.com/difiore/ada-2024-datasets/main/collins-scrabble-words-2019.txt"
googleword_url <- "https://raw.githubusercontent.com/difiore/ada-2024-datasets/main/google-10000-english-usa-no-swears.txt"
### Load Dictionary (as a list of string vectors) Function
load_dictionary <- function(filename){
  library(tidyverse)
  df <- read_csv(filename) %>% as.list()
  return(df$words)
}
### Create Lists of Dictionary Words
valid_list <- load_dictionary(scrabble_url) 
solution_list <- load_dictionary(googleword_url)

## Step 2
### Winnow the solution list to only valid values, then count total number
solution_list <- intersect(solution_list, valid_list)
length(solution_list)

## Step 3
### Custom function "pick_solution()"
pick_solution <- function(list=solution_list){
  
}