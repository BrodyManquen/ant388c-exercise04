# Exercise 4 - Mock WORDLE
library("tidyverse")
install.packages("rlist")
library(rlist)
## Step 1

### Load data with custom dictionary reader function
scrabble_url <- "https://raw.githubusercontent.com/difiore/ada-2024-datasets/main/collins-scrabble-words-2019.txt"
googleword_url <- "https://raw.githubusercontent.com/difiore/ada-2024-datasets/main/google-10000-english-usa-no-swears.txt"
### Load Dictionary (as a list of string vectors) Function
load_dictionary <- function(filename){
  library(tidyverse)
  df <- read_csv(filename) %>% as.list()
  return(df)
}
### Create Lists of Dictionary Words
valid_list <- load_dictionary(scrabble_url) 
solution_list <- load_dictionary(googleword_url)

## Step 2
### Winnow the solution list to only valid values, then count total number
solution_list <- intersect(solution_list$words, valid_list$words)
nchar(solution_list[2])


## Step 3
### Custom function "pick_solution()"
pick_solution <- function(solution_list, word_length=5){
  solution <- solution_list[nchar(solution_list) == word_length] %>% #only returns 5 letter words from solution list
    sample(size=1) %>%
    str_split(pattern="")
  return(solution)
}
solution <- pick_solution(solution_list)
length(solution[[1]])

## Step 4
### play_wordle() function
play_wordle <- function(solution, valid_list, num_guesses=6){
  n_guess <- 1 #initialize guess count
  print(paste0("You have ", num_guesses, " to guess a ", length(solution[[1]]), " letter word."))
  letters_left <- LETTERS
  print(paste0("Unguessed letters: ", letters_left))
  guess <- str_split(toupper(readline(paste0("Enter guess number ", n_guess, ": "))), pattern="")
  return(guess)
  #evaluate_guess <- function(guess, solution){
    
  #}
  #while n_guess < num_guesses
}
test <- play_wordle(solution, valid_list)
charmatch(solution[[1]], test[[1]])
solution[[1]] %in% test[[1]]
