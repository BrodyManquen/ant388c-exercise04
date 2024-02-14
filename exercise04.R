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
evaluate_guess <- function(guess, solution, letters_left){
  letters_left <- setdiff(letters_left, guess[[1]]) #remove guessed letters from list
  result <- case_match(test,
                       str_extract(test, solution[[1]]) ~ "+", ## finds correctly placed letters, sets to "+"
                       intersect(test, solution[[1]]) ~ "~",   ## finds incorrectly placed letters, sets to "~"
                       .default = "-"                          ## finds letters *not* in solution, sets to "-"
                      )
  return(result)
}
play_wordle <- function(solution, valid_list, num_guesses=6){
  n_guess <- 1 #initialize guess count
  print(paste0("You have ", num_guesses, " to guess a ", length(solution[[1]]), " letter word.")) #initial statement
  letters_left <- LETTERS # do not override by putting in while loop
  while (n_guess <= num_guesses){ #while loop for guess counter
    print(paste0("Unguessed letters: ", letters_left)) #print unguessed letters
    guess <- readline(paste0("Enter guess number ", n_guess, ": ")) %>% toupper() %>% str_split(pattern="") #parse guess
    evaluation <- evaluate_guess(guess, solution, letters_left)
    n_guess <- n_guess + 1 #ticker
  }

  return(guess)

}

### Testing Grounds
test_run <- play_wordle(solution, valid_list)
test <- c("H", "O", "N", "E", "C")
solution[[1]]
###

