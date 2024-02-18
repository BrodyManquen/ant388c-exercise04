# Exercise 4 - Mock WORDLE
library("tidyverse")
library("Hmisc")
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
# Pick a test solution
solution <- pick_solution(solution_list)
length(solution[[1]])

## Step 4
### play_wordle() function
evaluate_guess <- function(guess, solution){
  word_length <- length(solution[[1]])
  text_result <- rep("-", word_length)
  # I've integrated your 'ugly hack' 
  ## Calculate each letter counts
  guess_count <- tibble(letter=guess[[1]]) %>%
    group_by(letter) %>%
    summarise(n_in_guess=n())
  solution_count <- tibble(letter=solution[[1]]) %>%
    group_by(letter) %>%
    summarise(n_in_solution=n())
  counts <- inner_join(
    guess_count,
    solution_count, by="letter") %>%
    mutate(to_clear=n_in_guess-n_in_solution) %>%
    filter(to_clear>0) %>%
    select(letter, to_clear) #select the count of each letter in the guess that is "in excess" of the solution's count
  #Create * + - output result w/o account for repeats
  for (i in 1:word_length){
    text_result[i] <-
      case_when(
        guess[[1]][i] %in% solution[[1]] & guess[[1]][i] == solution[[1]][i] ~ "*",
        guess[[1]][i] %in% solution[[1]] & guess[[1]][i] != solution[[1]][i] ~ "+",
        guess[[1]][i] %nin% solution[[1]] ~ "-"
      )
    # loop through each letter with excess counts, check against guess[[1]] input (except for ones correct (*))
    for (j in counts$letter){
      if (guess[[1]][i]==j & text_result[i] != "*" & counts[counts$letter==j,]$to_clear>0){
        text_result[i] <- "-"
        counts[counts$letter==j,]$to_clear <- counts[counts$letter==j,]$to_clear - 1
      }
    }
  }
  return(text_result)
  ## Vestigial code that does not account for excess +
  # result <- str_replace_all(guess[[1]], pattern=solution[[1]], replacement="*") %>%
  #   case_match(
  #     "*" ~ "*",
  #     intersect(guess[[1]], solution[[1]]) ~ "+",   ## finds incorrectly placed letters, sets to "+"
  #     .default = "-"                                ## letters *not* in solution, sets to "-"
  #     )
  # return(result)
}
play_wordle <- function(solution, solution_list, num_guesses=6){
  library("tidyverse")
  library("Hmisc")
  n_guess <- 1 #initialize guess count
  print(paste0("Welcome to my makeshift wordle! You have ", num_guesses, " to guess a ", length(solution[[1]]), " letter word.")) #initial statement
  print("* is correct placement, + is incorrect placement, and - is an incorrect letter.")
  print("Be careful! This game does not track duplicates: a + may show when the correct letter has been placed")
  letters_left <- LETTERS # do not override by putting in while loop
  word_length <- length(solution[[1]])
  while (n_guess <= num_guesses){ #while loop for guess counter
    print(paste0("Unguessed letters: ", letters_left)) #print unguessed letters
    guess <- readline(paste0("Enter guess number ", n_guess, ": ")) %>% toupper() %>% str_split(pattern="") #parse guess
    if (length(guess[[1]])== word_length & paste(guess[[1]], collapse='') %in% solution_list) {
      letters_left <- setdiff(letters_left, guess[[1]]) #remove guessed letters from list
      evaluation <- evaluate_guess(guess, solution)
      print(evaluation)
      if (identical(guess[[1]], solution[[1]])) {
        cat('\n')
        print(paste0("Correct! The solution is: ", paste(solution[[1]], collapse='')))
        break
      } else if (n_guess < num_guesses) {
        cat('\n')
        print("Guess again!") #put some space around it to make sure you see the output!
        cat('\n')
        n_guess <- n_guess + 1
      } else {
        cat('\n')
        print("Incorrect!")
        print(paste0("The correct answer was: ", paste(solution[[1]], collapse='')))
        break
      }

    } else {
      cat('\n')
      print(paste0("Entry invalid! Please enter a ", word_length, " letter word or a word in the game dictionary."))
      cat('\n')
    }
  }
}
full_wordle <- function(solution_list, word_length=5, num_guesses=6){
  solution <- pick_solution(solution_list)
  game <- play_wordle(solution, solution_list, num_guesses=6)
}
full_wordle(solution_list, valid_list)