library(dplyr)
setwd("~/portfolio/AoC_2025/")

#read in data
input <- read.delim("day_1/input.txt", header=FALSE)
#split direction from number
input <- input %>% dplyr::mutate(sign = substr(input$V1,1,1), n = as.numeric(substring(input$V1, 2, nchar(input$V1))))


##Question 1 -----------------------------------------------------------------------
#use a vector to keep track of the # of times the dial rests at each number
counts_template <- rep(0, 100)

current_pos <- 51
for(i in 1:nrow(input)) {
  print(paste("The current position is", current_pos))
  #add current position to tally
  counts_template[current_pos] <- counts_template[current_pos] + 1
  
  print(paste("We are moving", input[i,"sign"], "for n numbers", input[i,"n"]))
  #find new position
  if(input[i,"sign"] == "R") {
    new_pos <- current_pos + input[i,"n"]
    if(new_pos > 100) new_pos <- new_pos %% 100
  } else {
    new_pos <- current_pos - input[i,"n"]
    if(new_pos < 1) new_pos <- 100 - (abs(new_pos) %% 100)
  }
  
  #update current position
  current_pos <- new_pos
}

##Answer 1 -------------------------------------------------------------------------
counts_template[1]



##Question 2 -----------------------------------------------------------------------
#use a vector to keep track of the # of times the dial rests at each number
counts_template <- rep(0, 100)
current_pos <- 51

add_to_tally <- 0 
for(i in 1:nrow(input)) {

  if(new_pos == 1) add_to_tally <- add_to_tally-1
  
  #find new position
  if(input[i,"sign"] == "R") {
    new_pos <- current_pos + input[i,"n"]
    if(new_pos > 100) {
      new_pos <- new_pos %% 100
      add_to_tally <- add_to_tally + floor(input[i,"n"] / 100) + 1
    }
    print(new_pos)
    print(paste("Tally:", add_to_tally))
  } else { 
    
    new_pos <- current_pos - input[i,"n"]
    if(new_pos < 1) {
      new_pos <- 100 - (abs(new_pos) %% 100)
      add_to_tally <- add_to_tally + floor(input[i,"n"] / 100) + 1
    }
    print(new_pos)
    print(paste("Tally:", add_to_tally))
  }
  
  #update current position
  current_pos <- new_pos
  
  #add to count
  counts_template[current_pos] <- counts_template[current_pos] + 1
}


 ##Answer 2 -------------------------------------------------------------------------
add_to_tally + counts_template[1]



