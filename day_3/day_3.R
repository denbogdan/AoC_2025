#read in data
input <- readLines("day_3/dummy.txt")


##Question 1 -----------------------------------------------------------------------
#initiate sum
sum <- 0

for(bank in input) {
  
}

bank = input[1]

#split into letters
bank_split <- strsplit(bank, "") |> unlist() |> as.numeric()

search_max <- function(bank_split) {
  #find the largest digit
  max_loc <- which(bank_split == max(bank_split))
  
  #check all locations of the largest digit - then truncate vector and repeat
  for(i in max_loc) {
    
  }
}

