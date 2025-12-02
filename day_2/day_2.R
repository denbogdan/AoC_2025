library(dplyr)
setwd("~/portfolio/AoC_2025/")

#read in data
input <- readLines("day_2/input.txt")
#split direction from number
input <- strsplit(input , ',') |> unlist()


##Question 1 -----------------------------------------------------------------------
#for each range generate the numbers
sum <- 0

for(range in input) {
  print(range)
  #split the range
  start <- strsplit(range, '-')[[1]][1] |> as.numeric()
  end <- strsplit(range, '-')[[1]][2] |> as.numeric()
  
  #generate the numbers
  numb <- seq(start, end)
  
  #for each number, split in half and compare left with right
  for(n in numb) {
    #split in half
    n_len <- nchar(n)
    
    if(n_len %% 2 == 0) {
      #compare halves
      h1 <- substr(n, 1, n_len/2) |> as.numeric()
      h2 <- substr(n, n_len/2 + 1, n_len) |> as.numeric()
      
      if(h1 == h2) {
        sum <- sum + as.numeric(n)
        print(n)
      }
    }
    
    
  }
}

##Answer 1 -------------------------------------------------------------------------
sum



##Question 2 -----------------------------------------------------------------------

#write function that checks for repetitions
check_rep <- function(n) {
  #initilaise return as 0
  to_return <- 0
  
  #compute length
  n_len <- nchar(n)
  
  #break down into divisors
  div <- divisors(n_len)
  #the number is always divisible by itself, not useful here
  div <- div[1:(length(div)-1)]
  
  #for each divisor, break the number into bits and check if they are the same
  for(d in div) {
    #break down
    components <- sapply(seq(from=1, to=nchar(n), by=d), function(i) substr(n, i, i+(d-1)))
    
    #check identity and update return
    if(length(unique(components)) == 1) to_return <- as.numeric(n)
  }
  
  return(to_return)
}

#run program similar to above
sum <- 0
collect_values <- c()

for(range in input) {
  print(range)
  #split the range
  start <- strsplit(range, '-')[[1]][1] |> as.numeric()
  end <- strsplit(range, '-')[[1]][2] |> as.numeric()
  
  #generate the numbers
  numb <- seq(start, end)
  
  #for each number, check if it has repetitions
  for(n in numb) {
    if(check_rep(n) > 0) {
      collect_values <- c(collect_values, n)
      sum <- sum + n
    }
  }
    
}

##Answer 2 -------------------------------------------------------------------------
#numbers 1:9 are divisible my themselves and end up wrongly counted
sum - sum(1:9)

#or
sum(collect_values) - sum(1:9)



