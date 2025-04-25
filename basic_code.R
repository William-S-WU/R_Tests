library(tidyverse)

H = 1
J = 2 
hj <- list(1,2)
for (x in 1:10){
  hj <- append(hj, x)
  cat(paste0("this is the ", x, "time this loop has been run\n"))  
  cat(paste0("Hj is ", hj, "\n")) 
}

print(hj)

# Initialize an empty vector
numbers <- c()

# Use a for loop to add iteration numbers to the vector
for (i in 1:10) {  # Adjust range for desired iterations
  numbers <- c(numbers, i)  # Append i to the vector
}
wnumbers <- c()
i <- 1
while (i <= 10){
  wnumbers <- c(wnumbers, i)
  i <- i+1
}
# Print the result

print(wnumbers)
print(numbers)


randomnum <- tibble(value = sample(1:20, 1))
print(randomnum)

numbers2 <- c()

# Use a for loop to add iteration numbers to the vector
for (i in 1:10) {  # Adjust range for desired iterations
  randomnum <- tibble(value = sample(1:20, 1))
  rand <- randomnum$value
  numbers2 <- c(numbers2, rand)  # Append random number
}
print(numbers2)
