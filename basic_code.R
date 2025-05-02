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

#Create random array with for loop
for (i in 1:10) {  
  randomnum <- tibble(value = sample(1:20, 1))
  rand <- randomnum$value
  numbers2 <- c(numbers2, rand)  
}
print(numbers2)
empty_array <- integer(length(numbers2))
numbers3 <- numbers2
for (i in 1:length(numbers2)){
  empty_array[i] <- numbers3[i]
  for (j in 1:length(numbers3)){
    if (empty_array[i] >= numbers3[j]){
      empty_array[i] <- numbers3[j]
      jtemp <- j
    }
  }
  numbers3[jtemp] <- 900
}
print(numbers2)
print(empty_array)
sorted <- sort(numbers2)
print(sorted)


#fill a empty df
cube <- data.frame(matrix(NA, nrow = 20, ncol = 20))

for (n in 1:20){
  for (m in 1:20){
    randomval <- tibble(value = sample(1:20, 1))
    cube[n, m] <- randomval$value
      
  }
}