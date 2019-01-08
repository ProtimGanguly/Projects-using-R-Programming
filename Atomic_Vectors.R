set.seed(99)

# Simulate the dice throws
total <- sample(1:6,1000,replace = T) + sample(1:6,1000, replace = T)

# Create the output vector
ans <- vector(mode = "numeric", length = 2)
names(ans) <- c("Number Odd", "Number Even")

# Store the results
ans[1] <- sum(total %% 2 != 0)
ans[2] <- sum(total %% 2 == 0)

# Generate Frequencies
freq <- vector(mode = "numeric", length = 11)
names(freq) <- 2:12

# Need a loop to populate the values
for(i in 1:length(total)){
  index <- which(names(freq)==total[i])
  freq[index] <- freq[index] + 1
}


# Print every 100th item.
lv <- c(T,rep(F,99))

total[lv]
