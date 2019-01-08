my_table <- function(v, na.rm=F){
  # First check to make sure no NA values
  if(na.rm == F && any(is.na(v)))
    stop("Error, the input vector has NA element(s)")
  else
    v <- v[!is.na(v)]

  # Second check to make sure its a character vector
  if(is.character(v) == F)
    stop("Error, input must be a character vector")
  
  factors <- unique(v)

  output <- vector(mode="integer",length=length(factors))
  names(output)<- sort(factors)
  
  for(i in seq_along(v)){
    loc <- which(v[i] == names(output))
    output[loc] <- output[loc]+1
  }
  
  output
}

set.seed(111)
(v <- 1:10)
(my_table(v))

set.seed(111)
(v <- sample(c('M','F'),10,prob = c(.3,.7),replace = T))
(v <- c(v,NA))
(my_table(v))

set.seed(111)
(v <- sample(c('M','F'),10,prob = c(.3,.7),replace = T))
(v <- c(v,NA))
(my_table(v,na.rm=T))

set.seed(111)
(v <- sample(c('M','F'),10,prob = c(.3,.7),replace = T))
(my_table(v))





