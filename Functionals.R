##########################   SOLUTION 1   #########################################################
set.seed(10)
N=10
cs1 <- rnorm(N,72,10)
cs2 <- rnorm(N,65,7)
cs3 <- rnorm(N,80,9)
cs4 <- rnorm(N,55,7)
cs5 <- rnorm(N,61,5)
m <- matrix(c(cs1,cs2,cs3,cs4,cs5),ncol = 5)

#Creating and Printing the matrix
m
#Function which sorts the column of matrix in descending order. Next I am assigning ranks (1-10) to the index
#that contains the highest element and continuing it in decreasing order.
ordering <- function(x){
  sorted_data<- sort.int(x,decreasing = T,index.return = T)
  print(sorted_data)
  #print(sorted_data$ix)
  x[sorted_data$ix] <- c(1:10)
  x
}

#Using apply to traverse the matrix
new_M <- apply(m,2,ordering)

#Assigning rownames and colnames
row_name <- paste("Student#",1:10,sep = "")
col_name <- paste("cs",1:5,sep = "")
rownames(new_M) <- row_name
colnames(new_M) <- col_name
new_M

#############################################
#Computation of Median Rank              
med <- function(r){
   median(r)
}
               
new_M <- cbind(new_M,apply(new_M,1,med))
colnames(new_M)[6] <- c("median")     
new_M


################################SOLUTION 2#####################################
#####################################################################################

set.seed(10)
N=10
cs1 <- rnorm(N,72,10)
cs2 <- rnorm(N,65,7)
cs3 <- rnorm(N,80,9)
cs4 <- rnorm(N,55,7)
cs5 <- rnorm(N,61,5)
m <- matrix(c(cs1,cs2,cs3,cs4,cs5),ncol = 5)
colnames(m) <- c('CS1', 'CS2' , 'CS3' , 'CS4' , 'CS5')
rownames(m) <- c('Student#1', 'Student#2' , 'Student#3' , 'Student#4' ,'Student#5','Student#6','Student#7',
                 'Student#8','Student#9','Student#10')

ranklen <- c(1:length(rownames(m)))
#Creating and printing the matrix
m
i <- vector(mode = "integer",length = 10)
full <- function(col){
  # Sort the column in decreasing order
  col_sort <- sort(col,decreasing = TRUE) 
  # The highest element will have Rank 1 and so on. So replace the sorted column with RankLen Vector from 1 -10
  col_arrange <- replace(col_sort,c(1:length(ranklen)),ranklen)
  #Now I need to sort the column based on the names assigned to the matrix, so storing the names in num to sort.
  num <- names(col_arrange)
  #Ordering the ranked column based on names assigned
  col_arrange[order(nchar(num),num)]
}

apply(m,2,full)
###########################################################################
