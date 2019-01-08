# Class constructor for class 'myvec'
myvec <- function(v){
  if(!is.numeric(v))
    stop("Error, data type must be numeric.")
  structure(list(data=v),class="myvec")
}

# Creating object x of class 'myvec'
x<-myvec(1:10)

# Since print is a generic function we can override it so print can perform the operation on class 'myvec'
print.myvec <- function(vec){
  cat("s3 class = ",class(vec),"\n","Number of elements = ", length(vec$data),"\n",vec$data)
}

x

# Overriding the subsetting operator to subset on the object of class 'myvec'
`[.myvec` <- function(vec,y){
  vec$data[y]
}

x[1:2]

# Overriding the assignment operator.
`[<-.myvec` <- function(vec,i, value){
  vec$data[i] <- value
  # Since the above is mutation operation we need to return the whole object to preserve its class 'myvec'
  vec
}

x[1:2] <- 0

# Overriding < operator for class 'myvec' to display values less than only 'Y'
`<.myvec` <- function(vec,y){
  vec$data < y
}

x[x<1]

# Overriding > operator for class 'myvec' to display values greater than only 'Y'
`>.myvec` <- function(vec,y){
  vec$data > y
}

x[x>1]

# Overriding ! operator for class 'myvec' to display values not equal to 'Y'
`!=.myvec` <- function(vec,y){
  vec$data != y
}

x[x!=0]

x[1]<-NA

#Since sum is a generic function we can override it to perform sum operation on class 'myvec'.
# ... is a placeholder for na.rm argument 
`sum.myvec` <- function(vec, ...){
  sum(vec$data,...)
}

sum(x)
sum(x,na.rm = T)  

#Since mean is a generic function we can override it to perform mean operation on class 'myvec'.
# ... is a placeholder for na.rm argument
`mean.myvec` <- function(vec, ...){
  mean(vec$data,...)
}

mean(x)
mean(x,na.rm=T)
