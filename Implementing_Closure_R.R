library(lubridate)

timer <- function() {
  #Creating a vector start and initializing it with logical NA.
  Start <- as.logical(NA)
  #Creating a vector Fiish and initializing it with logical NA.
  Finish <- as.logical(NA)
  #Shows the current time
  Init <- now()
  #List which contains the attributes of the timer
  cal <- list(Init = Init, Start = Start, Finish = Finish)
  #Returns the closure's state information.
  get_state <- function(){
    cal
  } 
  #To start the timer
  start <- function() {
    # Using <<- to update the Start attribute of the timer 
    cal$Start <<- now()
  }
  #To stop the timer
  stop = function() {
    #exception check that if timer is not started then it cant be stopped
    if(is.na(cal$Start)){
      base::stop("Error, Cannot stop as timer was not started...")
    }
    #If timer is started then you can stop the timer
    cal$Finish <<- now()
  }
  #Calculate the time difference using difftime
  get_time <- function(){
    # Throw error if timer is not stopped
    if(is.na(cal$Finish)){
      base::stop("Error, Cannot get time as stop was not called...")
    }
    difftime(cal$Finish, cal$Start, units = "secs")
  } 
  #Creating a list of all the functions inside timer
  list(get_state = get_state, start = start, stop = stop, get_time = get_time)
}

#Create the timer closure
t <- timer()
str(t$get_state())
#Check for error 1
str(t$stop)
#Start the timer
t$start()
str(t$get_state())
#Check for error 2
t$get_time()
#Stop the timer and calculate the time difference
t$stop()
t$get_time()

