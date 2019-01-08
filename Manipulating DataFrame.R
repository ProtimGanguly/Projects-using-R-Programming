set.seed(1000)
ids <- rep(as.character(1001:1005,2))
module <- c(rep("CT101",5),rep("CT102",5))
result <- c(rnorm(n = 5,mean = 70,sd = 5),
            rnorm(n = 5,mean = 50,sd = 8))

result[1] <- NA 
result
dataf <- data.frame(ids=ids,module=module,result= result, stringsAsFactors = F)

####Printing the Data Frame after creating it##########
dataf

####Creating the function###########
my_aggregate <- function(df, group_id, data_id, f, ...){
  #######Exceptions check#############
  if(!class(df) == "data.frame")
    stop("First parameter is not a data frame object")
  if( !group_id %in% colnames(df))
    stop("Error modul is not a valid column")
  if(!class(df[,data_id]) %in% "numeric")
    stop("Error ids is not a numeric column")
  if(!is.function(f))
    stop("Error ",f, " is not a function")
  
  ###########The function should only if the Result column is passed as data id###########3
  if(class(df[,data_id]) %in% "numeric"){
    
    ######## When grouping is done over Module Column#######
    if(group_id == "module")
    {
      
      ######### Loop over the unique elements of that column and creating a subset of the main
      ######### DataFrame (s1) containing the grouped values of Module.
      sapply(unique(module), function(x){
        s1 <- df[df$module == x,]
        print(s1)
        mean(s1[,"result"], ...)
      })
    }
    
    ######### Loop over the unique elements of that column and creating a subset of the main
    ######### DataFrame (s1) containing the grouped values of Module.
    else if (group_id == "ids")
    {
      sapply(unique(ids), function(x){
        s1 <- df[df$ids == x,]
        mean(s1[,"result"], ...)
      })
    }
  }
}

############ Function Calls provided in the assignment#####################
my_aggregate(dataf, "module", "result", mean)
my_aggregate(dataf, "module", "result", mean, na.rm=T)
my_aggregate(dataf, "ids", "result", mean)
my_aggregate(dataf, "ids", "result", mean,na.rm=T)

############ Validation checks for the exceptions thrown by the function########
my_aggregate(1:10, "module", "result", mean, na.rm=T)
my_aggregate(dataf, "modul", "result", mean)
my_aggregate(dataf, "module", "ids", mean)
my_aggregate(dataf, "module", "result", 13)

