
##############################################################FINAL SOLUTION#########################################################################

library(readxl)
library(ggplot2)

orig_list <- data.frame(readxl::read_excel("C:/Users/prodi/Desktop/Books/Programming for Data Analytics in R/titanic3_assignment.xls"))

plist <- orig_list  

head(orig_list)

######## Reading and Preparing the Data #######################################
dim(plist)

summary(plist)

# Converting Survived to Logical
plist$survived <- as.logical(plist$survived)
summary(plist)

#Changing class to String
plist$pclass <- ifelse(plist$pclass == 1,"First", ifelse( plist$pclass == 2,"Second", "Third"))
unique(plist$pclass)
summary(plist)

#Imputation of Age
plist$age[is.na(plist$age)] <- mean(plist$age, na.rm = T)
summary(plist)

#Imputation of Fare
plist$fare[is.na(plist$fare)] <- mean(plist$fare, na.rm = T)
summary(plist)

#Imputation of Embarking from randomnly choosing from 'S' , 'C' , 'Q'
set.seed(99)
plist$embarked[is.na(plist$embarked)] <- sample(c('S','C','Q'),size = 3,replace=TRUE)
summary(plist)

#New column Age_Cohort
plist$age_cohort <- ifelse(plist$age < 16,"Child", ifelse( plist$age >= 60,"Elderly", "Adult"))
unique(plist$age_cohort)
head(plist,10)
summary(plist)

#Replacing with full town origin
plist$embarked <- ifelse(plist$embarked == 'S',"Southampton", ifelse( plist$embarked == 'C',"Cherbourg", "Cobh"))
unique(plist$embarked)

### Double check#############################3
head(plist)
dim(plist)
table(plist$survived)
table(plist$survived,plist$age_cohort)
table(plist$survived,plist$sex)
table(plist$survived,plist$pclass)
table(plist$survived,plist$embarked)


######### Plot 1########################
ggplot(data = plist) + 
  ggtitle("Survival Numbers by Travel Class") +
  theme(legend.position="top" , legend.title = element_blank()) +
  geom_bar(mapping = aes(x = survived,fill=pclass)) + ylab("Number")

######### Plot 2 #######################
ggplot(data = plist) + 
  ggtitle("Survival Numbers by Gender") +
  theme(legend.position="top" , legend.title = element_blank()) +
  geom_bar(mapping = aes(x = survived,fill=sex)) + ylab("Number")

######### Plot 3 #####################
ggplot(data = plist) + 
  ggtitle("Survival Numbers by Age Cohort") +
  theme(legend.position="top" , legend.title = element_blank()) +
  geom_bar(mapping = aes(x = survived,fill=age_cohort)) + ylab("Number")

######## Plot 4 ######################
ggplot(data = plist) + 
  ggtitle("Survival Numbers by Embarked Location") +
  theme(legend.position="top" , legend.title = element_blank()) +
geom_bar(mapping = aes(x = survived,fill=embarked)) + ylab("Number")

######### Plot 5 ####################

ggplot(data=plist, aes(survived))+
  ggtitle("Survival Proportions by Embarked Location")+
  theme(legend.position="top" , legend.title = element_blank())+
  geom_bar(aes(fill=pclass), position="fill") +
  ylab("Proportion")

######### Plot 6 ####################
ggplot(data=plist, aes(survived))+
  ggtitle("Survival Proportions by Gender")+
  theme(legend.position="top" , legend.title = element_blank())+
  geom_bar(aes(fill=sex), position="fill") +
  ylab("Proportion")

######### Plot 7 ####################
ggplot(data=plist, aes(survived))+
  ggtitle("Survival Proportions by Age Cohort")+
  theme(legend.position="top" , legend.title = element_blank())+
  geom_bar(aes(fill=age_cohort), position="fill") +
  ylab("Proportion")

######### Plot 8 ###################
ggplot(data=plist, aes(survived))+
  ggtitle("Survival Proportions by place of Embarkation")+
  theme(legend.position="top" , legend.title = element_blank())+
  geom_bar(aes(fill=embarked), position="fill") +
  ylab("Proportion")

########## Plot 9 ##################
ggplot(data = plist) + 
  ggtitle("Survival Number by Cohort and Travel Class") +
  theme(legend.position="top" , legend.title = element_blank()) +
  geom_bar(mapping = aes(x = survived,fill=age_cohort)) + ylab("Number") +
  facet_wrap(~pclass)

########## Plot 10 ################
ggplot(data = plist) + 
  ggtitle("Survival Number by Gender and Travel Class") +
  theme(legend.position="top" , legend.title = element_blank()) +
  geom_bar(mapping = aes(x = survived,fill=sex)) + ylab("Number") +
  facet_grid(~pclass)

########## Plot 11 ################
ggplot(data = plist) + 
  ggtitle("Age v Fare by Place of Embarkation") +
  theme(legend.position="top" , legend.title = element_blank()) +
  geom_point(mapping = aes(x = age,y = fare, color = embarked)) + 
  ylab("Fare") +
  xlab("Age")

########## Plot 12 ################
ggplot(data = plist,aes(x = age,y = fare)) + 
  ggtitle("Age v Fare with Linear Model") +
  theme(legend.position="top" , legend.title = element_blank()) +
  geom_point() + 
  geom_smooth(method=lm , se = TRUE) +
  ylab("Fare") +
  xlab("Age")

######### Plot 13 ################
ggplot(data = plist) + 
  ggtitle("Age v Fare with Survival Info") +
  theme(legend.position="top" , legend.title = element_blank()) +
  geom_point(mapping = aes(x = age,y = fare, color = survived)) + 
  ylab("Fare") +
  xlab("Age")

######### Plot 14 ################
ggplot(data = plist) + 
  ggtitle("Age v Fare By Travel Class and Point of Departure") +
  theme(legend.position="top" , legend.title = element_blank()) +
  geom_point(mapping = aes(x = age,y = fare, color = embarked)) + 
  facet_grid(~pclass) +
  ylab("Fare") +
  xlab("Age")





