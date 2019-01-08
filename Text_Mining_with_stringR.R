library(stringr)
library(readr)
library(ggplot2)
library(dplyr)
library(stringr)

################### Start ########################################

stopwords <- c("a", "about", "above", "above", "across", "after", "afterwards", "again", 
               "against", "all", "almost", "alone", "along", "already", "also","although",
               "always","am","among", "amongst", "amoungst", "amount",  "an", "and", "another", 
               "any","anyhow","anyone","anything","anyway", "anywhere", "are", "around", "as",  
               "at", "back","be","became", "because","become","becomes", "becoming", "been", 
               "before", "beforehand", "behind", "being", "below", "beside", "besides", 
               "between", "beyond", "bill", "both", "bottom","but", "by", "call", "can", 
               "cannot", "cant", "co", "con", "could", "couldnt", "cry", "de", "describe", 
               "detail", "do", "done", "down", "due", "during", "each", "eg", "eight", "either", 
               "eleven","else", "elsewhere", "empty", "enough", "etc", "even", "ever", "every", 
               "everyone", "everything", "everywhere", "except", "few", "fifteen", "fify", 
               "fill", "find", "fire", "first", "five", "for", "former", "formerly", "forty", 
               "found", "four", "from", "front", "full", "further", "get", "give", "go", "had", 
               "has", "hasnt", "have", "he", "hence", "her", "here", "hereafter", "hereby", 
               "herein", "hereupon", "hers", "herself", "him", "himself", "his", "how", "however", 
               "hundred", "ie", "if", "in", "inc", "indeed", "interest", "into", "is", "it", "its", 
               "itself", "keep", "last", "latter", "latterly", "least", "less", "ltd", "made", 
               "many", "may", "me", "meanwhile", "might", "mill", "mine", "more", "moreover", 
               "most", "mostly", "move", "much", "must", "my", "myself", "name", "namely", 
               "neither", "never", "nevertheless", "next", "nine", "no", "nobody", "none", "noone", 
               "nor", "not", "nothing", "now", "nowhere", "of", "off", "often", "on", "once", "one", 
               "only", "onto", "or", "other", "others", "otherwise", "our", "ours", "ourselves", 
               "out", "over", "own","part", "per", "perhaps", "please", "put", "rather", "re", 
               "same", "see", "seem", "seemed", "seeming", "seems", "serious", "several", "she", 
               "should", "show", "side", "since", "sincere", "six", "sixty", "so", "some", "somehow", 
               "someone", "something", "sometime", "sometimes", "somewhere", "still", "such", "system",
               "take", "ten", "than", "that", "the", "their", "them", "themselves", "then", 
               "thence", "there", "thereafter", "thereby", "therefore", "therein", "thereupon", 
               "these", "they", "thickv", "thin", "third", "this", "those", "though", "three", 
               "through", "throughout", "thru", "thus", "to", "together", "too", "top", "toward",
               "towards", "twelve", "twenty", "two", "un", "under", "until", "up", "upon", "us", 
               "very", "via", "was", "we", "well", "were", "what", "whatever", "when", "whence", 
               "whenever", "where", "whereafter", "whereas", "whereby", "wherein", "whereupon", 
               "wherever", "whether", "which", "while", "whither", "who", "whoever", "whole", 
               "whom", "whose", "why", "will", "with", "within", "without", "would", "yet", "you", 
               "your", "yours", "yourself", "yourselves", "the")  
invalid_characters <- c("--","\\?","\\!","\\.",",","\\.","'",":")

f_pre <- readLines("C:/Users/prodi/Desktop/Books/Programming for Data Analytics in R/Chapter 02.txt")
# A	vector	of	429 elements,	each	for	a	line	of	text
str(f_pre)
f_pre[1:10]

# Function to convert Vector of lines to vector of Words
convert_to_words_vector <- function(corp){
  # Splitting the string on space which will return a list
  splitx <- str_split(corp," ")
  # Using the unlist function to get the string back from list
  splitx <- unlist(splitx)
  # subsetting the string to fetch the string without the null strings
  return (str_subset(splitx , ""))
}

# Calling the function
f_pre_vec <- convert_to_words_vector(f_pre)
# To display the variable f_pre_vec
str(f_pre_vec)

# Collapsing the vector 'invalid_characters' into a regex so that it can be passed to stringr functions
invalid_char_mod <- str_c(invalid_characters,collapse = "|")
# Adding anchors to every word in vector stopwords
stop_word_mod <- str_c("^",stopwords,"$",collapse = "|")

#Function to perform all the preprocessing
words_preprocess <- function(corp){
  #remove all the invalid characters
  str_rem_chr <- str_remove_all(corp,invalid_char_mod)
  # Subsetting the string vector without the null strings
  str_rem_chr <- str_subset(str_rem_chr , "")
  # Converting all words to lowercase
  str_to_lower <- str_to_lower(str_rem_chr)
  #remove stopwords
  stoprem <- str_remove_all(str_to_lower,stop_word_mod)
  stoprem <- str_subset(stoprem , "")
  return (stoprem)
}

f_post <- words_preprocess(f_pre_vec)
str(f_post)

# Fetching only the unique words and removing the duplicates
uniq_word <- unique(f_post)
str(uniq_word)

# Creating a tibble where first word is processed word.
ans <- tibble(Words = uniq_word)

#Adding columns to tibble for the given pattern and also the length of the words
ans <- ans %>% mutate(Pattern = str_c("^",Words,"$"),
               Wlength = str_length(Words))

print(ans)
################ Grouping the tibble with Wlength to pass into summarise() function
by_len <- group_by(ans,Wlength)

freq <- summarise(by_len,WFrequency = n())
print(freq)

#### Plotting the given plot which shows that 5 letter words has the maximum occurence ###
ggplot(freq, mapping = aes (x = Wlength , y = WFrequency)) +
xlab("Word Length") +
ylab("Word Frequency") +
geom_point(color = "blue") + geom_path()
