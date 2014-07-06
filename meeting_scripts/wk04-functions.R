##############  functions ####################


# lets start with something very simple
# if we want to square a number we just write
my_favorite_number <- 13
my_favorite_number^2

# with less conceit
2^2

13^2

# say for no good reason that we want to write a function that does what "^" does.
# we need input
# we need output
squared <- function(input){
    return(input*input) # here, "input*input" is our output
}
# our input is clear, and our output is whatever is inside of the return() function

squared(2)

squared(13)

# great, can easily give us values that we dont want
# say we have a vector of values and we want to multiply them together (we want the dot product)
toy_vector <- 1:9
toy_vector
squared(toy_vector)

# this doesn't give us what we want
# likewise consider a matrix of values that we want to square (we want the matrix product)
symmetric_toy_matrix <- matrix(1:9, nrow = 3, ncol = 3)
symmetric_toy_matrix
squared(symmetric_toy_matrix)

# again, not what we want
# we can modify our function, using flow control, and allow it to handle these special cases.
# in other words, we are making our function more robust (at least in terms of what we want to do)
squared <- function(input){
  if (length(input) > 1){ 
    return(input%*%input)
  }
  else if (length(input) == 1){
    return(input*input)
  }
  else {
    print("Something is wrong - check your input")
  }
}

squared(toy_vector)
squared(symmetric_toy_matrix)

# notice that this follows the form:
#
# name_of_the_function <- function(what_ever_the_input_is){
#   if (some_condition){
#     do_some_stuff 
#     return(whatever_the_output_is)
#   }
#   else if (some_other_condition){
#     do_some_stuff
#     return(another_output_according_the_condition)
#   }
#   else {
#     do_some_stuff (like print error messages)
#   }
# }

# lets see if we can break our function
asymmetric_toy_matrix <- matrix(1:6, nrow = 3, ncol = 2) 
asymmetric_toy_matrix

squared(asymmetric_toy_matrix)

# the error message is just telling us that we broke the laws of mathematics
 
# lets try writing our own function
# try writing a function that finds the arithmetic mean
# that is -plain old average-
# recall that you add all the numbers, and divide by how many numbers there are
# the functions sum() and length() will probably be useful here






























# I get something like this:
plain_old_average <- function(input){
    return(sum(input)/length(input))
}

some_nums <- floor(runif(10, 1, 100))
some_nums

plain_old_average(some_nums)

# just to check
mean(some_nums)

# here is more complicated example from the book
# "A Beginner's Guide to R" by Zuur, Ieno, Meesters (2009)
# the comments are mine

# first, we have to get the data
# use the getwd() command to find out where you are
getwd()
# now put the text file in that folder
vegetation <- read.table(file = "Vegetation2.txt", header = T)
str(vegetation)

# now say we want to know if there are any NA entries in this big dataset
NAinData <- function(input){
  temporary <- is.na(input)
  return(colSums(temporary))
}

# lets look at the different variables
names(vegetation)

# pick one and try to see if there are any NA's
NAinData(vegetation$SumTmax)
# track down the problem

# try it again
NAinData(vegetation[,7:8])

# and again
NAinData(vegetation)



# if your having problems running the second function, it needs an extra } at the end. 

