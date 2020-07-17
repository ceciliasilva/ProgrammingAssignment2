## PROGRAMMING ASSIGNMENT 2
## --------------------------------------------------------------------
## one function will store and the other will recover, if the inversion
## array has not yet been made, cacheSolve will perform the inversion
## and return the value
## --------------------------------------------------------------------
## this function get the matrix that is used in the argument, then 
## set the inverse, get the inverse and make a list

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y = matrix()){
  x <<- y
  i <<- NULL
  }
  get_matrix<- function() x
  set_inverse <- function(solve) i <<- solve
  get_inverse <- function() i
  list(set = set, get = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## the function below verifies if there is the inverse of the matrix
## x in the variable i, then if is not, it calculates the inverse and
## return its value

cacheSolve <- function(x, ...) {
  i <- x$get_inverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$set_inverse(i)
  i
        
}

#let's check the output

testing <- makeCacheMatrix(matrix(c(2,0,1,1), nrow = 2, ncol = 2))
testing$get()

testing$get_inverse()

# if it returns "NULL", you can try to get the data
# by using the cacheSolve function:

cacheSolve(testing)

#Now try again to use the get_inverse function
testing$get_inverse()
