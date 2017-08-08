
## The below piece of code is essentially the one given in the Assignment's example
## with the replacement of the "mean" function with the "solve" function.

## There is a small example commented out at the end of the script.


## The description of the function is shown in steps within the code below.

makeCacheMatrix <- function(x = matrix()) { # Initializing x variable within the function
  ans <- NULL # Initializing ans variable within the function
  set <- function(y) {
    x <<- y # assigning the input argument to x in the parent environment
    ans <<- NULL # clearing any ans values whenever x is reset 
  }
  get <- function() x # retrieving x from parent environment
  setinverse <- function(solve) ans <<- solve # solve is the function that calculates the inverse of a matrix
  getinverse <- function() ans # assigning the input argument to the value of ans in the parent environment
  list(set = set, get = get, # naming list elements
       setinverse = setinverse,
       getinverse = getinverse)
}



cacheSolve <- function(x, ...) { # object of type "makeCacheMatrix" gets transferred into the cacheSolve function
  ans <- x$getinverse() # retrieving the inverse of the matrix and setting it to ans
  if(!is.null(ans)) { # checking if ans is not NULL and therefore already existing in the cache memory 
    message("getting cached data") # making visible in the console that the value was stored in cache
    return(ans) 
  }
  data <- x$get() # if ans was NULL then the value was not stored in cache and therefore the matrix is recalled from the input object in order for the inverse to be calculated
  ans <- solve(data, ...) # the iverse is calculated anew via the solve function and replaces the previous NULL value of ans 
  x$setinverse(ans) # setting the above calculated inverse in the input object
  ans
}

# 
# # Testing the functions, assuming invertible 2x2 matrix
# 
# example_matrix <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
# cached_example_matrix <- makeCacheMatrix(example_matrix)
# cacheSolve(cached_example_matrix)
# 
# # Trying to get the original back
#
# inverted_example_matrix <- cacheSolve(cached_example_matrix)
# cached_example_matrix2 <- makeCacheMatrix(inverted_example_matrix)
# cacheSolve(cached_example_matrix2)