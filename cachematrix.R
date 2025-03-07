## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function will make a cache of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                    # Initialize the inverse as NULL
  
  set <- function(y) {           # Function to set the matrix
    x <<- y
    inv <<- NULL                 # Reset the inverse when the matrix is changed
  }
  
  get <- function() x           # Function to get the matrix
  setInverse <- function(inverse) inv <<- inverse # Function to set the inverse
  getInverse <- function() inv # Function to get the inverse
  
  list(set = set, get = get,   # Return a list of the functions
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function
## This function will check if there is an existing cache for the inverse of the  matrix
## If cache exists, returns it. Else computes the inverse and stores in cache

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()             #Get cached inverse value
  if(!is.null(inv)) {               #return the existing cache value
    message("getting cached data")
    return(inv)
  }
  data <- x$get()                   #Compute the inverse if the cache doesn't exist
  inv <- solve(data, ...)           #computing the inverse using solve()
  x$setInverse(inv)                 #Cache the new inverse
  
  inv                               #Return the inverse
}
