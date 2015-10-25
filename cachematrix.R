## These functions are a part of Programming Assignment 2
## The functions below are an exercise in Lexical Scoping
## The first function stores the matrix (and inverse matrix when it is created) and the 
## functions to pass the objects to and from the parent environment.
## The second function checks if the matrix passed to it already has had the inverse calculated.
## If the inverse has not been calculated, it then does the calculation and stores it.

## This function houses the local environment where the objects are stored/cached and the
## functions to pass the objects to and from the parent environment.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL    ## Create a blank matrix for the inverse
  set <- function(y) { ## function that caches the input matrix and clears the inverse matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x               ## function that returns the Non-Inverted matrix
  setinv <- function(inv) m <<- inv ## function that caches the inverted matrix passed to it
  getinv <- function() m            ## function that returns the inverted matrix (no-matter if it is NULL or otherwise)
  list(set = set, get = get,        ## creates a list of all the functions created so they can be easily referenced
       setinv = setinv,
       getinv = getinv)
}

## This function returns the inverse of the matrix that is entered (x)
## If the inverse had already been calculated, it retrieves it instead of recalculating

cacheSolve <- function(x, ...) { 
  m <- x$getinv()       ##retrieve the inverse of the input matrix
 
  ##If it is true that the inverse already exists, say "getting..." and return the cached inverse
  
  if(!is.null(m)) {     ##If m is NOT NULL (i.e. if it already exists...)
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()       ##Get the input matrix and store it as "data"
  m <- solve(data, ...) ##calculate the inverse matrix of "data" and store it in m
  x$setinv(m)           ##cache the newly created inverse m
  m                     ##auto-print the inverse matrix that was calculated
}
