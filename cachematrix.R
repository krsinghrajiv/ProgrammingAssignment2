## Put comments here that give an overall description of what your
## functions do

## The first function makeCacheMatrix is used to create a list which 
## is used to set the matrix, get the matrix
## set the value of the inverse Matrix, get the value of inverse matrix
##

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
      x <<- y
      inv <<- NULL
      
  }
  
  get <- function ()x
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(   # creating the list
      set = set,
      get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}


## The cacheSolve function creates the Cache after verifying if
## already a cache exists or not. If a cache exists, a message is displayed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if(!is.null(inv)){
      message("Getting from Cached data")
      return(inv)
  }
  checkVal <- x$get()
  inv <- solve(checkVal,...)
  x$setinverse(inv)
  inv
}
makeCacheMatrix ()
