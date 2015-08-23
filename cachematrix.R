## The first function makeCacheMatrix is used to create a list which 
## is used to set the matrix, get the matrix and
## set the value of the inverse Matrix, get the value of inverse matrix

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


## The cacheSolve function checks if the Inverse matrix exists in cache
## If exists in cache, it retrieves and prints the message else converts it into inverse


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
