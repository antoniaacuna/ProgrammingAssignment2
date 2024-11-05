#Antonia Acuña

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #initialize the inverse as NULL
  
  #Function to set the matrix and reset the inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #Function to get the matrix
  get <- function()x
  
  #Function to set the inverse 
  setInverse <- function(inverse) inv <<- inverse
  
  #Function to get the inverse
  getInverse <- function() inv
  
  #Return a list of the above functions
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
  
}


## This function computes the inverse of the special "matrix" created by makeCacheMatrix
#If the inverse has already been calculated, it retrieves it from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse() #retrieve the inverse from cache
  
  #Check if the inverse is already calculated
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  #If not cached, compute the inverse
  mat <-x$get() #get the matrix
  inv <- solve(mat,...) #compute the inverse
  x$setInverse(inv) #cache the inverse
  
  inv
  
}
