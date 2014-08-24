## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes matrix object as an argument. It returns a list containing methods to set/get 
## the orignal matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  
  ## 
  inverse  <- NULL
  
  ## setMatrix function caches the matrix object.
  setMatrix  <- function(y){
    
    ## cache using << operator
    x <<- y
    
    ## set the inverse to NULL to void any previous cache.
    inverse <<- NULL
    
  }
  
  ## return the matrix stored in cache.
  getMatrix  <- function() x
  
  ## cache the inverse of the matrix
  setInverse  <- function(inverse) {
    inverse  <<- inverse
    
  }
  
  ## return the cached version of the inverse
  getInverse  <- function () inverse
  
  
  ## return a list of functions to set/get the matrix and it's inverse
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse =getInverse)
  
}


## cacheSolve takes a matrix object as an argument and compute's it's inverse. If the inverse
## was previosuly calculated then the function returns the cached version or else the function computes the
## inverse and saves it in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
    ## retrieve the inverse matrix
    inverse = x$getInverse()
    
    ## test whether inverse is NULL. If not null means inverse is in cache so return cached copy.
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    
    ## get the source matrix.
    data <- x$getMatrix()
    
    ## use solve function to calculate inverse
    inverse <- solve(data)
    
    ## cache the inverse 
    x$setInverse(inverse)
    
    ## return the inverse 
    inverse
    
}
