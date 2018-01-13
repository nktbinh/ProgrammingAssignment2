## Below are the two functions that cache the inverse of a matrix



## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initialize the inverse to NULL
  i <- NULL
  
  ## Assign contents of input to cached Matrix and NULL the inverse
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Function the get the matrix
  get <- function() m
  
  ## Function to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Function to get the inverse of the matrix
  getInverse <- function() i
  
  ## Create list with methods for get / set of both original matrix
  ## and its inverse, return the list to parent environment.  
  ## This technique allows use of $ operator to access
  ## each function from the list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function computes the inverse of the special matrix returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Attempt to retrieve matrix from cache
  theMatrix <- x$getInverse()
  if( !is.null(theMatrix) ) {
    message("getting cached inverse")
    return(theMatrix)
  }
  ## If we get past the if() statement, the cache is empty
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse
  theMatrix <- solve(data)
  
  ## Set the inverse to the object
  x$setInverse(theMatrix)
  
  ## Return the matrix
  theMatrix
}
