## A pair of functions that cache the inverse of a matrix


makeCacheMatrix <- function( m = matrix() ) {
  
  ## Inverse property
  i <- NULL
  
  ## Setting matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Getting matrix
  get <- function() {
        m
  }
  
  ## Setting inverse
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Getting inverse of matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Getting the matrix from our object
  data <- x$get()
  
  ## Calculating the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Setting the inverse to the object
  x$setInverse(m)
  
  ## Returning the matrix
  m
}