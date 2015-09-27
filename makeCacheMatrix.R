makeCacheMatrix <- function (matx = matrix()) {

# Class variables
  invmat <- NULL
  
# Class methods  
  set <- function(maty) {
    matx <<- maty
    invmat <<- NULL
  }
  
  get <- function() {
    matx
  }
  
# Specific methods to set/get class variables
  setinverse <- function(inverse) {
    invmat <<- inverse
  }
  
  getinverse <- function() {
    invmat
  }
  
# Default declaration to present all methods
  # I did not understand very well this declaration, but if I did not put it, i got an error:
  # "Error in x$getinverse : object of type 'closure' is not subsettable", which could not resolve in another way
  # Found this declaration in the example provided in the Assignment description
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)	
  
}

cacheSolve <- function(x, ...) {

  # Getting current inverse matrix state for the matrix x passed as argument
  invmat <- x$getinverse()

  # Determine if the inverse matrix has been calculated
  if(!is.null(invmat)) {
    message("Reading from cache")
    return(invmat)
  }
  
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinverse(invmat)
  
  invmat
}