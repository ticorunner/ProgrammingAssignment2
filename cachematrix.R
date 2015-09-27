## Put comments here that give an overall description of what your
## functions do

  ## Erick's Comments:
  ## makeCacheMatrix: initializes the matrix and provides methods to handle encapsulated variables (no brain here)
  ## cacheSolve:      Calculates the inverse of the matrix passed as argument.
  ##                  It determines if the inverse matrix has been already calculated, if so, it returns the
  ##                  cached inverse matrix, otherwise it calculates the inverse matrix received as argument
  ## 
  ## Class Variables description:
  ##  matx    --> matrix to be processed, argument to makeCacheMatrix and cacheSolve functions
  ##  invmat  --> inverse matrix, local variable declared in both functions
  ##
  ## Function arguments description:
  ##  maty    --> matrix argument used in the "set" method, this is to assign its value to matx
  ##  inverse --> inverse matrix argment used in "setinverse" method, this is to assign its value to invmat

## Write a short comment describing this function
## This function initializes the matrix and provides methods to handle class variables

makeCacheMatrix <- function(matx = matrix()) {
  # Class variables
  invmat <- NULL
  
  # Class "set" & "get" methods  
  set <- function(maty) {
    matx <<- maty
    invmat <<- NULL
  }
  
  get <- function() {
    matx
  }
  
  # Specific "set" & "get" methods for class variables
  setinverse <- function(inverse) {
    invmat <<- inverse
  }
  
  getinverse <- function() {
    invmat
  }
  
  # Default declaration to present all methods
  # I did not understand very well this declaration, but if I did not put it, i got an error:
  # "Error in x$getinverse : object of type 'closure' is not subsettable", which I could not resolve in any other way.
  # I found this declaration in the example provided in the Assignment description
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)	
}


## Write a short comment describing this function
## This function does:
##  Receives a matrix as argument "x"
##  Reads the current state of the inverse matrix for "x"
##  If the inverse matrix is not null, returns the current value without recalculating it
##  Otherwise, reads current data for "x", calculates its inverse matrix, sets the result to the inverse matrix of "x"
##  and returns the inverse matrix

cacheSolve <- function(x, ...) {

  # Getting current inverse matrix state for the matrix x passed as argument
  invmat <- x$getinverse()
  
  # Determine if the inverse matrix is not null (means a calculation has been processed previously)
  if(!is.null(invmat)) {
    message("Reading from cache")
    # Returns current value stored for matrix x
    return(invmat)
  }
  
  # if no inverse matrix has been calculated then gets the matrix's data and uses it to calculate & set the class variable
  # of inverse matrix for matrix x
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinverse(invmat)
  
  # returns the inverse matrix
  invmat   
}
