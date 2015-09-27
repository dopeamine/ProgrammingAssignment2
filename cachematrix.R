# Function makeCacheMatrix takes in a matrix as argument, and gets or sets the
# inverse of the matrix.Function cacheSolve is used to actually calculate the
# inverse of the matrix and also check if the inverse has already been calculated.

# create a matrix a of any size,
# create object 'make' that is an instance of makeCacheMatrix,
# pass matrix a as argument to make
# call cacheSolve with make as argument

# Function makeCacheMatrix contains function to get or set the input matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  getsolve <- function() inv
  setsolve <- function(solved) inv <<- solved
  list(set = set, get = get,getsolve = getsolve,setsolve = setsolve)
}

# Function cacheSolve takes instance of makeCacheMatrix as argument and gets value of input matrix to compute its inverse.
# If inverse of that matrix has already been calculated, the inverse is returned from cache variable inv
cacheSolve <- function(x) {
  library(MASS)
  inv <-x$getsolve()
  if(!is.null(inv)) {
    message("Getting Inverse from cache")
    return(inv)
  }
  data <- x$get()
  solved <- ginv(data)
  x$setsolve(solved)
  x$getsolve()

}
