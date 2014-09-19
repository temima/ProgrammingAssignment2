## this set of functions supports creation of a matrix object, calculation of the inverse matrix for this matrix and cacheing this inverse for future use

# makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# 
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse for the matrix
# get the value of the inverse for the matrix


makeCacheMatrix<- function(x=matrix(numeric())) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve recieves a square matrix object and checks whether the matrix has a chached inverse
## if an inverse is found it is returned. Otherwise the inverse is calculated, returned and saved in the makeCacheMatrix environment 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}