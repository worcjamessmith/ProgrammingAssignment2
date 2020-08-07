## These functions calculate the inverse of a matrix and cache the resulting
## matrix. If the same matrix passed as an input then the cached matrix is
## retrieved, rather than it being recalculated.


## makeCacheMatrix creates the getters and setters (mutators and accessors)
## which will be passed as output to the object along with x and m
makeCacheMatrix <- function(x = matrix()) {
  #intitialisation of two objects, x and m 
  m <- NULL
  # define function for setting the matrix to solve
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # get x, either from the input to make.Vector or from the makeVector argument
  get <- function() x     
  setmatrix <- function(inverse) m <<- inverse 
  getmatrix <- function() m
  # return list of functions 
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## cacheSolve calculates the inverse of a matrix, or if the inverse of that
## matrix has already been calculated and is cached, get the cached data
cacheSolve <- function(x, ...) {
  # call getmatrix to get m
  m <- x$getmatrix()
  # if m isn't null get the cached matrix and return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # otherwise solve the matrix and return the result, storing it in makeCacheMatrix
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}     
  







