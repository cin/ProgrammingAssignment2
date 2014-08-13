## cacheSolve and makeCacheMatrix are used to cache matrix inverses, which can be expensive calculations.
##
## For example,
##
##> foo <- matrix(data = c(1.4,3,2,-6.7), nrow = 2, ncol = 2) # creates a 2x2 matrix
##> x <- makeCacheMatrix(foo)                                 # create a cache matrix
##> cacheSolve(x)                                             # cacheSolve the cache matrix, x
##[,1]        [,2]
##[1,] 0.4356307  0.13003901
##[2,] 0.1950585 -0.09102731
##> cacheSolve(x)                                             # cacheSolve the cache matrix, x. note the "getting cached data" message.
##getting cached data
##[,1]        [,2]
##[1,] 0.4356307  0.13003901
##[2,] 0.1950585 -0.09102731
##

## makeCacheMatrix creates a special “matrix” object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
  )
}

## cacheSolve computes the inverse of the special “matrix” returned by makeCacheMatrix. If a cached inverse cannot be found,
## it calculates the inverse, saves the result in the CacheMatrix (x), and returns the result.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  } else {
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
  }
}