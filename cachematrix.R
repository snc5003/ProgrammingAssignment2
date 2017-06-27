## The following functions cache the inverse of a matrix


## makeCacheMatrix creates a special matrix object that can cache
## its inverse.  


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m<<-solve
  getsolve <- function() m
  list (set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}



## cacheSolve computes the inverse of the matrix returned by
## makeCacheMatrix.  If inverse has already been calculated and
## the matrix is unchanged, then cacheSolve retrives inverse from
## the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return (m)
  }
  data<- x$get()
  m <- solve(data,...)
  x$setsolve(m)
  
  m
}


