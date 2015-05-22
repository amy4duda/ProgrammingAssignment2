## My functions will allow caching the value of the inverse of a matrix so that
## when I need it again, it can be looked up in the cache rather than recomputed

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

##Set Defaults 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
## Create a function get that returns x
  get <- function() x
## Create a function that caches the value sent to it
  setinverse <- function(inverse) m <<- inverse
## Create a function that returns the cached value
  getinverse <- function() m
## Returns the four functions that were created as a list to the function call
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the sepcial "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
##should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
##Retrieve m from makeCacheMatrix
  m <- x$getinverse()
##Tests whether m has been cached.  Return if cached else continue
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
##Set initial matrix into variable called data
  data <- x$get()
##Finds inverse of original matrix
  m <- solve(data, ...)
##call the function that caches the inverse matrix
  x$setinverse(m)
##Returns inverse matrix as output
  m
}
