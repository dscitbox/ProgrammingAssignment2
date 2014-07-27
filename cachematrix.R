# The functions in this file cache a matrix
# and solves a cached matrix respectively
# in this implementation solving the matrix 
# calculates its inverse

# makeCacheMatrix caches a matrix passed in as the argument
# use this function to avoid repeating time consuming calculation
# in the function, m is the matrix, i is its inverse

makeCacheMatrix <- function(m = matrix()) {
   i <- NULL                  # the cached inverse
   set <- function(y) {
      m <<- y                 # the cached matrix
      i <<- solve(m)          # when setting a matrix, also calculate its inverse
   }
   get <- function() m        # return the cached matrix
   setinverse <- function(inverse) i <<- inverse
   getinverse <- function() i # return the cached inverse
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

# returns the inverse of x if it exists, otherwise it calculates and caches it
cacheSolve <- function(x, ...) {
   # check if the inverse is cached 
   i <- x$getinverse()
   if(!is.null(i)){
      message("getting cached data")
      return(i)
   }
   data <- x$get()
   i <- solve(data, ...)
   x$setinverse(i)
   i
}

