## cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          set <- function(y) {
                  x <<- y
                  m <<- NULL
          }
          get <- function() x
          setinverse <- function(solve) m <<- solve
          getinverse <- function() m
          list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## computing the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse is already in the cache, it will be retrived.

cacheSolve <- function(x, ...) {
       m <- x$getinverse()
       if(!is.null(m)) {
                message("getting cached data")
                return(m)
       }
       data <- x$get()
       m <- solve(data, ...)
       x$setinverse(m)
       m
}
