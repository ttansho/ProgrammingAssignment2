## Calculate the inverse of an matrix or retrieve it from the cache if it was calculated previously.

## This function caches the inverse of an matrix, assuming that the matrix supplied has its inverse.
## Input variable has to be a matrix, e.g.: makeCacheMatrix(matrix(1:4,2,2)).  Below shows the matrix in this example.
##        [,1] [,2]
##  [1,]    1    3
##  [2,]    2    4

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix or retrieve the inverse from the cache if it is in the cache.

cacheSolve <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmean(m)
    m
}

