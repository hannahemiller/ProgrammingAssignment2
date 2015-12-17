## makeCacheMatrix creates and caches a matrix and its 
## inverse that can then be passed to cacheSolve to 
## retrieve or compute the inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmatrix <- function(solve) m <-- solve
      getmatrix <- function() m
      list(set = set, get = get, 
           setmatrix = setmatrix,
           getmatrix = getmatrix)
}


## cacheSolve calculates the inverse of a matrix created by
## makeCacheMatrix, but first checks to see if the inverse
## has been calculated previously

cacheSolve <- function(x=matrix(), ...) {
      m <- x$getmatrix()
      if(!is.null(m)) {
            message("retrieving cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(matrix, ...)
      x$setmatrix(m)
      m
}
