
## Creates and stores an inverse cache of a special matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
        x <<- y
        inv <<- NULL
  }
        get <- function() x
        setInv <- function() inv <<- solve(x)
        getInv <- function() inv
        
        list(set = set,
        get = get,
        setInv = setInv,
        getInv = getInv)
}



## Calculates inverse of makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

      inv <- x$getInv()
      if (!is.null(inv)) {
        return(inv)
  }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInve(inv)
    inv
}