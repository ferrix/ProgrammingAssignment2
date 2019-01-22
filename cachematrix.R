## This is practically object-oriented programming in R
# The makeCacheMatrix is an environment where two values
# and four functions operating on those values are stored.

## Matrix object storing matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    theMatrix <- x
    ## The cached inverse starts with NULL
    theInverse <- NULL

    ## The set accessor
    # When the matrix changes, the inverse is set to NULL as a side effect
    set <- function(x) {
        # Assign with <<- to define in the enclosing environment
        theMatrix <<- x
        theInverse <<- NULL
    }

    ## Get the actual matrix
    get <- function() {
        theMatrix
    }

    ## Store inverse
    setInverse <- function(y) {
        theInverse <<- y
    }

    ## Get the actual inverse
    getInverse <- function() {
        theInverse
    }

    ## Now wrap the whole obje
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute inverse of the matrix object if not found in cache

cacheSolve <- function(x, ...) {
      ## Get the inverse of 'x' from cache
      solution <- x$getInverse()

      ## If that worked return
      if (!is.null(solution)) {
          return(solution)
      }

      ## Get the inverse of matrix 'x'
      solution <- solve(x$get())

      ## Cache the inverse of matrix 'x'
      x$setInverse(solution)

      ## Return a matrix that is the inverse of 'x'
      solution
}
