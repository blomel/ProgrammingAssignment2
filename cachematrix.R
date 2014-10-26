## This file solves the 2nd programming assignment of the
## Coursera R course:
## It provides an augmented matrix object (created with
## makeCacheMatrix) that provides a caching mechanism for
## the inverse of the matrix from which it is
## contstructed.
## A helper function (cacheInverse) is provided to access
## the inverse of a matrix constructed with makeMatrix;
## only if the inverse is not yet present in the cache,
## it'll be calculated.
## ======================================================

## makeCacheMatrix
## params: x as R matrix
## return: augmented matrix object
## ======================================================
## Creates an augmented matrix object. Parameter x is
## required to be a R matrix type.
makeCacheMatrix <- function(x = matrix()) {
  
  # initialize matrix inverse
  inv <- NULL
  
  # sets the value of the augmented matrix and
  # initializes the inverse as NULL
  setVal <- function(mat) {
    x   <<- mat
    inv <<- NULL
  }
  
  # returns the value of the augmented matrix
  getVal <- function() x
  
  # caches the inverse matrix
  # reminder: solve(X) is a R function that caculates
  #           the inverse of matrix X (if non-singular)
  setCachedInv <- function(solve) {
    inv <<- solve
  }
  
  # returns the inverse matrix
  getCachedInv <- function() {
    inv
  }
  
  # this is the actual "value" of the augmented matrix,
  # i.e., a list of four "access" helpers that allow
  # to "call" internal storage/fctn facilities of the
  # augmented matrix "object"
  list(set = setVal,
       get = getVal,
       setInv = setCachedInv,
       getInv = getCachedInv)
}


## cacheSolve
## params: x as augmented matrix
## return: inverse matrix
## ======================================================
## Returns the inverse of a matrix. x has to be of
## the augmented matrix type, created as, for instance,
## x <- makeCacheMatrix(matrix(c(2,0,0,2), nrow=2))
cacheSolve <- function(x, ...) {

  # accesses the cached value of the inverse of x
  inv <- x$getInv()
  
  # if the inverse already exists in the cache,
  # return its value and end the function
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if the inverse didn't exist yet, compute it
  # using solve(), write it to the cache, and
  # return its value
  mat <- x$get()
  message("no cached data; calculating inverse")
  inv <- solve(mat,...)
  x$setInv(inv)
  inv
}
