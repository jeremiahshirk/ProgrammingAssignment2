## The inverse of a square matrix can be computed in R with the solve()
## function.  However, this can be an expensive operation.  The functions 
## below cache the result of previously matrix inversions to resude the cost
## of subsequent calls.

## Create a list that augments a matrix with functions to
## get and set values, as well as compute and retrieve the inverse.
## Results will be cached using the R <<- operator to assign the values in the
## calling environment.


### Example usage ###
# > m <- replicate(3, rnorm(3))
# > mm <- makeCacheMatrix(m)
# > cacheSolve(mm)
# [,1]       [,2]        [,3]
# [1,] 0.6427673 -0.6158601  0.20848716
# [2,] 0.1177942 -0.1257964  0.42627339
# [3,] 0.5597871  0.3249658 -0.02995406
# > cacheSolve(mm)
# cached
# [,1]       [,2]        [,3]
# [1,] 0.6427673 -0.6158601  0.20848716
# [2,] 0.1177942 -0.1257964  0.42627339
# [3,] 0.5597871  0.3249658 -0.02995406
# > 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(y) inv <<- y
  
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Use makeCacheMatrix above to instantiate an augmented matrix list wrapper,
## and use the associated instances functions to compute the inverse of a 
## given matrix. Report on whether the result was from the initial computation
# or retreived from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x', where x is from makeCacheMatrix
  
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("cached")
    return(inv)
  }
  else {
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
  }
  inv
}
