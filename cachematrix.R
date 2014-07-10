## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
## This pair of functions are used to cache the inverse of a matrix.

## The function below creates a special "matrix" object that can cache its inverse.
## Example: my_matrix <- makeCacheMatrix(matrix(c(1, 2, 8, 9), ncol = 2))

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## The function below computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
## Example: my_matrix <- makeCacheMatrix(matrix(c(1, 2, 8, 9), ncol = 2))
##          inverse_of_my_matrix <- cacheSolve(my_matrix)

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  matrix <- x$get()
  inv <- solve(matrix)
  x$setInverse(matrix)
  
  inv
}
