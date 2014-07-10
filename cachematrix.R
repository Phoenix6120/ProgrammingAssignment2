## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
## This pair of functions are used to cache the inverse of a matrix.

## The function below creates a special "matrix" object that can cache its inverse.
## Example: my_matrix <- makeCacheMatrix(matrix(c(1, 2, 8, 9), ncol = 2))

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## 'set()' is used to change the matrix itself
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## 'get()' is a function that returns the matrix itself
  get <- function() x
  
  ## 'setInverse()' is a function that stores the inverse of this matrix in the variable 'inv'
  setInverse <- function(inverse) inv <<- inverse
  
  ## 'getInverse()' can directly returns the inverse stored in 'inv' so that the computational cost is saved
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
  
  ## If the inverse has already been calculated, then retrieve inverse from the cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ##  If the inverse has not yet been calculated, then calculate it and store it in the cache
  matrix <- x$get()
  inv <- solve(matrix)
  x$setInverse(matrix)
  inv
  
}
