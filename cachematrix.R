

## A function to cache the Inverse of a Matrix 

makeCacheMatrix <- function(x = matrix()) {
  
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inverse) invrs <<- inverse
  getInverse <- function() invrs
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## A fuction that will return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  invrs <- x$getInverse()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  data <- x$get()
  invrs <- solve(data, ...)
  x$setInverse(invrs)
  invrs
}

#testing the inverse of matrix

test <- makeCacheMatrix(matrix(11:14, 2, 2))
test$get()
cacheSolve(test)
test$getInverse()

