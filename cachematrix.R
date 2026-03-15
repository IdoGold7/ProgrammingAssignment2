## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  setmatrix <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(solve) inv_matrix <<- solve
  getinverse <- function() inv_matrix
  list(setmatrix = setmatrix, 
       getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv_matrix <- x$getinverse()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  data <- x$getmatrix()
  inv_matrix <- solve(data, ...)
  x$setinvers(inv_matrix)
  inv_matrix
}

