## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #Creates a list of 4 functions: set and get a metrix; 
  #set and get an inverse of the matrix.
  #returns the list
  
  inv_matrix <- NULL
  setmatrix <- function(y) {
    #Update x and inv_matrix in parent env.
    #if func gets a new matrix, it invalidates the cached inverse matrix
    
    x <<- y
    inv_matrix <<- NULL
  }
  getmatrix <- function() x
  #Returns the matrix
  
  setinverse <- function(solve) inv_matrix <<- solve
  #calculates the inverse and update outside scope
  
  getinverse <- function() inv_matrix
  #returns the inversed matrix
  
  list(setmatrix = setmatrix, 
       getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv_matrix <- x$getinverse()
  
  #checking for cached matrix inverse, if inverse isn't NULL, returns it
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  
  #If inv_matrix is NULL, calculates the inverse and caches result 
  data <- x$getmatrix()
  inv_matrix <- solve(data, ...)
  x$setinverse(inv_matrix)
  inv_matrix
}

