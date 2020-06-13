## The functions mentioned below will be similar to that of the assignment.
## 

## considering the matrix is invertible. we initialize the value of the matrix

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  ## we get the value of the matrix
  get <- function() {x}
  ## setting the inverse
  setInverse <- function(inverse) {inv <<- inverse}
  ##getting the inverse of the matrix
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## the function computes inverse of the matrix

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  ##returns the matrix and assign it
  ## checking if its computed already, if so skipping  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  #computing the inverse    
  inv <- solve(mat, ...)
  ##setting the value  
  x$setInverse(inv)
  inv
}
