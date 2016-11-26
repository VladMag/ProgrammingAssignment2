## function "makeCacheMatrix" creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  x_inverse <- NULL
  
  set <- function(y) {
    x <<- y
    x_inverse <<- NULL
  }
  
  get <- function() x
  setInverse <- function(new_inverse) x_inverse <<- new_inverse
  getInverse <- function() x_inverse
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
}


## function "cacheSolve" computes the inverse of
## the "matrix" returned by "makeCacheMatrix" function above

cacheSolve <- function(x, ...) {
  
  tmp_inverse <- x$getInverse()
  if(!is.null(tmp_inverse)) {
    message("getting cached data")
    return(tmp_inverse)
  }
  
  tmp_matrix <- x$get()
  message("creating inverse")
  tmp_inverse <- solve(tmp_matrix, ...)
  x$setInverse(tmp_inverse)
  tmp_inverse
  
}
