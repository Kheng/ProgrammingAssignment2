
## -------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
  ## set the value of the matrix
  d <- NULL
  set <- function(y) {
    x <<- y
    dat <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  
  ## set the inverse of the matrix
  setinverse <- function(inverse) d <<- inverse
  getinverse <- function() d
  
  ## get the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## -------------------------------------------------

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## get the inverse of the matrix        
  d <- x$getinverse()
  
  ## check if there is the matrix   
  if(!is.null(d)) {
    message("getting cached data")
    return(d)
  }
  ## if not: get the inverse of the matrix   
  data <- x$get()
  d <- solve(data)
  ## set the inverse of the matrix 
  x$setinverse(d)
  
  d
  
}
