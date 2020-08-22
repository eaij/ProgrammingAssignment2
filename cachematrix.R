## The functions will cache the inverse of a matrix. 

##This will creates a matrix, which is actually a list. 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ##Setting the values of the matrix below
  set <- function(y) {
      x <<- y
      inv <<- NULL
  } 
  ## Now get the value of the matrix
    get <- function () x
    ## Set the value of the inverse
    setinv <- function(inverse) inv <<- inverse
    ## Get the value of the inverse
    getinv <- function() inv
    ## Creating a list now 
    list (set=set, get=get, 
          setinv = setinv, 
          getinv = getinv)
}

## Creating a matrix that's inverse of the x now. 

cacheSolve <- function(x, ...) {
 ## Assigns the inverse x matrix to inv:
  inv <- x$getinv()
  ## Check if inverse is already calculated. If so, get inverse from cache (inv) 
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    ## If not, create inverse of matrix. Need to set value of the inverse in cache
  mat <- x$get()
  ## This is cachesolve, so we need solve
  inv <- solve(mat,...)
  ## Use setinv to set the value of the inversion in the cache
  x$setinv(inv)
  inv
}
