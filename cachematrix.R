##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
  m<-NULL
  setmatrix <- function(y){
    x <<- y #to cache the input matrix in x in the top environment
    m <<- NULL
  }
  getmatrix <-function()x
  setinverse <- function(solve) m <<-solve
  getinverse <- function() m
  list(setmatrix=setmatrix, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
  if(!is.null(m)){message("getting cached data")
    return(m)
  }
  #otherwise
  y <- x$getmatrix()
  x$setmatrix(y)
  m <- solve(y,...)
  x$setinverse(m)
  m
}
